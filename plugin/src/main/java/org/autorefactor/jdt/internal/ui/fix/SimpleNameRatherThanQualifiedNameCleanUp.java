/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Handle local variable and outer classes
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.jdt.internal.ui.fix;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.CollectorVisitor;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.TypeNameMatch;
import org.eclipse.jdt.core.search.TypeNameMatchRequestor;

/** See {@link #getDescription()} method. */
public class SimpleNameRatherThanQualifiedNameCleanUp extends AbstractCleanUpRule {
	private static final class QName {
		private final QName qualifier;
		private final String simpleName;

		private static QName valueOf(final String fullyQualifiedName) {
			QName qname= null;
			for (String name : fullyQualifiedName.split("\\.")) { //$NON-NLS-1$
				qname= new QName(qname, name);
			}

			return qname;
		}

		private static QName valueOf(final String qualifiedName, final String simpleName) {
			return new QName(valueOf(qualifiedName), simpleName);
		}

		private QName(final QName qualifier, final String simpleName) {
			this.qualifier= qualifier;
			this.simpleName= simpleName;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			QName other= (QName) obj;
			return Objects.equals(simpleName, other.simpleName) && Objects.equals(qualifier, other.qualifier);
		}

		@Override
		public int hashCode() {
			return (simpleName != null ? simpleName.hashCode() : 0) * 1000
					+ (qualifier != null ? qualifier.hashCode() : 0);
		}

		@Override
		public String toString() {
			return qualifier != null ? qualifier + "." + simpleName : simpleName; //$NON-NLS-1$
		}
	}

	private enum FqnType {
		METHOD, FIELD, TYPE
	}

	/** Information about fully-qualified names. */
	private static final class FQN {
		private static final FQN CANNOT_REPLACE_SIMPLE_NAME= new FQN(null, false, false);
		private final QName fullyQualifiedName;
		private final boolean fromImport;
		/**
		 * Whether an import was a star import (static or not).
		 * <p>
		 * Only applicable when <code>{@link #fromImport} == true</code>.
		 */
		private final boolean onDemand;

		public FQN(final QName fullyQualifiedName, final boolean fromImport, final boolean onDemand) {
			this.fullyQualifiedName= fullyQualifiedName;
			this.fromImport= fromImport;
			this.onDemand= onDemand;
		}

		private static FQN fromImport(final QName fullyQualifiedName, final boolean onDemand) {
			return new FQN(fullyQualifiedName, true, onDemand);
		}

		private static FQN fromMember(final QName fullyQualifiedName) {
			return new FQN(fullyQualifiedName, false, false /* meaningless */);
		}

		public boolean isMember() {
			return !fromImport;
		}

		public boolean fromImport() {
			return fromImport;
		}

		public boolean fromStarImport() {
			return fromImport && onDemand;
		}

		public boolean fromSpecificImport() {
			return fromImport && !onDemand;
		}

		@Override
		public String toString() {
			if (equals(CANNOT_REPLACE_SIMPLE_NAME)) {
				return "CANNOT_REPLACE_SIMPLE_NAME"; //$NON-NLS-1$
			}

			return fullyQualifiedName + (fromImport ? " (imported)" : " (member)"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	/** Maps simple names to their fully qualified names. */
	private static final class Names {
		/**
		 * Simple names for java elements in use in this compilation unit. It merges
		 * imports and local declarations.
		 */
		private final Map<String, List<FQN>> simpleNames= new TreeMap<>();

		private void addName(final FQN fqn) {
			addName(fqn.fullyQualifiedName.simpleName, fqn);
		}

		private void cannotReplaceSimpleName(final String simpleName) {
			addName(simpleName, FQN.CANNOT_REPLACE_SIMPLE_NAME);
		}

		private void addName(final String simpleName, final FQN fqn) {
			List<FQN> existingFqns= simpleNames.get(simpleName);
			if (existingFqns == null) {
				existingFqns= new ArrayList<>();
				simpleNames.put(simpleName, existingFqns);
			}
			existingFqns.add(fqn);
		}

		private boolean canReplaceFqnWithSimpleName(final ASTNode node, final QName fullyQualifiedName, final FqnType fqnType) {
			String simpleName= fullyQualifiedName.simpleName;
			List<FQN> matches= getBestMatches(simpleName);
			switch (matches.size()) {
			case 0:
				return false;

			case 1:
				if (!matches.get(0).isMember()) {
					return fullyQualifiedName.equals(matches.get(0).fullyQualifiedName);
				}
				//$FALL-THROUGH$

			default:
				ITypeBinding enclosingTypeBinding= resolveEnclosingTypeBinding(ASTNodes.getEnclosingType(node));
				if (enclosingTypeBinding != null && matches.get(0).isMember()) {
					// All matches are local to this class
					ITypeBinding declaringType= getDeclaringTypeInTypeHierarchy(enclosingTypeBinding, simpleName,
							fqnType, node);
					if (declaringType != null) {
						return fullyQualifiedName.equals(QName.valueOf(declaringType.getQualifiedName(), simpleName));
					}

					// All matches are local to this class
					declaringType= getDeclaringTypeInTopLevelHierarchy(simpleName, fqnType, node);
					if (declaringType != null) {
						return fullyQualifiedName.equals(QName.valueOf(declaringType.getQualifiedName(), simpleName));
					}

					// Whether we can replace a simple name highly depends on the current context
					// are we in an inner class, is this from the enclosing class, etc.
					QName enclosingTypeQName= QName.valueOf(enclosingTypeBinding.getQualifiedName());
					return existsInAnyEnclosingType(matches, fullyQualifiedName, enclosingTypeQName);
				}

				return false;
			}
		}

		private boolean existsInAnyEnclosingType(final List<FQN> matches, final QName fullyQualifiedName,
				QName enclosingTypeQName) {
			while (enclosingTypeQName != null) {
				if (existsInEnclosingType(matches, fullyQualifiedName, enclosingTypeQName)) {
					return true;
				}
				enclosingTypeQName= enclosingTypeQName.qualifier;
			}

			return false;
		}

		private boolean existsInEnclosingType(final List<FQN> matches, final QName fullyQualifiedName, final QName enclosingTypeQName) {
			QName enclosingQName= new QName(enclosingTypeQName, fullyQualifiedName.simpleName);

			if (fullyQualifiedName.equals(enclosingQName)) {
				for (FQN fqn : matches) {
					if (fqn.fullyQualifiedName.equals(fullyQualifiedName)) {
						return true;
					}
				}
			}

			return false;
		}

		private ITypeBinding getDeclaringTypeInTypeHierarchy(final ITypeBinding typeBinding, final String simpleName,
				final FqnType fqnType, final ASTNode node) {
			ITypeBinding superTypeBinding= typeBinding;
			do {
				for (IBinding binding : getDeclaredBinding(superTypeBinding, fqnType, node)) {
					if (binding.getName().equals(simpleName) && (Modifier.isPublic(binding.getModifiers())
							|| Modifier.isProtected(binding.getModifiers()) || !Modifier.isPrivate(binding.getModifiers())
									&& superTypeBinding.getPackage().equals(typeBinding.getPackage()))) {
						return superTypeBinding;
					}
				}
				superTypeBinding= superTypeBinding.getSuperclass();
			} while (superTypeBinding != null);
			return null;
		}

		private ITypeBinding getDeclaringTypeInTopLevelHierarchy(final String simpleName, final FqnType fqnType,
				final ASTNode node) {
			Class<?>[] ancestorClasses= { AbstractTypeDeclaration.class, AnonymousClassDeclaration.class };
			ASTNode enclosingType= ASTNodes.getFirstAncestorOrNull(node, ancestorClasses);

			while (enclosingType != null) {
				ITypeBinding enclosingTypeBinding= resolveEnclosingTypeBinding(enclosingType);

				for (IBinding binding : getDeclaredBinding(enclosingTypeBinding, fqnType, node)) {
					if (binding.getName().equals(simpleName)) {
						return enclosingTypeBinding;
					}
				}
				enclosingType= ASTNodes.getFirstAncestorOrNull(enclosingType, ancestorClasses);
			}

			return null;
		}

		private IBinding[] getDeclaredBinding(final ITypeBinding typeBinding, final FqnType fqnType, final ASTNode node) {
			switch (fqnType) {
			case METHOD:
				return typeBinding.getDeclaredMethods();

			case FIELD:
				return typeBinding.getDeclaredFields();

			case TYPE:
				return typeBinding.getDeclaredTypes();

			default:
				throw new NotImplementedException(node, fqnType);
			}
		}

		private List<FQN> getBestMatches(final String simpleName) {
			List<FQN> fqns= simpleNames.get(simpleName);
			if (fqns == null) {
				return Collections.emptyList();
			}
			List<FQN> bestMatches= new ArrayList<>();
			for (FQN fqn : fqns) {
				if (fqn.equals(FQN.CANNOT_REPLACE_SIMPLE_NAME)) {
					// Something got wrong while computing the FQNs => bail out
					return Collections.emptyList();
				}
				if (bestMatches.isEmpty()) {
					// We now have a best match
					bestMatches.add(fqn);
				} else if (bestMatches.get(0).fromImport()) {
					if (fqn.isMember()) {
						// Local FQNs take precedence over imported FQNs
						bestMatches.clear();
						bestMatches.add(fqn);
					} else // FQN is imported
						if (bestMatches.get(0).fromStarImport()) {
							if (fqn.fromSpecificImport()) {
								// Specific imports take precedence over on-demand imports
								bestMatches.clear();
							}
							bestMatches.add(fqn);
						} else if (fqn.fromSpecificImport()) {
							// Import conflicts:
							// there is more than one specific import.
							// this is a compile-error: do not try to replace
							return Collections.emptyList();
						}
				} else if (fqn.isMember()) {
					// Group local FQNs together
					bestMatches.add(fqn);
				}
			}

			return bestMatches;
		}

		private ITypeBinding resolveEnclosingTypeBinding(final ASTNode node) {
			if (node != null) {
				switch (node.getNodeType()) {
				case ASTNode.ANONYMOUS_CLASS_DECLARATION:
					return ((AnonymousClassDeclaration) node).resolveBinding();
				case ASTNode.ENUM_DECLARATION:
					return ((EnumDeclaration) node).resolveBinding();
				case ASTNode.TYPE_DECLARATION:
					return ((TypeDeclaration) node).resolveBinding();
				default:
					throw new NotImplementedException(node, node);
				}
			}

			return null;
		}

		private void clear() {
			simpleNames.clear();
		}

		@Override
		public String toString() {
			return getClass().getSimpleName() + "(simpleNames=" + simpleNames + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private final Names types= new Names();
	private final Names methods= new Names();
	private final Names fields= new Names();

	private void resetAllNames() {
		types.clear();
		methods.clear();
		fields.clear();
	}

	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_SimpleNameRatherThanQualifiedNameCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_SimpleNameRatherThanQualifiedNameCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_SimpleNameRatherThanQualifiedNameCleanUp_reason;
	}

	@Override
	public boolean visit(final CompilationUnit node) {
		resetAllNames();
		return super.visit(node);
	}

	private void readImport(final ImportDeclaration node) {
		QName qname= QName.valueOf(node.getName().getFullyQualifiedName());
		if (node.isStatic()) {
			if (node.isOnDemand()) {
				importStaticTypesAndMembersFromType(node);
			} else {
				importStaticTypeOrMember(node, qname);
			}
		} else if (node.isOnDemand()) {
			String pkgName= node.getName().getFullyQualifiedName();
			importTypesFromPackage(pkgName, node);
		} else {
			types.addName(FQN.fromImport(qname, false));
		}
	}

	private void importStaticTypesAndMembersFromType(final ImportDeclaration node) {
		IBinding binding= node.resolveBinding();
		if (binding == null) {
			return;
		}
		if (binding.getKind() != IBinding.TYPE) {
			throw new NotImplementedException(node, "for a binding of kind " + binding.getKind()); //$NON-NLS-1$
		}
		ITypeBinding typeBinding= (ITypeBinding) binding;
		String typeName= typeBinding.getQualifiedName();
		for (IMethodBinding method : typeBinding.getDeclaredMethods()) {
			if (canAdd(method.getModifiers(), method.isSynthetic())) {
				QName qname= QName.valueOf(typeName, method.getName());
				methods.addName(FQN.fromImport(qname, true));
			}
		}
		for (IVariableBinding field : typeBinding.getDeclaredFields()) {
			if (canAdd(field.getModifiers(), field.isSynthetic())) {
				QName qname= QName.valueOf(typeName, field.getName());
				fields.addName(FQN.fromImport(qname, true));
			}
		}
		for (ITypeBinding memberType : typeBinding.getDeclaredTypes()) {
			if (canAdd(memberType.getModifiers(), memberType.isSynthetic())) {
				QName qname= QName.valueOf(memberType.getQualifiedName());
				types.addName(FQN.fromImport(qname, true));
			}
		}
	}

	private boolean canAdd(final int modifiers, final boolean isSynthetic) {
		return Modifier.isStatic(modifiers) && !Modifier.isPrivate(modifiers) && !isSynthetic;
	}

	private void importStaticTypeOrMember(final ImportDeclaration node, final QName fullyQualifiedName) {
		IBinding binding= node.resolveBinding();

		if (binding == null) {
			return;
		}

		switch (binding.getKind()) {
		case IBinding.METHOD:
			methods.addName(FQN.fromImport(fullyQualifiedName, false));
			break;

		case IBinding.VARIABLE:
			fields.addName(FQN.fromImport(fullyQualifiedName, false));
			break;

		case IBinding.TYPE:
			types.addName(FQN.fromImport(fullyQualifiedName, false));
			break;
		}
	}

	private void importTypesFromPackage(final String pkgName, final ASTNode node) {
		TypeNameMatchRequestor importTypeCollector= new TypeNameMatchRequestor() {
			@Override
			public void acceptTypeNameMatch(final TypeNameMatch typeNameMatch) {
				boolean isTopLevelType= typeNameMatch.getType().getDeclaringType() == null;
				if (isTopLevelType) {
					if (!pkgName.equals(typeNameMatch.getPackageName())) {
						// Sanity check failed
						throw new IllegalStateException("Expected package '" + typeNameMatch.getPackageName() //$NON-NLS-1$
								+ "' to be equal to '" + pkgName + "'"); //$NON-NLS-1$ //$NON-NLS-2$
					}
					QName qname= QName.valueOf(typeNameMatch.getFullyQualifiedName());
					types.addName(FQN.fromImport(qname, true));
				}
			}
		};

		try {
			SearchEngine searchEngine= new SearchEngine();
			searchEngine.searchAllTypeNames(pkgName.toCharArray(), SearchPattern.R_EXACT_MATCH, // search in this package
					null, SearchPattern.R_EXACT_MATCH, // do not filter by type name
					IBinding.TYPE, // look for all java types (class, interfaces, enums, etc.)
					SearchEngine.createWorkspaceScope(), // search everywhere
					importTypeCollector, IJavaSearchConstants.WAIT_UNTIL_READY_TO_SEARCH, // wait in case the indexer is indexing
					cuRewrite.getProgressMonitor());
		} catch (JavaModelException e) {
			throw new UnhandledException(node, e);
		}
	}

	@Override
	public boolean visit(final TypeDeclaration node) {
		ITypeBinding typeBinding= node.resolveBinding();
		if (typeBinding != null && !typeBinding.isNested() && node.getParent() instanceof CompilationUnit) {
			CompilationUnit compilationUnit= (CompilationUnit) node.getParent();
			for (ImportDeclaration importDecl : ASTNodes.imports(compilationUnit)) {
				readImport(importDecl);
			}
			importTypesFromPackage("java.lang", compilationUnit); //$NON-NLS-1$

			node.accept(new NamesCollector());
		}

		return true;
	}

	@Override
	public void endVisit(final TypeDeclaration node) {
		ITypeBinding typeBinding= node.resolveBinding();
		if (typeBinding != null && !typeBinding.isNested()) {
			resetAllNames();
		}
	}

	private final class NamesCollector extends ASTVisitor {
		@Override
		public boolean visit(final TypeDeclaration node) {
			return addTypeNames(node);
		}

		@Override
		public boolean visit(final EnumDeclaration node) {
			return addTypeNames(node);
		}

		private boolean addTypeNames(final AbstractTypeDeclaration node) {
			ITypeBinding typeBinding= node.resolveBinding();
			if (typeBinding != null) {
				types.addName(FQN.fromMember(QName.valueOf(typeBinding.getQualifiedName())));
			} else {
				// We cannot determine the FQN, so we cannot safely replace it
				types.cannotReplaceSimpleName(node.getName().getIdentifier());
			}

			return true;
		}

		@Override
		public boolean visit(final MethodDeclaration node) {
			String simpleName= node.getName().getIdentifier();
			IMethodBinding methodBinding= node.resolveBinding();
			if (methodBinding != null) {
				QName qname= QName.valueOf(methodBinding.getDeclaringClass().getQualifiedName(), simpleName);
				methods.addName(FQN.fromMember(qname));
			} else {
				// We cannot determine the FQN, so we cannot safely replace it
				methods.cannotReplaceSimpleName(simpleName);
			}

			return true;
		}

		@Override
		public boolean visit(final FieldDeclaration node) {
			for (VariableDeclarationFragment vdf : ASTNodes.fragments(node)) {
				String simpleName= vdf.getName().getIdentifier();
				IVariableBinding variableBinding= vdf.resolveBinding();
				if (variableBinding != null) {
					fields.addName(FQN.fromMember(
							QName.valueOf(variableBinding.getDeclaringClass().getQualifiedName(), simpleName)));
				} else {
					// We cannot determine the FQN, so we cannot safely replace it
					fields.cannotReplaceSimpleName(simpleName);
				}
			}

			return true;
		}
	}

	private QName getFullyQualifiedNameOrNull(final QualifiedName node) {
		IBinding binding= node.resolveBinding();

		if (binding != null) {
			switch (binding.getKind()) {
			case IBinding.TYPE:
				ITypeBinding typeBinding= (ITypeBinding) binding;
				return QName.valueOf(typeBinding.getErasure().getQualifiedName());

			case IBinding.VARIABLE:
				IVariableBinding fieldBinding= (IVariableBinding) binding;
				if (hasKind(node.getQualifier(), IBinding.TYPE)) {
					return QName.valueOf(fieldBinding.getDeclaringClass().getQualifiedName(), fieldBinding.getName());
				} // else this is a field access like other.fieldName
			}
		}

		return null;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		Expression expression= node.getExpression();
		IMethodBinding methodBinding= node.resolveMethodBinding();

		if (methodBinding != null && expression instanceof Name && hasKind((Name) expression, IBinding.TYPE)
				&& node.typeArguments().isEmpty()) {
			ITypeBinding declaringClass= methodBinding.getDeclaringClass();
			QName qname= QName.valueOf(declaringClass.getErasure().getQualifiedName(), methodBinding.getName());
			if (methods.canReplaceFqnWithSimpleName(node, qname, FqnType.METHOD)) {
				cuRewrite.getASTRewrite().remove(expression, null);
				return false;
			}
		}

		return true;
	}

	private boolean hasKind(final Name name, final int bindingKind) {
		IBinding binding= name.resolveBinding();
		return binding != null && binding.getKind() == bindingKind;
	}

	@Override
	public boolean visit(final FieldDeclaration node) {
		return maybeReplaceFqnsWithSimpleNames(node);
	}

	@Override
	public boolean visit(final Initializer node) {
		Set<String> localVars= ASTNodes.getLocalVariableIdentifiers(node.getBody(), true);
		return maybeReplaceFqnsWithSimpleNames(node.getBody(), localVars);
	}

	@Override
	public boolean visit(final MethodDeclaration node) {
		// Method parameters
		for (SingleVariableDeclaration parameter : ASTNodes.parameters(node)) {
			if (!maybeReplaceFqnsWithSimpleNames(parameter)) {
				return false;
			}
		}

		// Method return value
		if (!maybeReplaceFqnsWithSimpleNames(node.getReturnType2())) {
			return false;
		}

		// Method body
		Set<String> localIdentifiers= new HashSet<>();
		for (SingleVariableDeclaration localParameter : ASTNodes.parameters(node)) {
			localIdentifiers.add(localParameter.getName().getIdentifier());
		}
		localIdentifiers.addAll(ASTNodes.getLocalVariableIdentifiers(node.getBody(), true));

		return maybeReplaceFqnsWithSimpleNames(node.getBody(), localIdentifiers);
	}

	private boolean maybeReplaceFqnsWithSimpleNames(final ASTNode node) {
		return maybeReplaceFqnsWithSimpleNames(node, Collections.<String>emptySet());
	}

	private boolean maybeReplaceFqnsWithSimpleNames(final ASTNode node, final Set<String> localIdentifiers) {
		if (node != null) {
			Iterable<QualifiedName> qualifiedNames= new QualifiedNamesCollector().collect(node);
			for (QualifiedName qualifiedName : qualifiedNames) {
				if (!maybeReplaceFqnWithSimpleName(qualifiedName, localIdentifiers)) {
					return false;
				}
			}
		}

		return true;
	}

	private static final class QualifiedNamesCollector extends CollectorVisitor<QualifiedName> {
		@Override
		public boolean visit(final QualifiedName node) {
			addResult(node);
			return true;
		}
	}

	private boolean maybeReplaceFqnWithSimpleName(final QualifiedName node, final Set<String> localIdentifiers) {
		ASTNode ancestor= ASTNodes.getFirstAncestorOrNull(node, PackageDeclaration.class, ImportDeclaration.class);
		QName qname= getFullyQualifiedNameOrNull(node);
		if (ancestor != null || qname == null) {
			return true;
		}
		if (types.canReplaceFqnWithSimpleName(node, qname, FqnType.TYPE)
				|| fields.canReplaceFqnWithSimpleName(node, qname, FqnType.FIELD)
						&& !localIdentifiers.contains(qname.simpleName)) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			rewrite.replace(node, rewrite.createMoveTarget(node.getName()), null);
			return false;
		}

		return true;
	}
}
