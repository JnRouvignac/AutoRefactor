/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.CollectorVisitor;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.util.IllegalStateException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class EntrySetRatherThanKeySetAndValueSearchCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.EntrySetRatherThanKeySetAndValueSearchCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.EntrySetRatherThanKeySetAndValueSearchCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.EntrySetRatherThanKeySetAndValueSearchCleanUp_reason;
	}

	/**
	 * This class helps decide which name to give to a new variable.
	 * <p>
	 * When creating a new variable, its name may shadow another variable or field
	 * used in the local scope.
	 * <p>
	 * Does JDT provide a public API for naming local variables? I could not find
	 * any.
	 */
	private static final class VariableNameDecider {
		private final ASTNode scope;
		private final int insertionPoint;
		private final ASTNode namingScope;

		private VariableNameDecider(final ASTNode scope, final int insertionPoint) {
			this.scope= scope;
			this.insertionPoint= insertionPoint;
			this.namingScope= getNamingScope(scope);
		}

		private ASTNode getNamingScope(final ASTNode scope) {
			Class<? extends ASTNode>[] ancestorClasses= new Class[] { MethodDeclaration.class, Initializer.class };
			ASTNode ancestor= ASTNodes.getFirstAncestorOrNull(scope, ancestorClasses);
			if (ancestor == null) {
				throw new IllegalStateException(scope, "Expected to find an ancestor among the types " //$NON-NLS-1$
						+ Arrays.toString(ancestorClasses) + " but could not find any"); //$NON-NLS-1$
			}

			return ancestor;
		}

		/**
		 * Returns a name suggestion suitable for use when inserting a new variable
		 * declaration. This name:
		 * <ul>
		 * <li>will not shadow any variable name in use after the insertion point</li>
		 * <li>and will not conflict with local variables declared before the insertion
		 * point.</li>
		 * </ul>
		 *
		 * @param candidateNames the suggestion will be one of the candidate names,
		 *                       maybe suffixed by a number
		 * @return the suggestion for a variable name
		 */
		public String suggest(final String... candidateNames) {
			Set<String> declaredLocalVarNames= new HashSet<>(collectDeclaredLocalVariableNames());
			Set<String> varNamesUsedAfter= new HashSet<>(collectVariableNamesUsedAfter());
			// Can we use one of the candidate names?
			for (String candidate : candidateNames) {
				if (isSuitable(candidate, declaredLocalVarNames, varNamesUsedAfter)) {
					return candidate;
				}
			}

			// Iterate on the first candidate name and suffix it with an integer
			int i= 1;
			do {
				String candidate= candidateNames[0] + i;
				if (isSuitable(candidate, declaredLocalVarNames, varNamesUsedAfter)) {
					return candidate;
				}
				i++;
			} while (true);
		}

		private boolean isSuitable(final String candidateName, final Set<String> declaredLocalVarNames,
				final Set<String> varNamesUsedAfter) {
			// No variable declaration conflict
			return !declaredLocalVarNames.contains(candidateName)
					// New variable does not shadow use of other variables/fields with the same name
					&& !varNamesUsedAfter.contains(candidateName);
		}

		private Collection<String> collectDeclaredLocalVariableNames() {
			return new CollectorVisitor<String>() {
				@Override
				public boolean preVisit2(final ASTNode visited) {
					return !isTypeDeclaration(visited);
				}

				@Override
				public boolean visit(final SimpleName visited) {
					IBinding binding= visited.resolveBinding();
					if (binding != null && binding.getKind() == IBinding.VARIABLE) {
						addResult(binding.getName());
					}

					return true;
				}
			}.collect(namingScope);
		}

		private List<String> collectVariableNamesUsedAfter() {
			return new CollectorVisitor<String>() {
				@Override
				public boolean preVisit2(final ASTNode visited) {
					return visited.getStartPosition() > insertionPoint && !isTypeDeclaration(visited);
				}

				@Override
				public boolean visit(final SimpleName visited) {
					IBinding binding= visited.resolveBinding();
					if (binding != null && binding.getKind() == IBinding.VARIABLE) {
						addResult(binding.getName());
					}

					return true;
				}
			}.collect(scope);
		}

		private boolean isTypeDeclaration(final ASTNode node) {
			switch (node.getNodeType()) {
			case ASTNode.ANNOTATION_TYPE_DECLARATION:
			case ASTNode.ANONYMOUS_CLASS_DECLARATION:
			case ASTNode.ENUM_DECLARATION:
			case ASTNode.TYPE_DECLARATION:
				return true;

			default:
				return false;
			}
		}
	}

	@Override
	public boolean visit(final EnhancedForStatement enhancedFor) {
		MethodInvocation foreachExpression= ASTNodes.as(enhancedFor.getExpression(), MethodInvocation.class);

		if (isKeySetMethod(foreachExpression)) {
			// From 'for (K key : map.keySet()) { }'
			// -> mapExpression become 'map', parameter become 'K key'
			Expression mapExpression= foreachExpression.getExpression();
			if (mapExpression == null) {
				// Not implemented
				return true;
			}
			SingleVariableDeclaration parameter= enhancedFor.getParameter();
			List<MethodInvocation> getValueMis= collectMapGetValueCalls(mapExpression, parameter,
					enhancedFor.getBody());
			if (!getValueMis.isEmpty() && haveSameTypeBindings(getValueMis)) {
				replaceEntryIterationByKeyIteration(enhancedFor, mapExpression, parameter, getValueMis);
				return false;
			}
		}

		return true;
	}

	private void replaceEntryIterationByKeyIteration(final EnhancedForStatement enhancedFor, final Expression mapExpression,
			final SingleVariableDeclaration parameter, final List<MethodInvocation> getValueMis) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.EntrySetRatherThanKeySetAndValueSearchCleanUp_description);

		VarDefinitionsUsesVisitor keyUseVisitor= new VarDefinitionsUsesVisitor(parameter.resolveBinding(), enhancedFor.getBody(), true);
		int keyUses= keyUseVisitor.getReads().size();

		int insertionPoint= ASTNodes.asList(enhancedFor.getBody()).get(0).getStartPosition() - 1;
		String entryVar= new VariableNameDecider(enhancedFor.getBody(), insertionPoint).suggest("entry", "mapEntry"); //$NON-NLS-1$ //$NON-NLS-2$
		TypeNameDecider typeNameDecider= new TypeNameDecider(parameter);

		MethodInvocation getValueMi0= getValueMis.get(0);
		ITypeBinding typeBinding= getValueMi0.getExpression().resolveTypeBinding();
		MethodInvocation entrySetMethod= ast.newMethodInvocation();
		entrySetMethod.setExpression(ASTNodes.createMoveTarget(rewrite, mapExpression));
		entrySetMethod.setName(ast.newSimpleName("entrySet")); //$NON-NLS-1$

		rewrite.set(enhancedFor, EnhancedForStatement.EXPRESSION_PROPERTY, entrySetMethod, group);

		MethodInvocation getKeyMethod= ast.newMethodInvocation();
		getKeyMethod.setExpression(ast.newSimpleName(entryVar));
		getKeyMethod.setName(ast.newSimpleName("getKey")); //$NON-NLS-1$

		if (typeBinding != null && typeBinding.isRawType()) {
			// for (Object key : map.keySet()) => for (Object key : map.entrySet())
			refactorRawMap(enhancedFor, parameter, getValueMis, rewrite, ast, group, keyUses, insertionPoint, entryVar,
					typeNameDecider, getKeyMethod);
		} else {
			// for (K key : map.keySet()) => for (K key : map.entrySet())
			// for (K key : map.entrySet()) => for (Map.Entry<K, V> mapEntry :
			// map.entrySet())
			refactorGenericMap(enhancedFor, parameter, getValueMis, rewrite, ast, group, keyUses, entryVar,
					typeNameDecider, getValueMi0, getKeyMethod);
		}

		// Replace all occurrences of map.get(key) => mapEntry.getValue()
		for (MethodInvocation getValueMi : getValueMis) {
			MethodInvocation getValueMethod= ast.newMethodInvocation();
			getValueMethod.setExpression(ast.newSimpleName(entryVar));
			getValueMethod.setName(ast.newSimpleName("getValue")); //$NON-NLS-1$
			MethodInvocation newMethodInvocation= getValueMethod;
			rewrite.replace(getValueMi, newMethodInvocation, group);
		}
	}

	private void refactorRawMap(final EnhancedForStatement enhancedFor, final SingleVariableDeclaration parameter,
			final List<MethodInvocation> getValueMis, final ASTRewrite rewrite, final ASTNodeFactory ast, final TextEditGroup group,
			final int keyUses, final int insertionPoint, final String entryVar, final TypeNameDecider typeNameDecider,
			final MethodInvocation getKeyMethod) {
		Type objectType= ast.type(typeNameDecider.useSimplestPossibleName(Object.class.getCanonicalName()));
		String objectVar= new VariableNameDecider(enhancedFor.getBody(), insertionPoint).suggest("obj"); //$NON-NLS-1$
		rewrite.set(enhancedFor, EnhancedForStatement.PARAMETER_PROPERTY, ast.newSingleVariableDeclaration(objectVar, objectType), group);

		// for (Map.Entry<K, V> mapEntry : map.entrySet()) {
		// Map.Entry mapEntry = (Map.Entry) obj; // <--- add this statement
		// Object key = mapEntry.getKey(); // <--- add this statement

		Type mapKeyType= ast.createCopyTarget(parameter.getType());
		VariableDeclarationFragment newVariableDeclarationFragment= ast.newVariableDeclarationFragment(ASTNodes.createMoveTarget(rewrite, parameter.getName()));
		newVariableDeclarationFragment.setInitializer(getKeyMethod);
		VariableDeclarationStatement newKeyDecl= ast.newVariableDeclarationStatement(mapKeyType, newVariableDeclarationFragment);

		rewrite.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newKeyDecl, group);

		if (keyUses > getValueMis.size()) {
			String mapEntryTypeName= typeNameDecider.useSimplestPossibleName(Entry.class.getCanonicalName());

			VariableDeclarationStatement newEntryDecl= ast.newVariableDeclarationStatement(
					ast.type(mapEntryTypeName),
					ast.newVariableDeclarationFragment(ast.newSimpleName(entryVar), ast.newCastExpression(ast.type(mapEntryTypeName), ast.newSimpleName(objectVar))));
			rewrite.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newEntryDecl, group);
		}
	}

	private void refactorGenericMap(final EnhancedForStatement enhancedFor, final SingleVariableDeclaration parameter,
			final List<MethodInvocation> getValueMis, final ASTRewrite rewrite, final ASTNodeFactory ast, final TextEditGroup group,
			final int keyUses, final String entryVar, final TypeNameDecider typeNameDecider, final MethodInvocation getValueMi0,
			final MethodInvocation getKeyMethod) {
		Type mapEntryType= createMapEntryType(parameter, getValueMi0, typeNameDecider);
		rewrite.set(enhancedFor, EnhancedForStatement.PARAMETER_PROPERTY, ast.newSingleVariableDeclaration(entryVar, mapEntryType), group);

		if (keyUses > getValueMis.size()) {
			// for (Map.Entry<K, V> mapEntry : map.entrySet()) {
			// K key = mapEntry.getKey(); // <--- add this statement
			Type mapKeyType= ast.createCopyTarget(parameter.getType());

			VariableDeclarationFragment newVariableDeclarationFragment= ast.newVariableDeclarationFragment(ASTNodes.createMoveTarget(rewrite, parameter.getName()));
			newVariableDeclarationFragment.setInitializer(getKeyMethod);
			VariableDeclarationStatement newKeyDeclaration= ast.newVariableDeclarationStatement(mapKeyType, newVariableDeclarationFragment);
			rewrite.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newKeyDeclaration, group);
		}
	}

	/**
	 * If possible, use the type declaration, so we can return the type as it was
	 * declared. Otherwise, let's use the type binding and output verbose fully
	 * qualified types.
	 */
	private Type createMapEntryType(final SingleVariableDeclaration parameter, final MethodInvocation getValueMi,
			final TypeNameDecider typeNameDecider) {
		String mapEntryType= typeNameDecider.useSimplestPossibleName(Entry.class.getCanonicalName());

		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		Type paramType= parameter.getType();
		Type mapKeyType;
		if (paramType.isPrimitiveType()) {
			// Use the type binding (not as precise as what is in the code)
			ITypeBinding mapTypeBinding= getValueMi.getExpression().resolveTypeBinding();
			ITypeBinding keyTypeBinding= mapTypeBinding.getTypeArguments()[0];
			mapKeyType= ast.toType(keyTypeBinding, typeNameDecider);
		} else {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			// Use the type as defined in the code
			mapKeyType= ASTNodes.createMoveTarget(rewrite, paramType);
		}
		Type mapValueType= ast.copyType(getValueMi, typeNameDecider);
		return ast.newParameterizedType(mapEntryType, mapKeyType, mapValueType);
	}

	private boolean isKeySetMethod(final MethodInvocation foreachExpression) {
		return foreachExpression != null && ASTNodes.usesGivenSignature(foreachExpression, Map.class.getCanonicalName(), "keySet"); //$NON-NLS-1$
	}

	private List<MethodInvocation> collectMapGetValueCalls(final Expression mapExpression,
			final SingleVariableDeclaration parameter, final Statement body) {
		return new CollectMapGetCalls(mapExpression, parameter).collect(body);
	}

	/** Sanity check. */
	private boolean haveSameTypeBindings(final Collection<? extends Expression> exprs) {
		Iterator<? extends Expression> it= exprs.iterator();

		if (!it.hasNext()) {
			// Not really expected
			return false;
		}

		ITypeBinding type0= it.next().resolveTypeBinding();

		if (type0 == null) {
			return false;
		}

		while (it.hasNext()) {
			ITypeBinding typeN= it.next().resolveTypeBinding();

			if (!areSameTypeBindings(type0, typeN)) {
				return false;
			}
		}

		return true;
	}

	private boolean areSameTypeBindings(final ITypeBinding type1, final ITypeBinding type2) {
		return type1 == null
				|| type2 == null
				|| type1.isParameterizedType() == type2.isParameterizedType() && areSameParameterizedTypeBindings(type1, type2);
	}

	/** Special handling because of captures. */
	private boolean areSameParameterizedTypeBindings(final ITypeBinding type1, final ITypeBinding type2) {
		return type1.getErasure().equals(type2.getErasure())
				&& areSameTypeBindings(type1.getTypeArguments(), type2.getTypeArguments());
	}

	private boolean areSameTypeBindings(final ITypeBinding[] types1, final ITypeBinding[] types2) {
		if (types1.length != types2.length) {
			return false;
		}

		for (int i= 0; i < types1.length; i++) {
			if (!areSameTypeBindings(types1[i], types2[i])) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Class to find {@code map.get(loopVariable)} constructs in the AST tree, and
	 * collect the type of the value, which is unknown until one is located.
	 */
	static class CollectMapGetCalls extends CollectorVisitor<MethodInvocation> {
		private final Expression mapExpression;
		private final SingleVariableDeclaration forEachParameter;

		public CollectMapGetCalls(final Expression mapExpression, final SingleVariableDeclaration forEachParameter) {
			this.mapExpression= mapExpression;
			this.forEachParameter= forEachParameter;
		}

		@Override
		public boolean visit(final MethodInvocation visited) {
			if (isSameReference(visited.getExpression(), mapExpression)
					&& ASTNodes.usesGivenSignature(visited, Map.class.getCanonicalName(), "get", Object.class.getCanonicalName()) //$NON-NLS-1$
					&& ASTNodes.isSameVariable((Expression) visited.arguments().get(0), forEachParameter.getName())) {
				addResult(visited);
			}

			return true;
		}

		private boolean isSameReference(final Expression expression1, final Expression expression2) {
			if (expression1 == null || expression2 == null) {
				return false;
			}
			if (expression1.getNodeType() != ASTNode.METHOD_INVOCATION || expression2.getNodeType() != ASTNode.METHOD_INVOCATION) {
				return ASTNodes.isSameVariable(expression1, expression2);
			}
			MethodInvocation mi1= (MethodInvocation) expression1;
			MethodInvocation mi2= (MethodInvocation) expression2;
			return ASTNodes.areBindingsEqual(mi1.resolveTypeBinding(), mi2.resolveTypeBinding())
					&& isSameReference(mi1.getExpression(), mi2.getExpression());
		}
	}
}
