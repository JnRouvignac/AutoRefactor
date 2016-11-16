/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.SubMonitor;
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
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PackageDeclaration;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.TypeNameMatch;
import org.eclipse.jdt.core.search.TypeNameMatchRequestor;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;
import static org.eclipse.jdt.core.dom.IBinding.METHOD;
import static org.eclipse.jdt.core.dom.IBinding.TYPE;
import static org.eclipse.jdt.core.dom.IBinding.VARIABLE;
import static org.eclipse.jdt.core.dom.Modifier.*;
import static org.eclipse.jdt.core.search.IJavaSearchConstants.*;
import static org.eclipse.jdt.core.search.SearchEngine.*;
import static org.eclipse.jdt.core.search.SearchPattern.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class ReplaceQualifiedNamesBySimpleNamesRefactoring extends AbstractRefactoringRule {
    private static final class QName {
        private QName qualifier;
        private String simpleName;

        private static QName valueOf(String fullyQualifiedName) {
            QName qname = null;
            for (String name : fullyQualifiedName.split("\\.")) {
                qname = new QName(qname, name);
            }
            return qname;
        }

        private static QName valueOf(String qualifiedName, String simpleName) {
            return new QName(valueOf(qualifiedName), simpleName);
        }

        private QName(QName qualifier, String simpleName) {
            this.qualifier = qualifier;
            this.simpleName = simpleName;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            QName other = (QName) obj;
            return Objects.equals(simpleName, other.simpleName)
                    && Objects.equals(qualifier, other.qualifier);
        }

        @Override
        public int hashCode() {
            return Objects.hash(simpleName, qualifier);
        }

        @Override
        public String toString() {
            return qualifier != null ? qualifier + "." + simpleName : simpleName;
        }
    }

    private enum FqnType {
        METHOD, FIELD, TYPE
    }

    /** Information about fully-qualified names. */
    private static final class FQN {
        private static final FQN CANNOT_REPLACE_SIMPLE_NAME = new FQN(null, false, false);
        private QName fullyQualifiedName;
        private boolean fromImport;
        /**
         * Whether an import was a star import (static or not).
         * <p>
         * Only applicable when <code>{@link #fromImport} == true</code>.
         */
        private boolean onDemand;

        public FQN(QName fullyQualifiedName, boolean fromImport, boolean onDemand) {
            this.fullyQualifiedName = fullyQualifiedName;
            this.fromImport = fromImport;
            this.onDemand = onDemand;
        }

        private static FQN fromImport(QName fullyQualifiedName, boolean onDemand) {
            return new FQN(fullyQualifiedName, true, onDemand);
        }

        private static FQN fromLocal(QName fullyQualifiedName) {
            return new FQN(fullyQualifiedName, false, false /* meaningless */);
        }

        public boolean isLocal() {
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
            if (this.equals(CANNOT_REPLACE_SIMPLE_NAME)) {
                return "CANNOT_NOT_REPLACE_SIMPLE_NAME";
            }
            return fullyQualifiedName + (fromImport ? " (imported)" : " (local)");
        }
    }

    private static final class Names {
        /**
         * Simple names for java elements in use in this compilation unit.
         * It merges imports and local declarations.
         */
        private final Map<String, List<FQN>> simpleNames = new TreeMap<String, List<FQN>>();

        private void addName(FQN fqn) {
            addName(fqn.fullyQualifiedName.simpleName, fqn);
        }

        private void cannotReplaceSimpleName(String simpleName) {
            addName(simpleName, FQN.CANNOT_REPLACE_SIMPLE_NAME);
        }

        private void addName(final String simpleName, FQN fqn) {
            List<FQN> existingFqns = simpleNames.get(simpleName);
            if (existingFqns == null) {
                existingFqns = new ArrayList<FQN>();
                simpleNames.put(simpleName, existingFqns);
            }
            existingFqns.add(fqn);
        }

        private boolean canReplaceFqnWithSimpleName(ASTNode node, QName fullyQualifiedName, FqnType fqnType) {
            final String simpleName = fullyQualifiedName.simpleName;
            final List<FQN> matches = getBestMatches(simpleName);
            switch (matches.size()) {
            case 0:
                return false;

            case 1:
                return fullyQualifiedName.equals(matches.get(0).fullyQualifiedName);

            default:
                final ITypeBinding enclosingTypeBinding = resolveEnclosingTypeBinding(node);
                if (enclosingTypeBinding != null && matches.get(0).isLocal()) {
                    // all matches are local to this class
                    final ITypeBinding declaringType =
                            getDeclaringTypeInTypeHierarchy(enclosingTypeBinding, simpleName, fqnType, node);
                    if (declaringType != null) {
                        return fullyQualifiedName.equals(QName.valueOf(declaringType.getQualifiedName(), simpleName));
                    }
                    // whether we can replace a simple name highly depends on the current context
                    // are we in an inner class, is this from the enclosing class, etc.
                    QName enclosingTypeQName = QName.valueOf(enclosingTypeBinding.getQualifiedName());
                    return existsInAnyEnclosingType(matches, fullyQualifiedName, enclosingTypeQName);
                }
                return false;
            }
        }

        private boolean existsInAnyEnclosingType(
                List<FQN> matches, QName fullyQualifiedName, QName enclosingTypeQName) {
            while (enclosingTypeQName != null) {
                if (existsInEnclosingType(matches, fullyQualifiedName, enclosingTypeQName)) {
                    return true;
                }
                enclosingTypeQName = enclosingTypeQName.qualifier;
            }
            return false;
        }

        private boolean existsInEnclosingType(List<FQN> matches, QName fullyQualifiedName, QName enclosingTypeQName) {
            QName enclosingQName = new QName(enclosingTypeQName, fullyQualifiedName.simpleName);
            for (FQN fqn : matches) {
                if (fqn.fullyQualifiedName.equals(fullyQualifiedName)
                        && fqn.fullyQualifiedName.equals(enclosingQName)) {
                    return true;
                }
            }
            return false;
        }

        private ITypeBinding getDeclaringTypeInTypeHierarchy(
                ITypeBinding typeBinding, String simpleName, FqnType fqnType, ASTNode node) {
            while (typeBinding != null) {
                if (isDeclaredInType(typeBinding, simpleName, fqnType, node)) {
                    return typeBinding;
                }
                typeBinding = typeBinding.getSuperclass();
            }
            return null;
        }

        private boolean isDeclaredInType(ITypeBinding typeBinding, String simpleName, FqnType fqnType, ASTNode node) {
            return hasSimpleName(getDeclaredBinding(typeBinding, fqnType, node), simpleName);
        }

        private IBinding[] getDeclaredBinding(ITypeBinding typeBinding, FqnType fqnType, ASTNode node) {
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

        private boolean hasSimpleName(IBinding[] bindings, String simpleName) {
            for (IBinding binding : bindings) {
                if (binding.getName().equals(simpleName)) {
                    return true;
                }
            }
            return false;
        }

        private List<FQN> getBestMatches(String simpleName) {
            final List<FQN> fqns = simpleNames.get(simpleName);
            if (fqns == null) {
                return Collections.emptyList();
            }
            List<FQN> bestMatches = new ArrayList<FQN>();
            for (FQN fqn : fqns) {
                if (fqn.equals(FQN.CANNOT_REPLACE_SIMPLE_NAME)) {
                    // something got wrong while computing the FQNs => bail out
                    return Collections.emptyList();
                } else if (bestMatches.isEmpty()) {
                    // we now have a best match
                    bestMatches.add(fqn);
                } else if (bestMatches.get(0).fromImport()) {
                    if (fqn.isLocal()) {
                        // local FQNs take precedence over imported FQNs
                        bestMatches.clear();
                        bestMatches.add(fqn);
                    } else {
                        // fqn is imported
                        if (bestMatches.get(0).fromStarImport()) {
                            if (fqn.fromSpecificImport()) {
                                // specific imports take precedence over on-demand imports
                                bestMatches.clear();
                                bestMatches.add(fqn);
                            } else {
                                // group imported FQNs together
                                bestMatches.add(fqn);
                            }
                        } else if (fqn.fromSpecificImport()) {
                            // import conflicts:
                            // there is more than one specific import.
                            // this is a compile-error: do not try to replace
                            return Collections.emptyList();
                        }
                    }
                } else if (fqn.isLocal()) {
                    // group local FQNs together
                    bestMatches.add(fqn);
                }
            }
            return bestMatches;
        }

        private ITypeBinding resolveEnclosingTypeBinding(ASTNode node) {
            ASTNode enclosingType = getEnclosingType(node);
            if (enclosingType != null) {
                switch (enclosingType.getNodeType()) {
                case ANONYMOUS_CLASS_DECLARATION:
                    return ((AnonymousClassDeclaration) enclosingType).resolveBinding();
                case ENUM_DECLARATION:
                    return ((EnumDeclaration) enclosingType).resolveBinding();
                case TYPE_DECLARATION:
                    return ((TypeDeclaration) enclosingType).resolveBinding();
                default:
                    throw new NotImplementedException(node, enclosingType);
                }
            }
            return null;
        }

        private void clear() {
            simpleNames.clear();
        }

        @Override
        public String toString() {
            return getClass().getSimpleName() + "(simpleNames=" + simpleNames + ")";
        }
    }

    private final Names types = new Names();
    private final Names methods = new Names();
    private final Names fields = new Names();

    @Override
    public String getDescription() {
        return "Refactors types, method invocations and field accesses"
                + " to replace qualified names by simple names when appropriate."
                + " For example when relevant imports exist.";
    }

    @Override
    public String getName() {
        return "Replace qualified names by simple names";
    }

    @Override
    public boolean visit(final ImportDeclaration node) {
        final QName qname = QName.valueOf(node.getName().getFullyQualifiedName());
        if (node.isStatic()) {
            if (node.isOnDemand()) {
                importStaticTypesAndMembersFromType(node);
            } else {
                importStaticTypeOrMember(node, qname);
            }
        } else {
            if (node.isOnDemand()) {
                final String pkgName = node.getName().getFullyQualifiedName();
                importTypesFromPackage(pkgName, node);
            } else {
                types.addName(FQN.fromImport(qname, false));
            }
        }
        return VISIT_SUBTREE;
    }

    private void importStaticTypesAndMembersFromType(ImportDeclaration node) {
        final IBinding binding = node.resolveBinding();
        if (binding == null) {
            return;
        }
        if (binding.getKind() != TYPE) {
            throw new NotImplementedException(node, "for a binding of kind " + binding.getKind());
        }
        final ITypeBinding typeBinding = (ITypeBinding) binding;
        final String typeName = typeBinding.getQualifiedName();
        for (IMethodBinding method : typeBinding.getDeclaredMethods()) {
            if (canAdd(method.getModifiers(), method.isSynthetic())) {
                QName qname = QName.valueOf(typeName, method.getName());
                methods.addName(FQN.fromImport(qname, true));
            }
        }
        for (IVariableBinding field : typeBinding.getDeclaredFields()) {
            if (canAdd(field.getModifiers(), field.isSynthetic())) {
                QName qname = QName.valueOf(typeName, field.getName());
                fields.addName(FQN.fromImport(qname, true));
            }
        }
        for (ITypeBinding memberType : typeBinding.getDeclaredTypes()) {
            if (canAdd(memberType.getModifiers(), memberType.isSynthetic())) {
                QName qname = QName.valueOf(memberType.getQualifiedName());
                types.addName(FQN.fromImport(qname, true));
            }
        }
    }

    private boolean canAdd(int modifiers, boolean isSynthetic) {
        return isStatic(modifiers) && !isPrivate(modifiers) && !isSynthetic;
    }

    private void importStaticTypeOrMember(ImportDeclaration node, QName fullyQualifiedName) {
        final IBinding binding = node.resolveBinding();
        if (binding == null) {
            return;
        }
        switch (binding.getKind()) {
        case METHOD:
            methods.addName(FQN.fromImport(fullyQualifiedName, false));
            break;
        case VARIABLE:
            fields.addName(FQN.fromImport(fullyQualifiedName, false));
            break;
        case TYPE:
            types.addName(FQN.fromImport(fullyQualifiedName, false));
            break;
        }
    }

    private void importTypesFromPackage(final String pkgName, ASTNode node) {
        final TypeNameMatchRequestor importTypeCollector = new TypeNameMatchRequestor() {
            @Override
            public void acceptTypeNameMatch(TypeNameMatch typeNameMatch) {
                final boolean isTopLevelType = typeNameMatch.getType().getDeclaringType() == null;
                if (isTopLevelType) {
                    if (!pkgName.equals(typeNameMatch.getPackageName())) {
                        // sanity check failed
                        throw new IllegalStateException("Expected package '" + typeNameMatch.getPackageName()
                                                        + "' to be equal to '" + pkgName + "'");
                    }
                    QName qname = QName.valueOf(typeNameMatch.getFullyQualifiedName());
                    types.addName(FQN.fromImport(qname, true));
                }
            }
        };

        SubMonitor monitor = SubMonitor.convert(ctx.getProgressMonitor(), 1);
        final SubMonitor childMonitor = monitor.newChild(1);
        try {
            final SearchEngine searchEngine = new SearchEngine();
            searchEngine.searchAllTypeNames(
                    pkgName.toCharArray(), R_EXACT_MATCH, // search in this package
                    null, R_EXACT_MATCH,                  // do not filter by type name
                    TYPE,                                 // look for all java types (class, interfaces, enums, etc.)
                    createWorkspaceScope(),               // search everywhere
                    importTypeCollector,
                    WAIT_UNTIL_READY_TO_SEARCH,           // wait in case the indexer is indexing
                    childMonitor);
        } catch (JavaModelException e) {
            throw new UnhandledException(node, e);
        } finally {
            childMonitor.done();
        }
    }

    @Override
    public boolean visit(CompilationUnit node) {
        node.accept(new NamesCollector());
        importTypesFromPackage("java.lang", node);
        return VISIT_SUBTREE;
    }

    @Override
    public void endVisit(CompilationUnit node) {
        types.clear();
        methods.clear();
        fields.clear();
    }

    private final class NamesCollector extends ASTVisitor {
        @Override
        public boolean visit(TypeDeclaration node) {
            return addTypeNames(node);
        }

        @Override
        public boolean visit(EnumDeclaration node) {
            return addTypeNames(node);
        }

        private boolean addTypeNames(AbstractTypeDeclaration node) {
            final ITypeBinding typeBinding = node.resolveBinding();
            if (typeBinding != null) {
                types.addName(FQN.fromLocal(QName.valueOf(typeBinding.getQualifiedName())));
            } else {
                // We cannot determine the FQN, so we cannot safely replace it
                types.cannotReplaceSimpleName(node.getName().getIdentifier());
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(MethodDeclaration node) {
            final String simpleName = node.getName().getIdentifier();
            final IMethodBinding methodBinding = node.resolveBinding();
            if (methodBinding != null) {
                final QName qname = QName.valueOf(methodBinding.getDeclaringClass().getQualifiedName(), simpleName);
                methods.addName(FQN.fromLocal(qname));
            } else {
                // We cannot determine the FQN, so we cannot safely replace it
                methods.cannotReplaceSimpleName(simpleName);
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(FieldDeclaration node) {
            for (VariableDeclarationFragment vdf : fragments(node)) {
                final String simpleName = vdf.getName().getIdentifier();
                final IVariableBinding variableBinding = vdf.resolveBinding();
                if (variableBinding != null) {
                    fields.addName(FQN.fromLocal(
                            QName.valueOf(variableBinding.getDeclaringClass().getQualifiedName(), simpleName)));
                } else {
                    // We cannot determine the FQN, so we cannot safely replace it
                    fields.cannotReplaceSimpleName(simpleName);
                }
            }
            return VISIT_SUBTREE;
        }
    }

    @Override
    public boolean visit(QualifiedName node) {
        final ASTNode ancestor = getFirstAncestorOrNull(node, PackageDeclaration.class, ImportDeclaration.class);
        final QName qname = getFullyQualifiedNameOrNull(node);
        if (ancestor != null || qname == null) {
            return VISIT_SUBTREE;
        }
        if (types.canReplaceFqnWithSimpleName(node, qname, FqnType.TYPE)
                || fields.canReplaceFqnWithSimpleName(node, qname, FqnType.FIELD)) {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.copy(node.getName()));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private QName getFullyQualifiedNameOrNull(QualifiedName node) {
        final IBinding binding = node.resolveBinding();
        if (binding != null) {
            switch (binding.getKind()) {
            case TYPE:
                final ITypeBinding typeBinding = (ITypeBinding) binding;
                return QName.valueOf(typeBinding.getErasure().getQualifiedName());
            case VARIABLE:
                final IVariableBinding fieldBinding = (IVariableBinding) binding;
                if (hasKind(node.getQualifier(), TYPE)) {
                    return QName.valueOf(fieldBinding.getDeclaringClass().getQualifiedName(), fieldBinding.getName());
                } // else this is a field access like other.fieldName
            }
        }
        return null;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final Expression expr = node.getExpression();
        final IMethodBinding methodBinding = node.resolveMethodBinding();
        if (methodBinding != null
                && expr instanceof Name
                && hasKind((Name) expr, TYPE)
                && node.typeArguments().isEmpty()) {
            final ITypeBinding declaringClass = methodBinding.getDeclaringClass();
            final QName qname = QName.valueOf(declaringClass.getErasure().getQualifiedName(), methodBinding.getName());
            if (methods.canReplaceFqnWithSimpleName(node, qname, FqnType.METHOD)) {
                ctx.getRefactorings().remove(expr);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean hasKind(Name name, int bindingKind) {
        final IBinding binding = name.resolveBinding();
        return binding != null && binding.getKind() == bindingKind;
    }
}
