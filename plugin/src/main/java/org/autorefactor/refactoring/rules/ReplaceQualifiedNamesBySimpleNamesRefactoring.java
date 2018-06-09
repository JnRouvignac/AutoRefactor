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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.fragments;
import static org.autorefactor.refactoring.ASTHelper.getEnclosingType;
import static org.autorefactor.refactoring.ASTHelper.getFirstAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getLocalVariableIdentifiers;
import static org.autorefactor.refactoring.ASTHelper.imports;
import static org.autorefactor.refactoring.ASTHelper.parameters;
import static org.eclipse.jdt.core.dom.ASTNode.ANONYMOUS_CLASS_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.ENUM_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.TYPE_DECLARATION;
import static org.eclipse.jdt.core.dom.IBinding.METHOD;
import static org.eclipse.jdt.core.dom.IBinding.TYPE;
import static org.eclipse.jdt.core.dom.IBinding.VARIABLE;
import static org.eclipse.jdt.core.dom.Modifier.isPrivate;
import static org.eclipse.jdt.core.dom.Modifier.isStatic;
import static org.eclipse.jdt.core.search.IJavaSearchConstants.WAIT_UNTIL_READY_TO_SEARCH;
import static org.eclipse.jdt.core.search.SearchEngine.createWorkspaceScope;
import static org.eclipse.jdt.core.search.SearchPattern.R_EXACT_MATCH;
import static org.autorefactor.util.Utils.equal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.CollectorVisitor;
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
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.TypeNameMatch;
import org.eclipse.jdt.core.search.TypeNameMatchRequestor;

/** See {@link #getDescription()} method. */
public class ReplaceQualifiedNamesBySimpleNamesRefactoring extends AbstractRefactoringRule {
    private static final class QName {
        private final QName qualifier;
        private final String simpleName;

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
            return equal(simpleName, other.simpleName)
                    && equal(qualifier, other.qualifier);
        }

        @Override
        public int hashCode() {
            return (simpleName != null ? simpleName.hashCode() : 0) * 1000
                    + (qualifier != null ? qualifier.hashCode() : 0);
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
        private final QName fullyQualifiedName;
        private final boolean fromImport;
        /**
         * Whether an import was a star import (static or not).
         * <p>
         * Only applicable when <code>{@link #fromImport} == true</code>.
         */
        private final boolean onDemand;

        public FQN(QName fullyQualifiedName, boolean fromImport, boolean onDemand) {
            this.fullyQualifiedName = fullyQualifiedName;
            this.fromImport = fromImport;
            this.onDemand = onDemand;
        }

        private static FQN fromImport(QName fullyQualifiedName, boolean onDemand) {
            return new FQN(fullyQualifiedName, true, onDemand);
        }

        private static FQN fromMember(QName fullyQualifiedName) {
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
            if (this.equals(CANNOT_REPLACE_SIMPLE_NAME)) {
                return "CANNOT_REPLACE_SIMPLE_NAME";
            }
            return fullyQualifiedName + (fromImport ? " (imported)" : " (member)");
        }
    }

    /** Maps simple names to their fully qualified names. */
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
                if (!matches.get(0).isMember()) {
                    return fullyQualifiedName.equals(matches.get(0).fullyQualifiedName);
                }
                //$FALL-THROUGH$

            default:
                final ITypeBinding enclosingTypeBinding = resolveEnclosingTypeBinding(getEnclosingType(node));
                if (enclosingTypeBinding != null && matches.get(0).isMember()) {

                    // All matches are local to this class
                    ITypeBinding declaringType =
                            getDeclaringTypeInTypeHierarchy(enclosingTypeBinding, simpleName, fqnType, node);
                    if (declaringType != null) {
                        return fullyQualifiedName.equals(QName.valueOf(declaringType.getQualifiedName(), simpleName));
                    }

                    // All matches are local to this class
                    declaringType =
                            getDeclaringTypeInTopLevelHierarchy(simpleName, fqnType, node);
                    if (declaringType != null) {
                        return fullyQualifiedName.equals(QName.valueOf(declaringType.getQualifiedName(), simpleName));
                    }

                    // Whether we can replace a simple name highly depends on the current context
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

            if (fullyQualifiedName.equals(enclosingQName)) {
                for (FQN fqn : matches) {
                    if (fqn.fullyQualifiedName.equals(fullyQualifiedName)) {
                        return true;
                    }
                }
            }

            return false;
        }

        private ITypeBinding getDeclaringTypeInTypeHierarchy(
                final ITypeBinding typeBinding, final String simpleName, final FqnType fqnType, final ASTNode node) {
            ITypeBinding superTypeBinding = typeBinding;
            do {
                for (final IBinding binding : getDeclaredBinding(superTypeBinding, fqnType, node)) {
                    if (binding.getName().equals(simpleName)
                            && (Modifier.isPublic(binding.getModifiers())
                                    || Modifier.isProtected(binding.getModifiers())
                                    || (!Modifier.isPrivate(binding.getModifiers())
                                            && superTypeBinding.getPackage().equals(typeBinding.getPackage())))) {
                        return superTypeBinding;
                    }
                }
                superTypeBinding = superTypeBinding.getSuperclass();
            } while (superTypeBinding != null);
            return null;
        }

        private ITypeBinding getDeclaringTypeInTopLevelHierarchy(
                final String simpleName, final FqnType fqnType, final ASTNode node) {
            final Class<?>[] ancestorClasses = { AbstractTypeDeclaration.class, AnonymousClassDeclaration.class };
            ASTNode enclosingType = getFirstAncestorOrNull(node, ancestorClasses);

            while (enclosingType != null) {
                final ITypeBinding enclosingTypeBinding = resolveEnclosingTypeBinding(enclosingType);

                for (final IBinding binding : getDeclaredBinding(enclosingTypeBinding, fqnType, node)) {
                    if (binding.getName().equals(simpleName)) {
                        return enclosingTypeBinding;
                    }
                }
                enclosingType = getFirstAncestorOrNull(enclosingType, ancestorClasses);
            }
            return null;
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
                    if (fqn.isMember()) {
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
                } else if (fqn.isMember()) {
                    // group local FQNs together
                    bestMatches.add(fqn);
                }
            }
            return bestMatches;
        }

        private ITypeBinding resolveEnclosingTypeBinding(ASTNode node) {
            if (node != null) {
                switch (node.getNodeType()) {
                case ANONYMOUS_CLASS_DECLARATION:
                    return ((AnonymousClassDeclaration) node).resolveBinding();
                case ENUM_DECLARATION:
                    return ((EnumDeclaration) node).resolveBinding();
                case TYPE_DECLARATION:
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
            return getClass().getSimpleName() + "(simpleNames=" + simpleNames + ")";
        }
    }

    private final Names types = new Names();
    private final Names methods = new Names();
    private final Names fields = new Names();

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
    public String getName() {
        return "Replace qualified names by simple names";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactors types, method invocations and field accesses"
                + " to replace qualified names by simple names when appropriate."
                + " For example when relevant imports exist.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    @Override
    public boolean visit(CompilationUnit node) {
        resetAllNames();
        return super.visit(node);
    }

    private void readImport(final ImportDeclaration node) {
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

        try {
            final SearchEngine searchEngine = new SearchEngine();
            searchEngine.searchAllTypeNames(
                    pkgName.toCharArray(), R_EXACT_MATCH, // search in this package
                    null, R_EXACT_MATCH,                  // do not filter by type name
                    TYPE,                                 // look for all java types (class, interfaces, enums, etc.)
                    createWorkspaceScope(),               // search everywhere
                    importTypeCollector,
                    WAIT_UNTIL_READY_TO_SEARCH,           // wait in case the indexer is indexing
                    ctx.getProgressMonitor());
        } catch (JavaModelException e) {
            throw new UnhandledException(node, e);
        }
    }

    @Override
    public boolean visit(TypeDeclaration node) {
        final ITypeBinding typeBinding = node.resolveBinding();
        if (typeBinding != null && !typeBinding.isNested() && node.getParent() instanceof CompilationUnit) {
            final CompilationUnit compilationUnit = (CompilationUnit) node.getParent();
            for (final ImportDeclaration importDecl : imports(compilationUnit)) {
                readImport(importDecl);
            }
            importTypesFromPackage("java.lang", compilationUnit);

            node.accept(new NamesCollector());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public void endVisit(TypeDeclaration node) {
        final ITypeBinding typeBinding = node.resolveBinding();
        if (typeBinding != null && !typeBinding.isNested()) {
            resetAllNames();
        }
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
                types.addName(FQN.fromMember(QName.valueOf(typeBinding.getQualifiedName())));
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
                methods.addName(FQN.fromMember(qname));
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
                    fields.addName(FQN.fromMember(
                            QName.valueOf(variableBinding.getDeclaringClass().getQualifiedName(), simpleName)));
                } else {
                    // We cannot determine the FQN, so we cannot safely replace it
                    fields.cannotReplaceSimpleName(simpleName);
                }
            }
            return VISIT_SUBTREE;
        }
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

    @Override
    public boolean visit(FieldDeclaration node) {
        return maybeReplaceFqnsWithSimpleNames(node);
    }

    @Override
    public boolean visit(Initializer node) {
        final Set<String> localVars = getLocalVariableIdentifiers(node.getBody(), true);
        return maybeReplaceFqnsWithSimpleNames(node.getBody(), localVars);
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        // Method parameters
        for (final SingleVariableDeclaration parameter : parameters(node)) {
            if (maybeReplaceFqnsWithSimpleNames(parameter) == DO_NOT_VISIT_SUBTREE) {
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        // Method return value
        if (maybeReplaceFqnsWithSimpleNames(node.getReturnType2()) == DO_NOT_VISIT_SUBTREE) {
            return DO_NOT_VISIT_SUBTREE;
        }

        // Method body
        final Set<String> localIdentifiers = new HashSet<String>();
        for (final SingleVariableDeclaration localParameter : parameters(node)) {
            localIdentifiers.add(localParameter.getName().getIdentifier());
        }
        localIdentifiers.addAll(getLocalVariableIdentifiers(node.getBody(), true));

        return maybeReplaceFqnsWithSimpleNames(node.getBody(), localIdentifiers);
    }

    private boolean maybeReplaceFqnsWithSimpleNames(final ASTNode node) {
        return maybeReplaceFqnsWithSimpleNames(node, Collections.<String>emptySet());
    }

    private boolean maybeReplaceFqnsWithSimpleNames(final ASTNode node, final Set<String> localIdentifiers) {
        if (node != null) {
            final Iterable<QualifiedName> qualifiedNames = new QualifiedNamesCollector().collect(node);
            for (final QualifiedName qualifiedName : qualifiedNames) {
                if (maybeReplaceFqnWithSimpleName(qualifiedName, localIdentifiers) == DO_NOT_VISIT_SUBTREE) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private static final class QualifiedNamesCollector extends CollectorVisitor<QualifiedName> {
        @Override
        public boolean visit(QualifiedName node) {
            addResult(node);
            return VISIT_SUBTREE;
        }
    }

    private boolean maybeReplaceFqnWithSimpleName(final QualifiedName node, final Set<String> localIdentifiers) {
        final ASTNode ancestor = getFirstAncestorOrNull(node, PackageDeclaration.class, ImportDeclaration.class);
        final QName qname = getFullyQualifiedNameOrNull(node);
        if (ancestor != null || qname == null) {
            return VISIT_SUBTREE;
        }
        if (types.canReplaceFqnWithSimpleName(node, qname, FqnType.TYPE)
                || (fields.canReplaceFqnWithSimpleName(node, qname, FqnType.FIELD)
                        && !localIdentifiers.contains(qname.simpleName))) {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.copy(node.getName()));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}
