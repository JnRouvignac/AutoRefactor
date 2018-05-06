/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2017 Jean-Noël Rouvignac - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.catchClauses;
import static org.autorefactor.refactoring.ASTHelper.fragments;
import static org.autorefactor.refactoring.ASTHelper.getOverridenMethods;
import static org.autorefactor.refactoring.ASTHelper.isSameVariable;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.autorefactor.refactoring.ASTHelper.resolveTypeBinding;
import static org.autorefactor.refactoring.ASTHelper.types;
import static org.autorefactor.util.Utils.equalNotNull;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_TYPE;
import static org.eclipse.jdt.core.dom.ASTNode.UNION_TYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class UseMultiCatchRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Multi-catch";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactors catch clauses with the same body to use Java 7's multi-catch.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and testing cost. It also upgrades legacy code.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    private enum MergeDirection {
        NONE, UP, DOWN;
    }

    private static abstract class Binding {
        protected abstract Boolean isSubTypeCompatible(Binding type);
    }

    private static class SingleBinding extends Binding {
        private final ITypeBinding typeBinding;

        public SingleBinding(ITypeBinding typeBinding) {
            this.typeBinding = typeBinding;
        }

        @Override
        protected Boolean isSubTypeCompatible(Binding other) {
            if (typeBinding == null) {
                return false;
            } else if (other instanceof SingleBinding) {
                SingleBinding o = (SingleBinding) other;
                return typeBinding.isSubTypeCompatible(o.typeBinding);
            } else if (other instanceof MultiBinding) {
                MultiBinding o = (MultiBinding) other;
                for (ITypeBinding otherTypeBinding : o.typeBindings) {
                    if (otherTypeBinding == null || !typeBinding.isSubTypeCompatible(otherTypeBinding)) {
                        return false;
                    }
                }
                return true;
            } else {
                throw new NotImplementedException(null, other);
            }
        }

        @Override
        public String toString() {
            return typeBinding.getName();
        }
    }

    private static class MultiBinding extends Binding {
        private final ITypeBinding[] typeBindings;

        public MultiBinding(ITypeBinding[] typeBindings) {
            this.typeBindings = typeBindings;
        }

        @Override
        protected Boolean isSubTypeCompatible(Binding other) {
            if (other instanceof SingleBinding) {
                SingleBinding o = (SingleBinding) other;
                if (o.typeBinding == null) {
                    return null;
                }
                boolean anySubTypeCompatible = false;
                for (ITypeBinding typeBinding : typeBindings) {
                    if (typeBinding == null) {
                        return null;
                    } else if (typeBinding.isSubTypeCompatible(o.typeBinding)) {
                        anySubTypeCompatible = true;
                    }
                }
                return anySubTypeCompatible;
            } else if (other instanceof MultiBinding) {
                MultiBinding o = (MultiBinding) other;
                boolean anySubTypeCompatible = false;
                for (ITypeBinding typeBinding : typeBindings) {
                    if (typeBinding == null) {
                        return null;
                    }
                    for (ITypeBinding otherTypeBinding : o.typeBindings) {
                        if (otherTypeBinding == null) {
                            return null;
                        } else if (typeBinding.isSubTypeCompatible(otherTypeBinding)) {
                            anySubTypeCompatible = true;
                        }
                    }
                }
                return anySubTypeCompatible;
            } else {
                throw new NotImplementedException(null, other);
            }
        }

        @Override
        public String toString() {
            final StringBuilder sb = new StringBuilder();
            for (ITypeBinding typeBinding : typeBindings) {
                if (sb.length() > 0) {
                    sb.append(" | ");
                }
                sb.append(typeBinding.getName());
            }
            return sb.toString();
        }
    }

    private static final class MultiCatchASTMatcher extends ASTSemanticMatcher {
        private final Map<ASTNode, ASTNode> matchingVariables = new HashMap<ASTNode, ASTNode>();

        public MultiCatchASTMatcher(CatchClause catchClause1, CatchClause catchClause2) {
            matchingVariables.put(catchClause1.getException(), catchClause2.getException());
        }

        @Override
        public boolean match(VariableDeclarationStatement node, Object other) {
            return super.match(node, other)
                    || matchVariableDeclarationsWithDifferentNames(node, other);
        }

        private boolean matchVariableDeclarationsWithDifferentNames(
                VariableDeclarationStatement node, Object other) {
            if (!(other instanceof VariableDeclarationStatement)) {
                return false;
            }

            VariableDeclarationStatement node2 = (VariableDeclarationStatement) other;
            List<VariableDeclarationFragment> fragments1 = fragments(node);
            List<VariableDeclarationFragment> fragments2 = fragments(node2);
            if (fragments1.size() == fragments2.size()) {
                Iterator<VariableDeclarationFragment> it1 = fragments1.iterator();
                Iterator<VariableDeclarationFragment> it2 = fragments2.iterator();
                // Do not make all efforts to try to reconcile fragments declared in different order
                while (it1.hasNext() && it2.hasNext()) {
                    VariableDeclarationFragment f1 = it1.next();
                    VariableDeclarationFragment f2 = it2.next();
                    if (equalNotNull(resolveTypeBinding(f1), resolveTypeBinding(f2))
                            // This structural match is a bit dumb
                            // It cannot reconcile 1 with 1L, true with Boolean.TRUE, etc.
                            // Let's rely on other refactoring rules which will simplify such expressions
                            // and convert 1L => 1 (in long context), Boolean.TRUE to true (in boolean context), etc.
                            && ASTHelper.match(this, f1.getInitializer(), f2.getInitializer())) {
                        this.matchingVariables.put(f1, f2);
                        return true;
                    }
                }
            }
            return false;
        }

        @Override
        public boolean match(SimpleName node, Object other) {
            return super.match(node, other)
                    || areBothReferringToSameVariables(node, other);
        }

        @Override
        public boolean match(MethodInvocation mi1, Object other) {
            if (other instanceof MethodInvocation) {
                MethodInvocation mi2 = (MethodInvocation) other;
                return super.match(mi1, mi2)
                        && isSameMethodBinding(mi1.resolveMethodBinding(), mi2.resolveMethodBinding());
            }
            return false;
        }

        @Override
        public boolean match(SuperMethodInvocation mi1, Object other) {
            if (other instanceof SuperMethodInvocation) {
                SuperMethodInvocation mi2 = (SuperMethodInvocation) other;
                return super.match(mi1, mi2)
                        && isSameMethodBinding(mi1.resolveMethodBinding(), mi2.resolveMethodBinding());
            }
            return false;
        }

        @Override
        public boolean match(ClassInstanceCreation cic1, Object other) {
            if (other instanceof ClassInstanceCreation) {
                ClassInstanceCreation cic2 = (ClassInstanceCreation) other;
                return super.match(cic1, cic2)
                        && isSameMethodBinding(cic1.resolveConstructorBinding(), cic2.resolveConstructorBinding());
            }
            return false;
        }

        private boolean isSameMethodBinding(IMethodBinding binding1, IMethodBinding binding2) {
            return binding1 != null
                    && binding2 != null
                    && (binding1.equals(binding2)
                            || binding1.overrides(binding2)
                            || binding2.overrides(binding1)
                            // this is a really expensive check. Do it at the very end
                            || areOverridingSameMethod(binding1, binding2));
        }

        private boolean areOverridingSameMethod(IMethodBinding binding1, IMethodBinding binding2) {
            Set<IMethodBinding> commonOverridenMethods = getOverridenMethods(binding1);
            commonOverridenMethods.retainAll(getOverridenMethods(binding2));
            return !commonOverridenMethods.isEmpty();
        }

        private boolean areBothReferringToSameVariables(ASTNode node, Object other) {
            for (Entry<ASTNode, ASTNode> pairedVariables : matchingVariables.entrySet()) {
                if (isSameVariable(node, pairedVariables.getKey())) {
                    return isSameVariable0(other, pairedVariables.getValue());
                }
            }
            return false;
        }

        private boolean isSameVariable0(Object other, ASTNode node2) {
            return other instanceof ASTNode
                    && isSameVariable((ASTNode) other, node2);
        }
    }

    @Override
    public boolean visit(TryStatement node) {
        List<CatchClause> catchClauses = catchClauses(node);
        Binding[] typeBindings = resolveTypeBindings(catchClauses);
        for (int i = 0; i < catchClauses.size(); i++) {
            CatchClause catchClause1 = catchClauses.get(i);
            for (int j = i + 1; j < catchClauses.size(); j++) {
                CatchClause catchClause2 = catchClauses.get(j);
                MergeDirection direction = mergeDirection(typeBindings, i, j);
                if (!MergeDirection.NONE.equals(direction)
                        && matchMultiCatch(catchClause1, catchClause2)) {
                    Refactorings r = this.ctx.getRefactorings();
                    UnionType ut = unionTypes(
                            catchClause1.getException().getType(),
                            catchClause2.getException().getType());
                    if (MergeDirection.UP.equals(direction)) {
                        r.set(catchClause1.getException(), SingleVariableDeclaration.TYPE_PROPERTY, ut);
                        r.remove(catchClause2);
                    } else if (MergeDirection.DOWN.equals(direction)) {
                        r.remove(catchClause1);
                        r.set(catchClause2.getException(), SingleVariableDeclaration.TYPE_PROPERTY, ut);
                    }
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private Binding[] resolveTypeBindings(List<CatchClause> catchClauses) {
        final Binding[] results = new Binding[catchClauses.size()];
        for (int i = 0; i < catchClauses.size(); i++) {
            results[i] = resolveBinding(catchClauses.get(i));
        }
        return results;
    }

    private Binding resolveBinding(CatchClause catchClause) {
        SingleVariableDeclaration svd = catchClause.getException();
        Type type = svd.getType();
        switch (type.getNodeType()) {
        case SIMPLE_TYPE:
            return new SingleBinding(type.resolveBinding());

        case UNION_TYPE:
            List<Type> types = types((UnionType) type);
            ITypeBinding[] typeBindings = new ITypeBinding[types.size()];
            for (int j = 0; j < types.size(); j++) {
                typeBindings[j] = types.get(j).resolveBinding();
            }
            return new MultiBinding(typeBindings);

        default:
            // TODO JNR throw
            return null;
        }
    }

    private boolean matchMultiCatch(CatchClause catchClause1, CatchClause catchClause2) {
        final MultiCatchASTMatcher matcher = new MultiCatchASTMatcher(catchClause1, catchClause2);
        return match(matcher, catchClause1.getBody(), catchClause2.getBody());
    }

    private MergeDirection mergeDirection(Binding[] typeBindings, int start, int end) {
        if (canMergeTypesDown(typeBindings, start, end)) {
            return MergeDirection.DOWN;
        } else if (canMergeTypesUp(typeBindings, start, end)) {
            return MergeDirection.UP;
        } else {
            return MergeDirection.NONE;
        }
    }

    private boolean canMergeTypesDown(final Binding[] types, int start, int end) {
        final Binding startType = types[start];
        for (int i = start + 1; i < end; i++) {
            final Binding type = types[i];
            if (Boolean.TRUE.equals(startType.isSubTypeCompatible(type))) {
                return false;
            }
        }
        return true;
    }

    private boolean canMergeTypesUp(final Binding[] types, int start, int end) {
        final Binding endType = types[end];
        for (int i = start + 1; i < end; i++) {
            final Binding type = types[i];
            if (Boolean.TRUE.equals(type.isSubTypeCompatible(endType))) {
                return false;
            }
        }
        return true;
    }

    private UnionType unionTypes(Type... types) {
        final List<Type> allTypes = new ArrayList<Type>();
        collectAllUnionedTypes(allTypes, Arrays.asList(types));
        removeSupersededAlternatives(allTypes);

        final ASTBuilder b = this.ctx.getASTBuilder();
        final UnionType result = this.ctx.getAST().newUnionType();
        final List<Type> unionedTypes = types(result);
        for (Type unionedType : allTypes) {
            unionedTypes.add(b.copy(unionedType));
        }
        return result;
    }

    private void collectAllUnionedTypes(List<Type> results, Collection<Type> types) {
        for (final Type type : types) {
            if (type instanceof UnionType) {
                final UnionType ut = (UnionType) type;
                collectAllUnionedTypes(results, types(ut));
            } else {
                results.add(type);
            }
        }
    }

    private void removeSupersededAlternatives(List<Type> allTypes) {
        for (ListIterator<Type> it1 = allTypes.listIterator(); it1.hasNext();) {
            final ITypeBinding binding1 = it1.next().resolveBinding();
            for (ListIterator<Type> it2 = allTypes.listIterator(it1.nextIndex()); it2.hasNext();) {
                final ITypeBinding binding2 = it2.next().resolveBinding();
                if (binding1.isSubTypeCompatible(binding2)) {
                    it1.remove();
                    break;
                }
            }
        }
    }
}
