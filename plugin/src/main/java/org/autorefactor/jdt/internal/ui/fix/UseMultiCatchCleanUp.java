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
package org.autorefactor.jdt.internal.ui.fix;

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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
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
public class UseMultiCatchCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseMultiCatchCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseMultiCatchCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseMultiCatchCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 7;
    }

    private enum MergeDirection {
        NONE, UP, DOWN;
    }

    private abstract static class Binding {
        protected abstract Boolean isSubTypeCompatible(Binding type);
    }

    private static class SingleBinding extends Binding {
        private final ITypeBinding typeBinding;

        public SingleBinding(final ITypeBinding typeBinding) {
            this.typeBinding= typeBinding;
        }

        @Override
        protected Boolean isSubTypeCompatible(final Binding other) {
            if (typeBinding == null) {
                return false;
            }
            if (other instanceof SingleBinding) {
                SingleBinding o= (SingleBinding) other;
                return typeBinding.isSubTypeCompatible(o.typeBinding);
            }
            if (other instanceof MultiBinding) {
                MultiBinding o= (MultiBinding) other;
                for (ITypeBinding otherTypeBinding : o.typeBindings) {
                    if (otherTypeBinding == null || !typeBinding.isSubTypeCompatible(otherTypeBinding)) {
                        return false;
                    }
                }

                return true;
            }
            throw new NotImplementedException(null, other);
        }

        @Override
        public String toString() {
            return typeBinding.getName();
        }
    }

    private static class MultiBinding extends Binding {
        private final ITypeBinding[] typeBindings;

        public MultiBinding(final ITypeBinding[] typeBindings) {
            this.typeBindings= typeBindings;
        }

        @Override
        protected Boolean isSubTypeCompatible(final Binding other) {
            if (other instanceof SingleBinding) {
                SingleBinding o= (SingleBinding) other;
                if (o.typeBinding == null) {
                    return null;
                }
                boolean anySubTypeCompatible= false;
                for (ITypeBinding typeBinding : typeBindings) {
                    if (typeBinding == null) {
                        return null;
                    }
                    if (typeBinding.isSubTypeCompatible(o.typeBinding)) {
                        anySubTypeCompatible= true;
                    }
                }

                return anySubTypeCompatible;
            }
            if (other instanceof MultiBinding) {
                MultiBinding o= (MultiBinding) other;
                boolean anySubTypeCompatible= false;
                for (ITypeBinding typeBinding : typeBindings) {
                    if (typeBinding == null) {
                        return null;
                    }
                    for (ITypeBinding otherTypeBinding : o.typeBindings) {
                        if (otherTypeBinding == null) {
                            return null;
                        }
                        if (typeBinding.isSubTypeCompatible(otherTypeBinding)) {
                            anySubTypeCompatible= true;
                        }
                    }
                }

                return anySubTypeCompatible;
            }
            throw new NotImplementedException(null, other);
        }

        @Override
        public String toString() {
            StringBuilder sb= new StringBuilder();
            for (ITypeBinding typeBinding : typeBindings) {
                if (sb.length() > 0) {
                    sb.append(" | "); //$NON-NLS-1$
                }
                sb.append(typeBinding.getName());
            }

            return sb.toString();
        }
    }

    private static final class MultiCatchASTMatcher extends ASTSemanticMatcher {
        private final Map<ASTNode, ASTNode> matchingVariables= new HashMap<>();

        public MultiCatchASTMatcher(final CatchClause catchClause1, final CatchClause catchClause2) {
            matchingVariables.put(catchClause1.getException(), catchClause2.getException());
        }

        @Override
        public boolean match(final VariableDeclarationStatement node, final Object other) {
            return super.match(node, other) || matchVariableDeclarationsWithDifferentNames(node, other);
        }

        private boolean matchVariableDeclarationsWithDifferentNames(final VariableDeclarationStatement node, final Object other) {
            if (!(other instanceof VariableDeclarationStatement)) {
                return false;
            }

            VariableDeclarationStatement node2= (VariableDeclarationStatement) other;
            List<VariableDeclarationFragment> fragments1= ASTNodes.fragments(node);
            List<VariableDeclarationFragment> fragments2= ASTNodes.fragments(node2);
            if (fragments1.size() == fragments2.size()) {
                Iterator<VariableDeclarationFragment> it1= fragments1.iterator();
                Iterator<VariableDeclarationFragment> it2= fragments2.iterator();
                // Do not make all efforts to try to reconcile fragments declared in different
                // order
                while (it1.hasNext() && it2.hasNext()) {
                    VariableDeclarationFragment f1= it1.next();
                    VariableDeclarationFragment f2= it2.next();
                    if (Utils.equalNotNull(ASTNodes.resolveTypeBinding(f1), ASTNodes.resolveTypeBinding(f2))
                            // This structural match is a bit dumb
                            // It cannot reconcile 1 with 1L, true with Boolean.TRUE, etc.
                            // Let's rely on other cleanup rules which will simplify such expressions
                            // and convert 1L => 1 (in long context), Boolean.TRUE to true (in boolean
                            // context), etc.
                            && ASTNodes.match(this, f1.getInitializer(), f2.getInitializer())) {
                        this.matchingVariables.put(f1, f2);
                        return true;
                    }
                }
            }

            return false;
        }

        @Override
        public boolean match(final SimpleName node, final Object other) {
            return super.match(node, other) || areBothReferringToSameVariables(node, other);
        }

        @Override
        public boolean match(final MethodInvocation mi1, final Object other) {
            if (other instanceof MethodInvocation) {
                MethodInvocation mi2= (MethodInvocation) other;
                return super.match(mi1, mi2)
                        && isSameMethodBinding(mi1.resolveMethodBinding(), mi2.resolveMethodBinding());
            }

            return false;
        }

        @Override
        public boolean match(final SuperMethodInvocation mi1, final Object other) {
            if (other instanceof SuperMethodInvocation) {
                SuperMethodInvocation mi2= (SuperMethodInvocation) other;
                return super.match(mi1, mi2)
                        && isSameMethodBinding(mi1.resolveMethodBinding(), mi2.resolveMethodBinding());
            }

            return false;
        }

        @Override
        public boolean match(final ClassInstanceCreation cic1, final Object other) {
            if (other instanceof ClassInstanceCreation) {
                ClassInstanceCreation cic2= (ClassInstanceCreation) other;
                return super.match(cic1, cic2)
                        && isSameMethodBinding(cic1.resolveConstructorBinding(), cic2.resolveConstructorBinding());
            }

            return false;
        }

        private boolean isSameMethodBinding(final IMethodBinding binding1, final IMethodBinding binding2) {
            return binding1 != null && binding2 != null
                    && (binding1.equals(binding2) || binding1.overrides(binding2) || binding2.overrides(binding1)
                    // This is a really expensive check. Do it at the very end
                            || areOverridingSameMethod(binding1, binding2));
        }

        private boolean areOverridingSameMethod(final IMethodBinding binding1, final IMethodBinding binding2) {
            Set<IMethodBinding> commonOverridenMethods= ASTNodes.getOverridenMethods(binding1);
            commonOverridenMethods.retainAll(ASTNodes.getOverridenMethods(binding2));
            return !commonOverridenMethods.isEmpty();
        }

        private boolean areBothReferringToSameVariables(final ASTNode node, final Object other) {
            for (Entry<ASTNode, ASTNode> pairedVariables : matchingVariables.entrySet()) {
                if (ASTNodes.isSameVariable(node, pairedVariables.getKey())) {
                    return isSameVariable0(other, pairedVariables.getValue());
                }
            }

            return false;
        }

        private boolean isSameVariable0(final Object other, final ASTNode node2) {
            return other instanceof ASTNode && ASTNodes.isSameVariable((ASTNode) other, node2);
        }
    }

    @Override
    public boolean visit(final TryStatement node) {
        List<CatchClause> catchClauses= ASTNodes.catchClauses(node);
        Binding[] typeBindings= resolveTypeBindings(catchClauses);
        for (int i= 0; i < catchClauses.size(); i++) {
            CatchClause catchClause1= catchClauses.get(i);
            for (int j= i + 1; j < catchClauses.size(); j++) {
                CatchClause catchClause2= catchClauses.get(j);
                MergeDirection direction= mergeDirection(typeBindings, i, j);
                if (!MergeDirection.NONE.equals(direction) && matchMultiCatch(catchClause1, catchClause2)) {
                    ASTRewrite rewrite= cuRewrite.getASTRewrite();
                    UnionType ut= unionTypes(catchClause1.getException().getType(),
                            catchClause2.getException().getType());
                    if (MergeDirection.UP.equals(direction)) {
                        rewrite.set(catchClause1.getException(), SingleVariableDeclaration.TYPE_PROPERTY, ut);
                        rewrite.remove(catchClause2);
                    } else if (MergeDirection.DOWN.equals(direction)) {
                        rewrite.remove(catchClause1);
                        rewrite.set(catchClause2.getException(), SingleVariableDeclaration.TYPE_PROPERTY, ut);
                    }

                    return false;
                }
            }
        }

        return true;
    }

    private Binding[] resolveTypeBindings(final List<CatchClause> catchClauses) {
        Binding[] results= new Binding[catchClauses.size()];

        for (int i= 0; i < catchClauses.size(); i++) {
            results[i]= resolveBinding(catchClauses.get(i));
        }

        return results;
    }

    private Binding resolveBinding(final CatchClause catchClause) {
        SingleVariableDeclaration svd= catchClause.getException();
        Type type= svd.getType();

        switch (type.getNodeType()) {
        case ASTNode.SIMPLE_TYPE:
            return new SingleBinding(type.resolveBinding());

        case ASTNode.UNION_TYPE:
            List<Type> types= ASTNodes.types((UnionType) type);
            ITypeBinding[] typeBindings= new ITypeBinding[types.size()];
            for (int j= 0; j < types.size(); j++) {
                typeBindings[j]= types.get(j).resolveBinding();
            }

            return new MultiBinding(typeBindings);

        default:
            // TODO JNR throw
            return null;
        }
    }

    private boolean matchMultiCatch(final CatchClause catchClause1, final CatchClause catchClause2) {
        MultiCatchASTMatcher matcher= new MultiCatchASTMatcher(catchClause1, catchClause2);
        return ASTNodes.match(matcher, catchClause1.getBody(), catchClause2.getBody());
    }

    private MergeDirection mergeDirection(final Binding[] typeBindings, final int start, final int end) {
        if (canMergeTypesDown(typeBindings, start, end)) {
            return MergeDirection.DOWN;
        }
        if (canMergeTypesUp(typeBindings, start, end)) {
            return MergeDirection.UP;
        }

        return MergeDirection.NONE;
    }

    private boolean canMergeTypesDown(final Binding[] types, final int start, final int end) {
        Binding startType= types[start];
        for (int i= start + 1; i < end; i++) {
            Binding type= types[i];
            if (Boolean.TRUE.equals(startType.isSubTypeCompatible(type))) {
                return false;
            }
        }

        return true;
    }

    private boolean canMergeTypesUp(final Binding[] types, final int start, final int end) {
        Binding endType= types[end];
        for (int i= start + 1; i < end; i++) {
            Binding type= types[i];
            if (Boolean.TRUE.equals(type.isSubTypeCompatible(endType))) {
                return false;
            }
        }

        return true;
    }

    private UnionType unionTypes(final Type... types) {
        List<Type> allTypes= new ArrayList<>();
        collectAllUnionedTypes(allTypes, Arrays.asList(types));
        removeSupersededAlternatives(allTypes);

        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        UnionType result= cuRewrite.getAST().newUnionType();
        List<Type> unionedTypes= ASTNodes.types(result);
        unionedTypes.addAll(rewrite.createMoveTarget(allTypes));
        return result;
    }

    private void collectAllUnionedTypes(final List<Type> results, final Collection<Type> types) {
        for (Type type : types) {
            if (type instanceof UnionType) {
                UnionType ut= (UnionType) type;
                collectAllUnionedTypes(results, ASTNodes.types(ut));
            } else {
                results.add(type);
            }
        }
    }

    private void removeSupersededAlternatives(final List<Type> allTypes) {
        for (ListIterator<Type> it1= allTypes.listIterator(); it1.hasNext();) {
            ITypeBinding binding1= it1.next().resolveBinding();

            for (ListIterator<Type> it2= allTypes.listIterator(it1.nextIndex()); it2.hasNext();) {
                ITypeBinding binding2= it2.next().resolveBinding();

                if (binding1 != null && binding1.isSubTypeCompatible(binding2)) {
                    it1.remove();
                    break;
                }
            }
        }
    }
}
