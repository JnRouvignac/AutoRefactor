/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.UnionType;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class UseMultiCatchRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Refactors catch clauses with the same body to use Java 7's multi-catch.";
    }

    @Override
    public String getName() {
        return "Multi-catch";
    }

    private static enum AggregateDirection {
        NONE, FORWARD, BACKWARD;
    }

    private static final class MultiCatchASTMatcher extends ASTMatcher {
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
                return super.match(mi1, mi2) && isSameMethod(mi1, mi2);
            }
            return false;
        }

        private boolean isSameMethod(MethodInvocation mi1, MethodInvocation mi2) {
            IMethodBinding binding1 = mi1.resolveMethodBinding();
            IMethodBinding binding2 = mi2.resolveMethodBinding();
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
        for (int i = 0; i < catchClauses.size(); i++) {
            CatchClause catchClause1 = catchClauses.get(i);
            for (int j = i + 1; j < catchClauses.size(); j++) {
                CatchClause catchClause2 = catchClauses.get(j);
                AggregateDirection direction = aggregateDirection(catchClauses, i, j);
                if (!AggregateDirection.NONE.equals(direction)
                        && matchMultiCatch(catchClause1, catchClause2)) {
                    Refactorings r = this.ctx.getRefactorings();
                    UnionType ut = concat(catchClause1.getException().getType(), catchClause2.getException().getType());
                    if (AggregateDirection.BACKWARD.equals(direction)) {
                        r.remove(catchClause1);
                        r.set(catchClause2.getException(), SingleVariableDeclaration.TYPE_PROPERTY, ut);
                    } else if (AggregateDirection.FORWARD.equals(direction)) {
                        r.set(catchClause1.getException(), SingleVariableDeclaration.TYPE_PROPERTY, ut);
                        r.remove(catchClause2);
                    }
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean matchMultiCatch(CatchClause catchClause1, CatchClause catchClause2) {
        final MultiCatchASTMatcher matcher = new MultiCatchASTMatcher(catchClause1, catchClause2);
        return match(matcher, catchClause1.getBody(), catchClause2.getBody());
    }

    private AggregateDirection aggregateDirection(List<CatchClause> catchClauses, int start, int end) {
        final ITypeBinding[] types = new ITypeBinding[catchClauses.size()];
        for (int i = start; i <= end; i++) {
            types[i] = resolveTypeBindingOfException(catchClauses.get(i));
            if (types[i] == null) {
                return AggregateDirection.NONE;
            }
        }

        if (changeInBehaviour(types, start, end)) {
            return AggregateDirection.NONE;
        } else if (canRefactorBackward(types, start, end)) {
            return AggregateDirection.BACKWARD;
        } else if (canRefactorForward(types, start, end)) {
            return AggregateDirection.FORWARD;
        } else {
            return AggregateDirection.NONE;
        }
    }

    private ITypeBinding resolveTypeBindingOfException(CatchClause catchClause) {
        SingleVariableDeclaration svd = catchClause.getException();
        IVariableBinding vb = svd.resolveBinding();
        if (vb != null) {
            return vb.getType();
        }
        return null;
    }

    private boolean changeInBehaviour(ITypeBinding[] types, int start, int end) {
        ITypeBinding startType = types[start];
        ITypeBinding endType = types[end];
        for (int i = start + 1; i < end; i++) {
            final ITypeBinding type = types[i];
            if (type != null
                    && startType.isSubTypeCompatible(type)
                    && type.isSubTypeCompatible(endType)) {
                return true;
            }
        }
        return false;
    }

    private boolean canRefactorBackward(final ITypeBinding[] types, int start, int end) {
        final ITypeBinding startType = types[start];
        for (int i = start + 1; i < end; i++) {
            final ITypeBinding type = types[i];
            if (startType.isSubTypeCompatible(type)) {
                return false;
            }
        }
        return true;
    }

    private boolean canRefactorForward(final ITypeBinding[] types, int start, int end) {
        final ITypeBinding endType = types[end];
        for (int i = start + 1; i < end; i++) {
            final ITypeBinding type = types[i];
            if (endType.isSubTypeCompatible(type)) {
                return false;
            }
        }
        return true;
    }

    private UnionType concat(Type... types) {
        final List<Type> allTypes = new ArrayList<Type>();
        collectAllTypes(allTypes, Arrays.asList(types));
        removeSupersededAlternatives(allTypes);

        final ASTBuilder b = this.ctx.getASTBuilder();
        final UnionType result = this.ctx.getAST().newUnionType();
        final List<Type> unionedTypes = types(result);
        for (Type unionedType : allTypes) {
            unionedTypes.add(b.copy(unionedType));
        }
        return result;
    }

    private void collectAllTypes(List<Type> results, Collection<Type> types) {
        for (final Type type : types) {
            if (type instanceof UnionType) {
                final UnionType ut = (UnionType) type;
                collectAllTypes(results, types(ut));
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
