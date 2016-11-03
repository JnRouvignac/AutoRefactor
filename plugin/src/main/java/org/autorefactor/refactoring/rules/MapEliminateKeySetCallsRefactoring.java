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
package org.autorefactor.refactoring.rules;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.CollectorVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.TypeNameDecider;
import org.autorefactor.util.IllegalStateException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.Initializer;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;
import static org.eclipse.jdt.core.dom.EnhancedForStatement.*;

/** See {@link #getDescription()} method. */
public class MapEliminateKeySetCallsRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
                + "Directly invoke methods on Map rather than on Map.keySet() where possible,\n"
                + "also convert for loops iterating on Map.keySet() to iterate on Map.entrySet() where possible.";
    }

    @Override
    public String getName() {
        return "Replace useless calls to Map.keySet() when direct calls to the Map are possible";
    }

    /**
     * This class helps decide which name to give to a new variable.
     * <p>
     * When creating a new variable, its name may shadow another variable or field used in the local
     * scope.
     * <p>
     * Does JDT provide a public API for naming local variables? I could not find any.
     */
    private static final class VariableNameDecider {
        private final ASTNode scope;
        private final int insertionPoint;
        private final ASTNode namingScope;

        private VariableNameDecider(ASTNode scope, int insertionPoint) {
            this.scope = scope;
            this.insertionPoint = insertionPoint;
            this.namingScope = getNamingScope(scope);
        }

        private ASTNode getNamingScope(ASTNode scope) {
            Class<?>[] ancestorClasses = new Class<?>[] { MethodDeclaration.class, Initializer.class };
            ASTNode ancestor = getFirstAncestorOrNull(scope, ancestorClasses);
            if (ancestor == null) {
                throw new IllegalStateException(scope, "Expected to find an ancestor among the types "
                    + Arrays.toString(ancestorClasses) + " but could not find any");
            }
            return ancestor;
        }

        /**
         * Returns a name suggestion suitable for use when inserting a new variable declaration. This
         * name:
         * <ul>
         * <li>will not shadow any variable name in use after the insertion point</li>
         * <li>and will not conflict with local variables declared before the insertion point.</li>
         * </ul>
         *
         * @param candidateNames
         *          the suggestion will be one of the candidate names, maybe suffixed by a number
         * @return the suggestion for a variable name
         */
        public String suggest(String... candidateNames) {
            final Set<String> declaredLocalVarNames = new HashSet<String>(collectDeclaredLocalVariableNames());
            final Set<String> varNamesUsedAfter = new HashSet<String>(collectVariableNamesUsedAfter());
            int i = 0;
            while (true) {
                for (String candidateName : candidateNames) {
                    final String candidate = i == 0 ? candidateName : candidateName + i;
                    if (// no variable declaration conflict
                        !declaredLocalVarNames.contains(candidate)
                        // new variable does not shadow use of other variables/fields with the same name
                            && !varNamesUsedAfter.contains(candidate)) {
                        return candidate;
                    }
                }
                i++;
            }
        }

        private Collection<String> collectDeclaredLocalVariableNames() {
            return new CollectorVisitor<String>() {
                @Override
                public boolean preVisit2(ASTNode node) {
                    return !isTypeDeclaration(node);
                }

                @Override
                public boolean visit(SimpleName node) {
                    final IBinding binding = node.resolveBinding();
                    if (binding.getKind() == IBinding.VARIABLE) {
                        addResult(((IVariableBinding) binding).getName());
                    }
                    return VISIT_SUBTREE;
                }
            } .collect(namingScope);
        }

        private List<String> collectVariableNamesUsedAfter() {
            return new CollectorVisitor<String>() {
                @Override
                public boolean preVisit2(ASTNode node) {
                    return node.getStartPosition() > insertionPoint
                            && !isTypeDeclaration(node);
                }

                @Override
                public boolean visit(SimpleName node) {
                    final IBinding binding = node.resolveBinding();
                    if (binding.getKind() == IBinding.VARIABLE) {
                        addResult(((IVariableBinding) binding).getName());
                    }
                    return VISIT_SUBTREE;
                }
            } .collect(scope);
        }

        private boolean isTypeDeclaration(ASTNode node) {
            switch (node.getNodeType()) {
            case ANNOTATION_TYPE_DECLARATION:
            case ANONYMOUS_CLASS_DECLARATION:
            case ENUM_DECLARATION:
            case TYPE_DECLARATION:
                return true;

            default:
                return false;
            }
        }
    }

    @Override
    public boolean visit(MethodInvocation mi) {
        Expression miExpr = mi.getExpression();
        if (isKeySetMethod(miExpr)) {
            MethodInvocation mapKeySetMi = (MethodInvocation) miExpr;
            if (isMethod(mi, "java.util.Set", "clear")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "clear");
            }
            if (isMethod(mi, "java.util.Set", "size")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "size");
            }
            if (isMethod(mi, "java.util.Set", "isEmpty")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "isEmpty");
            }
            if (isMethod(mi, "java.util.Set", "remove", "java.lang.Object")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "remove");
            }
            if (isMethod(mi, "java.util.Set", "contains", "java.lang.Object")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "containsKey");
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean removeInvocationOfMapKeySet(
            MethodInvocation mapKeySetMi, MethodInvocation actualMi, String methodName) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(
            actualMi,
            b.invoke(
                b.copyExpression(mapKeySetMi),
                methodName,
                b.copyRange(arguments(actualMi))));
        return DO_NOT_VISIT_SUBTREE;
    }

    @Override
    public boolean visit(EnhancedForStatement enhancedFor) {
        final Expression foreachExpr = enhancedFor.getExpression();
        if (isKeySetMethod(foreachExpr)) {
            // From 'for (K key : map.keySet()) { }'
            // -> mapExpression become 'map', parameter become 'K key'
            final Expression mapExpression = ((MethodInvocation) foreachExpr).getExpression();
            if (mapExpression == null) {
                // not implemented
                return VISIT_SUBTREE;
            }
            final SingleVariableDeclaration parameter = enhancedFor.getParameter();
            final List<MethodInvocation> getValueMis =
                    collectMapGetValueCalls(mapExpression, parameter, enhancedFor.getBody());
            if (!getValueMis.isEmpty() && haveSameTypeBindings(getValueMis)) {
                final ASTBuilder b = ctx.getASTBuilder();
                final Refactorings r = ctx.getRefactorings();

                final int insertionPoint = asList(enhancedFor.getBody()).get(0).getStartPosition() - 1;
                final String entryVarName =
                        new VariableNameDecider(enhancedFor.getBody(), insertionPoint).suggest("entry", "mapEntry");

                // for (K key : map.keySet()) => for (K key : map.entrySet())
                r.set(enhancedFor, EXPRESSION_PROPERTY, b.invoke(b.move(mapExpression), "entrySet"));
                // for (K key : map.entrySet()) => for (Map.Entry<K, V> mapEntry : map.entrySet())
                final TypeNameDecider typeNameDecider = new TypeNameDecider(parameter);
                final Type mapEntryType = createMapEntryType(parameter, getValueMis.get(0), typeNameDecider);
                r.set(enhancedFor, PARAMETER_PROPERTY, b.declareSingleVariable(entryVarName, mapEntryType));
                // for (Map.Entry<K, V> mapEntry : map.entrySet()) {
                //     K key = mapEntry.getKey(); // <--- add this statement
                final Type mapKeyType = b.copy(parameter.getType());
                final VariableDeclarationStatement newKeyDeclaration = b.declareStmt(
                        mapKeyType,
                        b.move(parameter.getName()),
                        b.invoke(b.name(entryVarName), "getKey"));
                r.insertFirst(enhancedFor.getBody(), Block.STATEMENTS_PROPERTY, newKeyDeclaration);

                // Replace all occurrences of map.get(key) => mapEntry.getValue()
                for (MethodInvocation getValueMi : getValueMis) {
                    r.replace(getValueMi, b.invoke(b.name(entryVarName), "getValue"));
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    /**
     * If possible, use the type declaration, so we can return the type as it was declared.
     * Otherwise, let's use the type binding and output verbose fully qualified types.
     */
    private Type createMapEntryType(
            SingleVariableDeclaration parameter, Expression getValueMi, TypeNameDecider typeNameDecider) {
        final String mapEntryType = typeNameDecider.useSimplestPossibleName("java.util.Map.Entry");

        final ASTBuilder b = ctx.getASTBuilder();
        final Type mapKeyType = b.move(parameter.getType());
        final Type mapValueType = b.copyType(getValueMi, typeNameDecider);
        return b.genericType(mapEntryType, mapKeyType, mapValueType);
    }

    private boolean isKeySetMethod(Expression expr) {
        return expr instanceof MethodInvocation
            && isMethod((MethodInvocation) expr, "java.util.Map", "keySet");
    }

    private List<MethodInvocation> collectMapGetValueCalls(
            Expression mapExpression, SingleVariableDeclaration parameter, Statement body) {
        return new CollectMapGetCalls(mapExpression, parameter).collect(body);
    }

    /** Sanity check. */
    private boolean haveSameTypeBindings(Collection<? extends Expression> exprs) {
        Iterator<? extends Expression> it = exprs.iterator();
        if (!it.hasNext()) {
            // Not really expected
            return false;
        }
        final ITypeBinding type0 = it.next().resolveTypeBinding();
        if (type0 == null) {
            return false;
        }
        for (; it.hasNext();) {
            final ITypeBinding typeN = it.next().resolveTypeBinding();
            if (!type0.equals(typeN)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Class to find {@code map.get(loopVariable)} constructs in the AST tree,
     * and collect the type of the value, which is unknown until one is located.
     */
    class CollectMapGetCalls extends CollectorVisitor<MethodInvocation> {
        private final Expression mapExpression;
        private final SingleVariableDeclaration forEachParameter;

        public CollectMapGetCalls(Expression mapExpression, SingleVariableDeclaration forEachParameter) {
            this.mapExpression = mapExpression;
            this.forEachParameter = forEachParameter;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (isSameReference(node.getExpression(), mapExpression)
                    && isMethod(node, "java.util.Map", "get", "java.util.Object")
                    && isSameVariable(arg0(node), forEachParameter.getName())) {
                addResult(node);
            }
            return VISIT_SUBTREE;
        }

        private boolean isSameReference(Expression expr1, Expression expr2) {
            if (expr1 == null || expr2 == null) {
                return false;
            }
            switch (expr1.getNodeType()) {
            case METHOD_INVOCATION:
                switch (expr2.getNodeType()) {
                case METHOD_INVOCATION:
                    final MethodInvocation mi1 = (MethodInvocation) expr1;
                    final MethodInvocation mi2 = (MethodInvocation) expr2;
                    return areBindingsEqual(mi1.resolveTypeBinding(), mi2.resolveTypeBinding())
                            && isSameReference(mi1.getExpression(), mi2.getExpression());

                default:
                    return isSameVariable(expr1, expr2);
                }

            default:
                return isSameVariable(expr1, expr2);
            }
        }
    }
}
