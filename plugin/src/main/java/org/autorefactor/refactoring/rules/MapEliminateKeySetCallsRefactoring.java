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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class MapEliminateKeySetCallsRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
                + "Directly invoke methods on Map rather than on Map.keySet() where possible\n";
    }

    @Override
    public String getName() {
        return "Replace useless calls to Map.keySet() when direct calls to the Map are possible";
    }

    @Override
    public boolean visit(MethodInvocation mi) {
        Expression miExpr = mi.getExpression();
        if (isKeySetMethod(miExpr)) {
            MethodInvocation parentMi = (MethodInvocation) miExpr;
            if (isMethod(mi, "java.util.Set", "clear")) {
                removeInvocationOfMapKeySet(parentMi, mi, "clear");
                return DO_NOT_VISIT_SUBTREE;
            }
            if (isMethod(mi, "java.util.Set", "size")) {
                removeInvocationOfMapKeySet(parentMi, mi, "size");
                return DO_NOT_VISIT_SUBTREE;
            }
            if (isMethod(mi, "java.util.Set", "isEmpty")) {
                removeInvocationOfMapKeySet(parentMi, mi, "isEmpty");
                return DO_NOT_VISIT_SUBTREE;
            }
            if (isMethod(mi, "java.util.Set", "remove", "java.lang.Object")) {
                removeInvocationOfMapKeySet(parentMi, mi, "remove");
                return DO_NOT_VISIT_SUBTREE;
            }
            if (isMethod(mi, "java.util.Set", "contains", "java.lang.Object")) {
                removeInvocationOfMapKeySet(parentMi, mi, "containsKey");
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean isKeySetMethod(Expression expr) {
        if (expr instanceof MethodInvocation) {
            return isMethod((MethodInvocation) expr, "java.util.Map", "keySet");
        }
        return false;
    }

    @Override
    public boolean visit(EnhancedForStatement enhancedFor) {
        if (isKeySetMethod(enhancedFor.getExpression())) {
            // From 'for (K key : map.keySet()) { }'
            // -> mapExpression become 'map', parameter become 'K key'
            final Expression mapExpression = ((MethodInvocation) enhancedFor.getExpression()).getExpression();
            final SingleVariableDeclaration parameter = enhancedFor.getParameter();

            final ITypeBinding valueType = findAndRefactor(mapExpression, parameter, enhancedFor.getBody());
            if (valueType != null) {

                final ASTBuilder b = ctx.getASTBuilder();
                final AST ast = ctx.getAST();
                final EnhancedForStatement newFor = ast.newEnhancedForStatement();
                newFor.setExpression(b.invoke(b.copy(mapExpression), "entrySet"));
                newFor.setParameter(b.declareSingleVariable(b.simpleName("entry"), "Map.Entry",
                        getFriendlyTypeName(parameter.getType().resolveBinding()), getFriendlyTypeName(valueType)));

                final Statement newBody = b.copy(enhancedFor.getBody());

                // Declare 'KeyType' 'oldLoopVariable' = entry.getKey();
                final VariableDeclarationStatement newKeyDeclaration = b.declareStmt(
                        getFriendlyTypeName(parameter.getType().resolveBinding()),
                        b.copy(parameter.getName()),
                        b.invoke(b.name("entry"), "getKey"));

                newFor.setBody(newBody);
                ctx.getRefactorings().insertBefore(newKeyDeclaration,
                        (ASTNode) ((Block) enhancedFor.getBody()).statements().get(0));
                ctx.getRefactorings().replace(enhancedFor, newFor);
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    /**
     * @param typeBinding
     * @return a short name, for classes in the java. packages, otherwise a fully qualified name.
     */
    private String getFriendlyTypeName(ITypeBinding typeBinding) {
        final String name = typeBinding.getQualifiedName();
        if (name.startsWith("java.")) {
            return typeBinding.getName();
        } else {
            // TODO: fix to import the necessary type, if not found
            return name;
        }
    }

    /**
     * Class to find map.get(loopVariable) construct in the AST tree, and collect the type of the value,
     * which is unknown till one is located.
     *
     */
    class FindExpression extends ASTVisitor {

        private final Expression mapExpression;
        private final SingleVariableDeclaration parameter;
        private ITypeBinding valueType;

        public FindExpression(Expression mapExpression, SingleVariableDeclaration parameter) {
            this.mapExpression = mapExpression;
            this.parameter = parameter;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (ASTHelper.isSameVariable(node.getExpression(), mapExpression)) {
                if (isMethod(node, "java.util.Map", "get", "java.util.Object")) {
                    final ASTNode getArgument = (ASTNode) node.arguments().get(0);
                    if (ASTHelper.isSameVariable(getArgument, parameter.getName())) {

                        // collect the value type
                        valueType = node.resolveTypeBinding();
                        final ASTBuilder b = ctx.getASTBuilder();
                        ctx.getRefactorings().replace(node, b.invoke(b.name("entry"), "getValue"));
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
            return VISIT_SUBTREE;
        }

        public ITypeBinding getValueType() {
            return valueType;
        }
    }

    private ITypeBinding findAndRefactor(Expression mapExpression, SingleVariableDeclaration parameter,
            Statement body) {
        final FindExpression finder = new FindExpression(mapExpression, parameter);
        body.accept(finder);
        return finder.getValueType();
    }

    private void removeInvocationOfMapKeySet(MethodInvocation mapKeySetMi, MethodInvocation actualMi,
            String methodName) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(actualMi, b.invoke(b.copyExpression(mapKeySetMi), methodName,
                b.copyRange(arguments(actualMi))));
    }
}
