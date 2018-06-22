/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
 * Copyright (C) 2018 Jean-Noël Rouvignac - fix NPE
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

import static org.autorefactor.refactoring.ASTHelper.*;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.BlockSubVisitor;
import org.autorefactor.refactoring.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CreationReference;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionMethodReference;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SuperMethodReference;
import org.eclipse.jdt.core.dom.TypeMethodReference;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class LambdaRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Improve lambda expressions";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Improve lambda expressions.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(Block node) {
        if (getJavaMinorVersion() >= 8) {
            final LambdaExprVisitor lambdaExprVisitor = new LambdaExprVisitor(node);
            node.accept(lambdaExprVisitor);
            return lambdaExprVisitor.getResult();
        }
        return VISIT_SUBTREE;
    }

    private final class LambdaExprVisitor extends BlockSubVisitor {

        public LambdaExprVisitor(final Block startNode) {
            super(null, startNode);
        }

        @Override
        public boolean visit(LambdaExpression node) {
            if (node.hasParentheses() && node.parameters().size() == 1) {
                removeParamParentheses(node);
                return DO_NOT_VISIT_SUBTREE;
            } else if (node.getBody() instanceof Block) {
                final List<Statement> stmts = asList((Block) node.getBody());

                if (stmts.size() == 1 && stmts.get(0) instanceof ReturnStatement) {
                    removeReturnAndBrackets(node, stmts);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else if (node.getBody() instanceof ClassInstanceCreation) {
                final ClassInstanceCreation ci = (ClassInstanceCreation) node.getBody();

                final List<Expression> arguments = arguments(ci);
                if (node.parameters().size() == arguments.size()
                        && isSameIdentifier(node, arguments)) {
                    replaceByCreationReference(node, ci);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else if (node.getBody() instanceof SuperMethodInvocation) {
                final SuperMethodInvocation smi = (SuperMethodInvocation) node.getBody();

                final List<Expression> arguments = arguments(smi);
                if (node.parameters().size() == arguments.size()
                        && isSameIdentifier(node, arguments)) {
                    replaceBySuperMethodReference(node, smi);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else if (node.getBody() instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) node.getBody();
                final Expression miExpr = mi.getExpression();

                final List<Expression> arguments = arguments(mi);
                if (node.parameters().size() == arguments.size()) {
                    if (!isSameIdentifier(node, arguments)) {
                        return VISIT_SUBTREE;
                    }

                    if (miExpr instanceof SimpleName
                            && ((SimpleName) miExpr).resolveBinding().getKind() == IBinding.TYPE
                                    && !arguments.isEmpty()
                                    && haveSameType(arguments.get(0), miExpr)) {

                        final String[] remainingParams = new String[arguments.size() - 1];
                        for (int i = 0; i < arguments.size() - 1; i++) {
                            remainingParams[i] =
                                    arguments.get(i + 1).resolveTypeBinding().getQualifiedName();
                        }

                        final ITypeBinding clazz = miExpr.resolveTypeBinding();
                        for (final IMethodBinding methodBinding : clazz.getDeclaredMethods()) {
                            if ((methodBinding.getModifiers() & Modifier.STATIC) == 0
                                    && isMethod(methodBinding,
                                            clazz.getQualifiedName(),
                                            mi.getName().getIdentifier(), remainingParams)) {
                                return VISIT_SUBTREE;
                            }
                        }
                    }

                    replaceByMethodReference(node, mi);
                    return DO_NOT_VISIT_SUBTREE;
                } else if (miExpr instanceof SimpleName
                        && node.parameters().size() == arguments.size() + 1) {
                    final SimpleName calledObject = (SimpleName) miExpr;
                    if (isSameIdentifier(node, 0, calledObject)) {
                        for (int i = 0; i < arguments.size(); i++) {
                            final ASTNode expr = removeParentheses(arguments.get(i));
                            if (!(expr instanceof SimpleName)
                                    || !isSameIdentifier(node, i + 1, (SimpleName) expr)) {
                                return VISIT_SUBTREE;
                            }
                        }

                        final ITypeBinding clazz = miExpr.resolveTypeBinding();
                        final String[] remainingParams = new String[arguments.size() + 1];
                        remainingParams[0] = clazz.getQualifiedName();
                        for (int i = 0; i < arguments.size(); i++) {
                            remainingParams[i + 1] =
                                    arguments.get(i).resolveTypeBinding().getQualifiedName();
                        }

                        for (IMethodBinding methodBinding : clazz.getDeclaredMethods()) {
                            if ((methodBinding.getModifiers() & Modifier.STATIC) > 0
                                    && isMethod(methodBinding,
                                            clazz.getQualifiedName(),
                                            mi.getName().getIdentifier(), remainingParams)) {
                                return VISIT_SUBTREE;
                            }
                        }

                        replaceByTypeReference(node, mi);
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
            return VISIT_SUBTREE;
        }

        private boolean isSameIdentifier(LambdaExpression node, List<Expression> arguments) {
            for (int i = 0; i < node.parameters().size(); i++) {
                if (!isSameIdentifier(node, arguments, i)) {
                    return false;
                }
            }
            return true;
        }

        private boolean isSameIdentifier(final LambdaExpression node, final List<Expression> arguments, final int i) {
            final Expression expr = removeParentheses(arguments.get(i));
            return expr instanceof SimpleName && isSameIdentifier(node, i, (SimpleName) expr);
        }

        private boolean isSameIdentifier(final LambdaExpression node, final int i, final SimpleName argument) {
            final VariableDeclarationFragment vdf = (VariableDeclarationFragment) node.parameters().get(i);
            return vdf.getName().getIdentifier().equals(argument.getIdentifier());
        }

        @SuppressWarnings("unchecked")
        private void removeParamParentheses(final LambdaExpression node) {
            final ASTBuilder b = LambdaRefactoring.this.ctx.getASTBuilder();

            final LambdaExpression copyOfLambdaExpr = b.lambda();
            final ASTNode copyOfParameter = b.copy((ASTNode) node.parameters().get(0));
            copyOfLambdaExpr.parameters().add(copyOfParameter);
            copyOfLambdaExpr.setBody(b.copy(node.getBody()));
            copyOfLambdaExpr.setParentheses(false);
            LambdaRefactoring.this.ctx.getRefactorings().replace(node, copyOfLambdaExpr);
        }

        private void removeReturnAndBrackets(final LambdaExpression node, final List<Statement> stmts) {
            final ASTBuilder b = LambdaRefactoring.this.ctx.getASTBuilder();

            final ReturnStatement returnStmt = (ReturnStatement) stmts.get(0);
            LambdaRefactoring.this.ctx.getRefactorings().replace(node.getBody(),
                    b.parenthesizeIfNeeded(b.copy(returnStmt.getExpression())));
        }

        private void replaceByCreationReference(final LambdaExpression node, final ClassInstanceCreation ci) {
            final ASTBuilder b = LambdaRefactoring.this.ctx.getASTBuilder();

            final TypeNameDecider typeNameDecider = new TypeNameDecider(ci);

            final CreationReference creationRef = b.creationRef();
            creationRef.setType(b.toType(ci.resolveTypeBinding().getErasure(), typeNameDecider));
            LambdaRefactoring.this.ctx.getRefactorings().replace(node, creationRef);
        }

        private void replaceBySuperMethodReference(final LambdaExpression node, final SuperMethodInvocation ci) {
            final ASTBuilder b = LambdaRefactoring.this.ctx.getASTBuilder();

            final SuperMethodReference creationRef = b.superMethodRef();
            creationRef.setName(b.copy(ci.getName()));
            LambdaRefactoring.this.ctx.getRefactorings().replace(node, creationRef);
        }

        private void replaceByTypeReference(final LambdaExpression node, final MethodInvocation mi) {
            final ASTBuilder b = LambdaRefactoring.this.ctx.getASTBuilder();

            final TypeNameDecider typeNameDecider = new TypeNameDecider(mi);

            final TypeMethodReference typeMethodRef = b.typeMethodRef();
            typeMethodRef.setType(b.toType(mi.getExpression().resolveTypeBinding().getErasure(), typeNameDecider));
            typeMethodRef.setName(b.copy(mi.getName()));
            LambdaRefactoring.this.ctx.getRefactorings().replace(node, typeMethodRef);
        }

        private void replaceByMethodReference(final LambdaExpression node, final MethodInvocation mi) {
            final ASTBuilder b = LambdaRefactoring.this.ctx.getASTBuilder();

            final ExpressionMethodReference typeMethodRef = b.exprMethodRef();
            if (mi.getExpression() != null) {
                typeMethodRef.setExpression(b.copy(mi.getExpression()));
            } else {
                typeMethodRef.setExpression(b.this0());
            }
            typeMethodRef.setName(b.copy(mi.getName()));
            LambdaRefactoring.this.ctx.getRefactorings().replace(node, typeMethodRef);
        }
    }
}
