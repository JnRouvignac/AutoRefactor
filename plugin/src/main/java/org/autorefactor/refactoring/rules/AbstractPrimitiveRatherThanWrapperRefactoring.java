/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isField;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CAST_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.CONDITIONAL_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PARENTHESIZED_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.POSTFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PREFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.InterruptableVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public abstract class AbstractPrimitiveRatherThanWrapperRefactoring extends AbstractRefactoringRule {
    /**
     * Get the wrapper fully qualified name.
     *
     * @return the wrapper fully qualified name.
     */
    public abstract String getWrapperFullyQualifiedName();

    /**
     * Get the primitive type name.
     *
     * @return the primitive type name.
     */
    public abstract String getPrimitiveTypeName();

    /**
     * Get the literal class.
     *
     * @return the literal class.
     */
    public abstract Class<? extends Expression> getLiteralClass();

    /**
     * Get the prefix in safe operators.
     *
     * @return the prefix in safe operators.
     */
    public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
        return new ArrayList<PrefixExpression.Operator>(0);
    }

    /**
     * Get the Infix In Safe Operators.
     *
     * @return the Infix In Safe Operators.
     */
    public List<InfixExpression.Operator> getInfixInSafeOperators() {
        return new ArrayList<InfixExpression.Operator>(0);
    }

    /**
     * Get the postfix in safe operators.
     *
     * @return the postfix in safe operators.
     */
    public List<PostfixExpression.Operator> getPostfixInSafeOperators() {
        return new ArrayList<PostfixExpression.Operator>(0);
    }

    /**
     * Get the prefix out safe operators.
     *
     * @return the prefix out safe operators.
     */
    public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
        return new ArrayList<PrefixExpression.Operator>(0);
    }

    /**
     * Get the infix out safe operators.
     *
     * @return the infix out safe operators.
     */
    public List<InfixExpression.Operator> getInfixOutSafeOperators() {
        return new ArrayList<InfixExpression.Operator>(0);
    }

    /**
     * Get the postfix out safe operators.
     *
     * @return the postfix out safe operators.
     */
    public List<PostfixExpression.Operator> getPostfixOutSafeOperators() {
        return new ArrayList<PostfixExpression.Operator>(0);
    }

    /**
     * Get the assignment out safe operators.
     *
     * @return the assignment out safe operators.
     */
    public List<Assignment.Operator> getAssignmentOutSafeOperators() {
        return new ArrayList<Assignment.Operator>(0);
    }

    /**
     * Get the safe in constants.
     *
     * @return the safe in constants.
     */
    public String[] getSafeInConstants() {
        return new String[0];
    }

    /**
     * True if the specific primitive is allowed.
     *
     * @param node The node
     *
     * @return True if the specific primitive is allowed.
     */
    public boolean isSpecificPrimitiveAllowed(final ASTNode node) {
        return false;
    }

    @Override
    public boolean visit(VariableDeclarationStatement node) {
        if (node.fragments().size() == 1) {
            final VariableDeclarationFragment fragment = (VariableDeclarationFragment) node.fragments().get(0);
            if (hasType(fragment.resolveBinding().getType(), getWrapperFullyQualifiedName())
                    && fragment.getInitializer() != null) {
                if (isNotNull(fragment.getInitializer())) {
                    final VarOccurrenceVisitor varOccurrenceVisitor = new VarOccurrenceVisitor(fragment);
                    final Block parentBlock = getAncestorOrNull(fragment, Block.class);
                    if (parentBlock != null) {
                        varOccurrenceVisitor.visitNode(parentBlock);

                        if (varOccurrenceVisitor.isPrimitiveAllowed()
                                && varOccurrenceVisitor.getAutoBoxingCount() < 2) {
                            refactorWrapper(node);
                            return DO_NOT_VISIT_SUBTREE;
                        }
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private void refactorWrapper(final VariableDeclarationStatement node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Type primitiveType = b.type(getPrimitiveTypeName());
        ctx.getRefactorings().replace(node.getType(),
                primitiveType);
    }

    private boolean isNotNull(final Expression expr) {
        if (expr instanceof ParenthesizedExpression) {
            final ParenthesizedExpression parenthesizedExpr = (ParenthesizedExpression) expr;
            return isNotNull(parenthesizedExpr.getExpression());
        } else if (expr instanceof ConditionalExpression) {
            final ConditionalExpression prefixExpr = (ConditionalExpression) expr;
            return isNotNull(prefixExpr.getThenExpression()) && isNotNull(prefixExpr.getElseExpression());
        } else if (getLiteralClass().equals(expr.getClass())) {
            return true;
        } else if (expr instanceof QualifiedName) {
            final QualifiedName qualifiedName = (QualifiedName) expr;
            return hasType(qualifiedName.getQualifier(), getWrapperFullyQualifiedName())
                    && (isField(qualifiedName, getWrapperFullyQualifiedName(), getSafeInConstants())
                            || isField(qualifiedName, getPrimitiveTypeName(), getSafeInConstants()));
        } else if (expr instanceof InfixExpression) {
            final InfixExpression infixExpr = (InfixExpression) expr;
            return getInfixInSafeOperators()
                    .contains(infixExpr.getOperator());
        } else if (expr instanceof PrefixExpression) {
            final PrefixExpression prefixExpr = (PrefixExpression) expr;
            return getPrefixInSafeOperators()
                    .contains(prefixExpr.getOperator());
        } else if (expr instanceof PostfixExpression) {
            final PostfixExpression postfixExpr = (PostfixExpression) expr;
            return getPostfixInSafeOperators()
                    .contains(postfixExpr.getOperator());
        } else if (expr instanceof CastExpression) {
            final CastExpression castExpr = (CastExpression) expr;
            return hasType(castExpr.getType().resolveBinding(), getPrimitiveTypeName())
                    || (hasType(castExpr.getType().resolveBinding(), getWrapperFullyQualifiedName())
                            && isNotNull(castExpr.getExpression()));
        } else if (expr instanceof MethodInvocation) {
            final MethodInvocation mi = (MethodInvocation) expr;
            return isMethod(mi, getWrapperFullyQualifiedName(), "valueOf", getPrimitiveTypeName());
        }
        return false;
    }

    private class VarOccurrenceVisitor extends InterruptableVisitor {
        private final VariableDeclarationFragment varDecl;

        private boolean isPrimitiveAllowed = true;

        private boolean isVarReturned;

        private int autoBoxingCount = 0;

        public VarOccurrenceVisitor(final VariableDeclarationFragment var) {
            varDecl = var;
        }

        public boolean isPrimitiveAllowed() {
            return isPrimitiveAllowed;
        }

        public int getAutoBoxingCount() {
            return autoBoxingCount;
        }

        @Override
        public boolean visit(final SimpleName aVar) {
            if (isPrimitiveAllowed && aVar.getIdentifier().equals(varDecl.getName().getIdentifier())
                    && !aVar.getParent().equals(varDecl)) {
                isPrimitiveAllowed = isPrimitiveAllowed(aVar);
                if (!isPrimitiveAllowed) {
                    return interruptVisit();
                }
            }
            return VISIT_SUBTREE;
        }

        private boolean isPrimitiveAllowed(final ASTNode node) {
            final ASTNode parentNode = node.getParent();

            switch (parentNode.getNodeType()) {
            case PARENTHESIZED_EXPRESSION:
                return isPrimitiveAllowed(parentNode);

            case CAST_EXPRESSION:
                final CastExpression castExpr = (CastExpression) parentNode;
                return hasType(castExpr.getType().resolveBinding(), getPrimitiveTypeName());

            case ASSIGNMENT:
                final Assignment assignment = (Assignment) parentNode;
                if (getAssignmentOutSafeOperators().contains(assignment.getOperator())) {
                    return true;
                } else if (assignment.getLeftHandSide().equals(node)) {
                    return isNotNull(assignment.getRightHandSide());
                } else if (assignment.getRightHandSide().equals(node)) {
                    if (assignment.getLeftHandSide() instanceof Name) {
                        return isOfType((Name) assignment.getLeftHandSide());
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }

            case VARIABLE_DECLARATION_FRAGMENT:
                final VariableDeclarationFragment fragment = (VariableDeclarationFragment) parentNode;
                if (fragment.getInitializer().equals(node)) {
                    return isOfType(fragment.getName());
                } else {
                    return false;
                }

            case RETURN_STATEMENT:
                final ReturnStatement returnStmt = (ReturnStatement) parentNode;
                if (returnStmt.getExpression().equals(node)) {
                    final MethodDeclaration method = getAncestorOrNull(returnStmt, MethodDeclaration.class);
                    if (method != null && method.getReturnType2() != null) {
                        if (hasType(method.getReturnType2().resolveBinding(), getPrimitiveTypeName())) {
                            return true;
                        } else if (hasType(method.getReturnType2().resolveBinding(), getWrapperFullyQualifiedName())) {
                            if (!isVarReturned) {
                                isVarReturned = true;
                                autoBoxingCount++;
                            }
                            return true;
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }

            case CONDITIONAL_EXPRESSION:
                final ConditionalExpression conditionalExpr = (ConditionalExpression) parentNode;
                return conditionalExpr.getExpression().equals(node);

            case PREFIX_EXPRESSION:
                return getPrefixOutSafeOperators()
                        .contains(((PrefixExpression) parentNode).getOperator());

            case INFIX_EXPRESSION:
                return getInfixOutSafeOperators()
                        .contains(((InfixExpression) parentNode).getOperator());

            case POSTFIX_EXPRESSION:
                return getPostfixOutSafeOperators()
                        .contains(((PostfixExpression) parentNode).getOperator());

            default:
                return isSpecificPrimitiveAllowed(node);
            }
        }

        private boolean isOfType(final Name name) {
            if (hasType(name.resolveTypeBinding(), getPrimitiveTypeName())) {
                return true;
            } else if (hasType(name.resolveTypeBinding(), getWrapperFullyQualifiedName())) {
                autoBoxingCount++;
                return true;
            } else {
                return false;
            }
        }
    }
}
