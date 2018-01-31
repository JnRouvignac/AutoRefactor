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
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CONDITIONAL_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.DO_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.PREFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;
import static org.eclipse.jdt.core.dom.ASTNode.WHILE_STATEMENT;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.XOR;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;
import static org.eclipse.jdt.core.dom.Assignment.Operator.BIT_AND_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.BIT_OR_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.BIT_XOR_ASSIGN;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class PrimitiveRatherThanBooleanWrapperRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Replace Boolean wrapper object by boolean primitive type when an object is not necessary.";
    }

    @Override
    public String getName() {
        return "Primitive rather than Boolean wrapper";
    }

    @Override
    public boolean visit(VariableDeclarationStatement node) {
        if (node.fragments().size() == 1) {
            final VariableDeclarationFragment fragment = (VariableDeclarationFragment) node.fragments().get(0);
            if (hasType(fragment.resolveBinding().getType(), "java.lang.Boolean")
                    && fragment.getInitializer() != null) {
                if (isNotNull(fragment.getInitializer())) {
                    final VarOccurrenceVisitor varOccurrenceVisitor = new VarOccurrenceVisitor(fragment);
                    final Block parentBlock = getAncestorOrNull(fragment, Block.class);
                    if (parentBlock != null) {
                        parentBlock.accept(varOccurrenceVisitor);

                        if (varOccurrenceVisitor.canBeRefactored() && varOccurrenceVisitor.getAutoBoxingCount() < 2) {
                            refactorBoolean(node);
                            return DO_NOT_VISIT_SUBTREE;
                        }
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private void refactorBoolean(final VariableDeclarationStatement node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Type booleanPrimitiveType = b.type("boolean");
        ctx.getRefactorings().replace(node.getType(),
                booleanPrimitiveType);
    }

    private boolean isNotNull(final Expression expr) {
        final Object constantCondition =
                expr.resolveConstantExpressionValue();
        if (constantCondition != null) {
            return Boolean.TRUE.equals(constantCondition)
                    || Boolean.FALSE.equals(constantCondition);
        } else if (expr instanceof ParenthesizedExpression) {
            final ParenthesizedExpression parenthesizedExpr = (ParenthesizedExpression) expr;
            return isNotNull(parenthesizedExpr.getExpression());
        } else if (expr instanceof ConditionalExpression) {
            final ConditionalExpression prefixExpr = (ConditionalExpression) expr;
            return isNotNull(prefixExpr.getThenExpression()) && isNotNull(prefixExpr.getElseExpression());
        } else if (expr instanceof InfixExpression) {
            return true;
        } else if (expr instanceof PrefixExpression) {
            final PrefixExpression prefixExpr = (PrefixExpression) expr;
            return NOT.equals(prefixExpr.getOperator());
        } else if (expr instanceof CastExpression) {
            final CastExpression castExpr = (CastExpression) expr;
            return hasType(castExpr.getType().resolveBinding(), "boolean");
        } else if (expr instanceof QualifiedName) {
            final QualifiedName qualifiedName = (QualifiedName) expr;
            return isField(qualifiedName, "java.lang.Boolean", "TRUE")
                    || isField(qualifiedName, "java.lang.Boolean", "FALSE");
        }
        return false;
    }

    private class VarOccurrenceVisitor extends ASTVisitor {
        private final VariableDeclarationFragment varDecl;

        private boolean canBeRefactored = true;

        private boolean isVarReturned;

        private int autoBoxingCount = 0;

        public VarOccurrenceVisitor(final VariableDeclarationFragment var) {
            varDecl = var;
        }

        public boolean canBeRefactored() {
            return canBeRefactored;
        }

        public int getAutoBoxingCount() {
            return autoBoxingCount;
        }

        @Override
        public boolean visit(final SimpleName aVar) {
            if (aVar.getIdentifier().equals(varDecl.getName().getIdentifier())
                    && !aVar.getParent().equals(varDecl)) {
                canBeRefactored = canInstantiationBeRefactored(aVar);
                if (!canBeRefactored) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
            return VISIT_SUBTREE;
        }

        private boolean canInstantiationBeRefactored(final ASTNode node) {
            final ASTNode parentNode = node.getParent();

            switch (parentNode.getNodeType()) {
            case IF_STATEMENT:
            case WHILE_STATEMENT:
            case DO_STATEMENT:
                return true;

            case ASSIGNMENT:
                final Assignment assignment = (Assignment) parentNode;
                if (BIT_AND_ASSIGN.equals(assignment.getOperator())
                        || BIT_OR_ASSIGN.equals(assignment.getOperator())
                        || BIT_XOR_ASSIGN.equals(assignment.getOperator())) {
                    return true;
                } else if (assignment.getLeftHandSide().equals(node)) {
                    return isNotNull(assignment.getRightHandSide());
                } else if (assignment.getRightHandSide().equals(node)) {
                    if (assignment.getLeftHandSide() instanceof Name) {
                        return isBoolean((Name) assignment.getLeftHandSide());
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }

            case VARIABLE_DECLARATION_FRAGMENT:
                final VariableDeclarationFragment fragment = (VariableDeclarationFragment) parentNode;
                if (fragment.getInitializer().equals(node)) {
                    return isBoolean(fragment.getName());
                } else {
                    return false;
                }

            case RETURN_STATEMENT:
                final ReturnStatement returnStmt = (ReturnStatement) parentNode;
                if (returnStmt.getExpression().equals(node)) {
                    final MethodDeclaration method = getAncestorOrNull(returnStmt, MethodDeclaration.class);
                    if (method != null && method.getReturnType2() != null) {
                        if (hasType(method.getReturnType2().resolveBinding(), "boolean")) {
                            return true;
                        } else if (hasType(method.getReturnType2().resolveBinding(), "java.lang.Boolean")) {
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
                return NOT.equals(((PrefixExpression) parentNode).getOperator());

            case INFIX_EXPRESSION:
                return AND.equals(((InfixExpression) parentNode).getOperator())
                        || OR.equals(((InfixExpression) parentNode).getOperator())
                        || CONDITIONAL_AND.equals(((InfixExpression) parentNode).getOperator())
                        || CONDITIONAL_OR.equals(((InfixExpression) parentNode).getOperator())
                        || XOR.equals(((InfixExpression) parentNode).getOperator());

            default:
                return false;
            }
        }

        private boolean isBoolean(final Name name) {
            if (hasType(name.resolveTypeBinding(), "boolean")) {
                return true;
            } else if (hasType(name.resolveTypeBinding(), "java.lang.Boolean")) {
                autoBoxingCount++;
                return true;
            } else {
                return false;
            }
        }
    }
}
