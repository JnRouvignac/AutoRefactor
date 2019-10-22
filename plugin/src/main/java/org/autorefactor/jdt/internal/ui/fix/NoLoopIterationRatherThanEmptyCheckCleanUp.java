/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper;
import org.autorefactor.jdt.internal.corext.dom.ForLoopHelper.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class NoLoopIterationRatherThanEmptyCheckCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoLoopIterationRatherThanEmptyCheckCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoLoopIterationRatherThanEmptyCheckCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoLoopIterationRatherThanEmptyCheckCleanUp_reason;
    }

    @Override
    public boolean visit(IfStatement node) {
        if (node.getElseStatement() == null) {
            List<Statement> statements= ASTNodes.asList(node.getThenStatement());

            if (statements != null
                    && statements.size() == 1) {
                Expression container= getContainer(statements);

                if (ASTNodes.isArray(container) && ASTNodes.isPassive(container)) {
                    InfixExpression condition= ASTNodes.as(node.getExpression(), InfixExpression.class);

                    if (isConditionValid(condition, container)) {
                        final ASTNodeFactory b= ctx.getASTBuilder();
                        final Refactorings r= ctx.getRefactorings();
                        r.replace(node, b.move(statements.get(0)));
                        return false;
                    }

                    if (ASTNodes.hasOperator(condition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)) {
                        List<Expression> operands= ASTNodes.allOperands(condition);
                        Expression operand= ASTNodes.as(operands.get(operands.size() - 1), InfixExpression.class);

                        if (isConditionValid(operand, container)) {
                            removeCondition(condition, operands);
                            return false;
                        }
                    }
                }
            }
        }

        return true;
    }

    private boolean isConditionValid(Expression expression, Expression container) {
        InfixExpression condition= ASTNodes.as(expression, InfixExpression.class);
        return condition != null
                && !condition.hasExtendedOperands() && ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS,
                        InfixExpression.Operator.GREATER,
                        InfixExpression.Operator.GREATER_EQUALS,
                        InfixExpression.Operator.LESS,
                        InfixExpression.Operator.LESS_EQUALS)
                && (isConditionValid(condition, container, condition.getLeftOperand(), condition.getRightOperand(), true)
                        || isConditionValid(condition, container, condition.getRightOperand(), condition.getLeftOperand(), false));
    }

    private boolean isConditionValid(InfixExpression condition, Expression container, final Expression arrayOperand,
            final Expression literalOperand, boolean isArrayOnLeft) {
        Expression array= getArray(container, arrayOperand);
        Long literal= ASTNodes.integerLiteral(literalOperand);

        if (array != null
                && literal != null) {
            long value= literal;

            if (ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS)) {
                return value == 0;
            }
            if (ASTNodes.hasOperator(condition, InfixExpression.Operator.GREATER)) {
                return isArrayOnLeft && value == 0;
            }
            if (ASTNodes.hasOperator(condition, InfixExpression.Operator.GREATER_EQUALS)) {
                return isArrayOnLeft && value == 1;
            }
            if (ASTNodes.hasOperator(condition, InfixExpression.Operator.LESS)) {
                return !isArrayOnLeft && value == 0;
            }
            if (ASTNodes.hasOperator(condition, InfixExpression.Operator.LESS_EQUALS)) {
                return !isArrayOnLeft && value == 1;
            }
        }

        return false;
    }

    private Expression getArray(Expression container, final Expression operand) {
        FieldAccess fieldAccess= ASTNodes.as(operand, FieldAccess.class);
        QualifiedName name= ASTNodes.as(operand, QualifiedName.class);

        if (fieldAccess != null) {
            if (ASTNodes.isSameVariable(fieldAccess.getExpression(), container) && "length".equals(fieldAccess.getName().getIdentifier())) {
                return fieldAccess.getExpression();
            }
        } else if (name != null) {
            if (ASTNodes.isSameVariable(name.getQualifier(), container) && "length".equals(name.getName().getIdentifier())) {
                return name.getQualifier();
            }
        }

        return null;
    }

    private Expression getContainer(List<Statement> statements) {
        ForStatement forStatement= ASTNodes.as(statements.get(0), ForStatement.class);
        EnhancedForStatement enhancedForStatement= ASTNodes.as(statements.get(0), EnhancedForStatement.class);

        if (forStatement != null) {
            final ForLoopContent loopContent= ForLoopHelper.iterateOverContainer(forStatement);

            if (loopContent != null) {
                return loopContent.getContainerVariable();
            }
        } else if (enhancedForStatement != null) {
            return enhancedForStatement.getExpression();
        }

        return null;
    }

    private void removeCondition(InfixExpression condition, List<Expression> operands) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final Refactorings r= ctx.getRefactorings();

        if (operands.size() == 2) {
            r.replace(condition, b.copy(operands.get(0)));
        } else {
            operands.remove(operands.size() - 1);
            InfixExpression newCondition= b.infixExpression(condition.getOperator(), b.copy(operands));

            r.replace(condition, newCondition);
        }
    }
}
