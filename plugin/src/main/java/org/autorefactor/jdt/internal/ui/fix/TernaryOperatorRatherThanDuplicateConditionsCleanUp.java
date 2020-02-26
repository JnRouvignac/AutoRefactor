/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2018 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class TernaryOperatorRatherThanDuplicateConditionsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_reason;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR)) {
            List<Expression> operands= ASTNodes.allOperands(node);

            for (int i= 1; i < operands.size(); i++) {
                List<Expression> previousOperands= operands.subList(0, i - 1);
                InfixExpression firstCondition= ASTNodes.as(operands.get(i - 1), InfixExpression.class);
                InfixExpression secondCondition= ASTNodes.as(operands.get(i), InfixExpression.class);
                List<Expression> nextOperands= operands.subList(i + 1, operands.size());

                if (firstCondition != null && !firstCondition.hasExtendedOperands()
                        && ASTNodes.hasOperator(firstCondition, InfixExpression.Operator.CONDITIONAL_AND,
                                InfixExpression.Operator.AND)
                        && secondCondition != null && !secondCondition.hasExtendedOperands()
                        && ASTNodes.hasOperator(secondCondition, InfixExpression.Operator.CONDITIONAL_AND,
                                InfixExpression.Operator.AND)
                        && isBooleanAndPassive(firstCondition.getLeftOperand())
                        && isBooleanAndPassive(firstCondition.getRightOperand())
                        && isBooleanAndPassive(secondCondition.getLeftOperand())
                        && isBooleanAndPassive(secondCondition.getRightOperand())) {
                    if (!maybeReplaceDuplicateExpression(node, firstCondition.getLeftOperand(), secondCondition.getLeftOperand(),
                            firstCondition.getRightOperand(), secondCondition.getRightOperand(),
                            previousOperands, nextOperands)
                            || !maybeReplaceDuplicateExpression(node, firstCondition.getLeftOperand(), secondCondition.getRightOperand(),
                                    firstCondition.getRightOperand(), secondCondition.getLeftOperand(),
                                    previousOperands, nextOperands)
                            || !maybeReplaceDuplicateExpression(node, firstCondition.getRightOperand(), secondCondition.getLeftOperand(),
                                    firstCondition.getLeftOperand(), secondCondition.getRightOperand(),
                                    previousOperands, nextOperands)
                            || !maybeReplaceDuplicateExpression(node, firstCondition.getRightOperand(), secondCondition.getRightOperand(),
                                    firstCondition.getLeftOperand(), secondCondition.getLeftOperand(),
                                    previousOperands, nextOperands)) {
                        return false;
                    }
                }
            }
        }

        return true;
    }

    private boolean isBooleanAndPassive(final Expression expression) {
        return ASTNodes.isPrimitive(expression, boolean.class.getSimpleName()) && ASTNodes.isPassive(expression);
    }

    private boolean maybeReplaceDuplicateExpression(final InfixExpression node, final Expression oneCondition,
            final Expression oppositeCondition, final Expression oneExpression, final Expression oppositeExpression,
            final List<Expression> previousOperands, final List<Expression> nextOperands) {
        if (ASTSemanticMatcher.INSTANCE.matchOpposite(oneCondition, oppositeCondition)
                && !ASTNodes.match(oneExpression, oppositeExpression)) {
            replaceDuplicateExpression(node, oneCondition, oneExpression, oppositeExpression, previousOperands,
                    nextOperands);
            return false;
        }

        return true;
    }

    private void replaceDuplicateExpression(final InfixExpression node, final Expression oneCondition,
            final Expression oneExpression, final Expression oppositeExpression, final List<Expression> previousOperands,
            final List<Expression> nextOperands) {
        AtomicBoolean isFirstExprPositive= new AtomicBoolean();

        Expression basicExpression= getBasisExpression(oneCondition, isFirstExprPositive);

        Expression thenExpression;
        Expression elseExpression;

        if (isFirstExprPositive.get()) {
            thenExpression= oneExpression;
            elseExpression= oppositeExpression;
        } else {
            thenExpression= oppositeExpression;
            elseExpression= oneExpression;
        }

        ASTNodeFactory b= ctx.getASTBuilder();
        Refactorings r= ctx.getRefactorings();

        ParenthesizedExpression newConditionalExpression= b.parenthesize(b.conditionalExpression(b.createMoveTarget(basicExpression),
                b.createMoveTarget(thenExpression), b.createMoveTarget(elseExpression)));

        if (previousOperands.isEmpty() && nextOperands.isEmpty()) {
            r.replace(node, newConditionalExpression);
        } else {
            List<Expression> operands= b.createMoveTarget(previousOperands);
            operands.add(newConditionalExpression);
            operands.addAll(b.createMoveTarget(nextOperands));
            r.replace(node, b.infixExpression(node.getOperator(), operands));
        }
    }

    private Expression getBasisExpression(final Expression originalExpression, final AtomicBoolean isExprPositive) {
        Expression basisExpression;
        PrefixExpression negateExpression= ASTNodes.as(originalExpression, PrefixExpression.class);

        if (ASTNodes.hasOperator(negateExpression, PrefixExpression.Operator.NOT)) {
            basisExpression= negateExpression.getOperand();
            isExprPositive.set(false);
        } else {
            basisExpression= originalExpression;
            isExprPositive.set(true);
        }

        return basisExpression;
    }
}
