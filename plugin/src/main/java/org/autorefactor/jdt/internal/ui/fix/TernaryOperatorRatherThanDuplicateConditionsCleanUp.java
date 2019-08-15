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

import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class TernaryOperatorRatherThanDuplicateConditionsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_TernaryOperatorRatherThanDuplicateConditionsCleanUp_reason;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR) && !node.hasExtendedOperands()) {
            final InfixExpression firstCondition= ASTNodes.as(node.getLeftOperand(), InfixExpression.class);
            final InfixExpression secondCondition= ASTNodes.as(node.getRightOperand(), InfixExpression.class);

            if (firstCondition != null && !firstCondition.hasExtendedOperands()
                    && ASTNodes.hasOperator(firstCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND) && secondCondition != null
                    && !secondCondition.hasExtendedOperands() && ASTNodes.hasOperator(secondCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)
                    && isBooleanAndPassive(firstCondition.getLeftOperand())
                    && isBooleanAndPassive(firstCondition.getRightOperand())
                    && isBooleanAndPassive(secondCondition.getLeftOperand())
                    && isBooleanAndPassive(secondCondition.getRightOperand())) {
                ASTSemanticMatcher matcher= new ASTSemanticMatcher();

                return maybeReplaceDuplicateExpr(matcher, node, firstCondition.getLeftOperand(),
                        secondCondition.getLeftOperand(), firstCondition.getRightOperand(),
                        secondCondition.getRightOperand())
                        && maybeReplaceDuplicateExpr(matcher, node, firstCondition.getLeftOperand(),
                                secondCondition.getRightOperand(), firstCondition.getRightOperand(),
                                secondCondition.getLeftOperand())
                        && maybeReplaceDuplicateExpr(matcher, node, firstCondition.getRightOperand(),
                                secondCondition.getLeftOperand(), firstCondition.getLeftOperand(),
                                secondCondition.getRightOperand())
                        && maybeReplaceDuplicateExpr(matcher, node, firstCondition.getRightOperand(),
                                secondCondition.getRightOperand(), firstCondition.getLeftOperand(),
                                secondCondition.getLeftOperand());
            }
        }

        return true;
    }

    private boolean isBooleanAndPassive(final Expression expr) {
        return ASTNodes.isPrimitive(expr, boolean.class.getSimpleName()) && ASTNodes.isPassive(expr);
    }

    private boolean maybeReplaceDuplicateExpr(final ASTSemanticMatcher matcher, final InfixExpression node,
            final Expression oneCondition, final Expression oppositeCondition, final Expression oneExpr,
            final Expression oppositeExpr) {
        if (matcher.matchOpposite(oneCondition, oppositeCondition) && !ASTNodes.match(matcher, oneExpr, oppositeExpr)) {
            replaceDuplicateExpr(node, oneCondition, oneExpr, oppositeExpr);
            return false;
        }

        return true;
    }

    private Expression getBasisExpression(final Expression originalExpr, final AtomicBoolean isExprPositive) {
        Expression basisExpr= null;
        final PrefixExpression negateExpr= ASTNodes.as(originalExpr, PrefixExpression.class);

        if (ASTNodes.hasOperator(negateExpr, PrefixExpression.Operator.NOT)) {
            basisExpr= negateExpr.getOperand();
            isExprPositive.set(false);
        } else {
            basisExpr= originalExpr;
            isExprPositive.set(true);
        }

        return basisExpr;
    }

    private void replaceDuplicateExpr(final InfixExpression node, final Expression oneCondition,
            final Expression oneExpr, final Expression oppositeExpr) {
        final AtomicBoolean isFirstExprPositive= new AtomicBoolean();

        final Expression basicExpr= getBasisExpression(oneCondition, isFirstExprPositive);

        final ASTNodeFactory b= ctx.getASTBuilder();
        final Expression thenExpr;
        final Expression elseExpr;

        if (isFirstExprPositive.get()) {
            thenExpr= oneExpr;
            elseExpr= oppositeExpr;
        } else {
            thenExpr= oppositeExpr;
            elseExpr= oneExpr;
        }

        ctx.getRefactorings().replace(node, b.conditionalExpr(b.copy(basicExpr), b.copy(thenExpr), b.copy(elseExpr)));
    }
}
