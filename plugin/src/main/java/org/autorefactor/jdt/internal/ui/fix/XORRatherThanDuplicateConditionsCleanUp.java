/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
public class XORRatherThanDuplicateConditionsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_XORRatherThanDuplicateConditionsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_XORRatherThanDuplicateConditionsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_XORRatherThanDuplicateConditionsCleanUp_reason;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR) && !node.hasExtendedOperands()) {
            final InfixExpression firstCondition= ASTNodes.as(node.getLeftOperand(), InfixExpression.class);
            final InfixExpression secondCondition= ASTNodes.as(node.getRightOperand(), InfixExpression.class);

            if ((firstCondition != null) && !firstCondition.hasExtendedOperands()
                    && ASTNodes.hasOperator(firstCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND) && (secondCondition != null)
                    && !secondCondition.hasExtendedOperands() && ASTNodes.hasOperator(secondCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)
                    && ASTNodes.isPassive(firstCondition.getLeftOperand()) && ASTNodes.isPassive(firstCondition.getRightOperand())
                    && ASTNodes.isPassive(secondCondition.getLeftOperand()) && ASTNodes.isPassive(secondCondition.getRightOperand())) {
                return maybeReplaceDuplicateExpression(node, firstCondition.getLeftOperand(), secondCondition.getLeftOperand(),
                        firstCondition.getRightOperand(), secondCondition.getRightOperand())
                        && maybeReplaceDuplicateExpression(node, firstCondition.getLeftOperand(), secondCondition.getRightOperand(),
                                firstCondition.getRightOperand(), secondCondition.getLeftOperand());
            }
        }

        return true;
    }

    private boolean maybeReplaceDuplicateExpression(final InfixExpression node, final Expression firstExpression,
            final Expression firstOppositeExpression, final Expression secondExpression, final Expression secondOppositeExpression) {
        if (ASTSemanticMatcher.INSTANCE.matchOpposite(firstExpression, firstOppositeExpression)
                && ASTSemanticMatcher.INSTANCE.matchOpposite(secondExpression, secondOppositeExpression)) {
            final AtomicBoolean isFirstExprPositive= new AtomicBoolean();
            final AtomicBoolean isSecondExprPositive= new AtomicBoolean();

            final Expression firstBasicExpression= getBasisExpression(firstExpression, isFirstExprPositive);
            final Expression secondBasicExpression= getBasisExpression(secondExpression, isSecondExprPositive);

            replaceDuplicateExpression(node, firstBasicExpression, secondBasicExpression, isFirstExprPositive, isSecondExprPositive);
            return false;
        }

        return true;
    }

    private Expression getBasisExpression(final Expression originalExpression, final AtomicBoolean isExprPositive) {
        Expression basisExpression= null;
        final PrefixExpression negateExpression= ASTNodes.as(originalExpression, PrefixExpression.class);

        if (ASTNodes.hasOperator(negateExpression, PrefixExpression.Operator.NOT)) {
            basisExpression= negateExpression.getOperand();
            isExprPositive.set(false);
        } else {
            basisExpression= originalExpression;
            isExprPositive.set(true);
        }

        return basisExpression;
    }

    private void replaceDuplicateExpression(final InfixExpression node, final Expression firstExpression,
            final Expression secondExpression, final AtomicBoolean isFirstExprPositive,
            final AtomicBoolean isSecondExprPositive) {
        final ASTNodeFactory b= ctx.getASTBuilder();

        if (isFirstExprPositive.get() == isSecondExprPositive.get()) {
            ctx.getRefactorings().replace(node, b.infixExpression(b.move(firstExpression), InfixExpression.Operator.EQUALS, b.move(secondExpression)));
        } else {
            ctx.getRefactorings().replace(node, b.infixExpression(b.move(firstExpression), InfixExpression.Operator.XOR, b.move(secondExpression)));
        }
    }
}
