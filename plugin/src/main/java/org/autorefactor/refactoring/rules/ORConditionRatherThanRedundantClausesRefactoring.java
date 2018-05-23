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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.isPrimitive;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.OR;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class ORConditionRatherThanRedundantClausesRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "OR condition rather than redundant clauses";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace (X && Y) || !X by Y || !X.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility.";
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (isPassive(node) && hasOperator(node, CONDITIONAL_OR, OR)
                && !node.hasExtendedOperands()) {
            final Expression leftOperand = node.getLeftOperand();
            final Expression rightOperand = node.getRightOperand();
            return maybeRefactorCondition(node, node.getOperator(), leftOperand, rightOperand, true)
                    && maybeRefactorCondition(node, node.getOperator(), rightOperand, leftOperand, false);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorCondition(final InfixExpression node, final Operator operator,
            final Expression leftOperand, final Expression rightOperand, final boolean forward) {
        final InfixExpression complexCondition = as(leftOperand, InfixExpression.class);

        if (complexCondition != null
                && !complexCondition.hasExtendedOperands()
                && hasOperator(complexCondition, CONDITIONAL_AND, AND)) {
            final AtomicBoolean isFirstExprPositive = new AtomicBoolean();
            final AtomicBoolean isSecondExprPositive = new AtomicBoolean();
            final AtomicBoolean isThirdExprPositive = new AtomicBoolean();

            final Expression firstExpr = getBasisExpression(complexCondition.getLeftOperand(), isFirstExprPositive);
            final Expression secondExpr = getBasisExpression(complexCondition.getRightOperand(),
                    isSecondExprPositive);
            final Expression thirdExpr = getBasisExpression(rightOperand,
                    isThirdExprPositive);

            if (isPrimitive(firstExpr) && isPrimitive(secondExpr) && isPrimitive(thirdExpr)) {
                if (match(new ASTSemanticMatcher(), secondExpr, thirdExpr)) {
                    replaceDuplicateExpr(node, operator, firstExpr, thirdExpr,
                            isFirstExprPositive.get(),
                            isThirdExprPositive.get(), forward);
                    return DO_NOT_VISIT_SUBTREE;
                } else if (match(new ASTSemanticMatcher(), firstExpr, thirdExpr)) {
                    replaceDuplicateExpr(node, operator, secondExpr, thirdExpr,
                            isSecondExprPositive.get(),
                            isThirdExprPositive.get(), forward);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private Expression getBasisExpression(final Expression originalExpr, final AtomicBoolean isExprPositive) {
        final Expression basisExpr;
        final PrefixExpression negateExpr = as(originalExpr, PrefixExpression.class);
        if (hasOperator(negateExpr, NOT)) {
            basisExpr = negateExpr.getOperand();
            isExprPositive.set(false);
        } else {
            basisExpr = originalExpr;
            isExprPositive.set(true);
        }
        return basisExpr;
    }

    private void replaceDuplicateExpr(final InfixExpression node, final Operator operator,
            final Expression leftExpr,
            final Expression rightExpr,
            final boolean isLeftExprPositive, final boolean isRightExprPositive, final boolean forward) {
        final ASTBuilder b = ctx.getASTBuilder();
        Expression copyOfLeftExpr = b.copy(leftExpr);
        if (!isLeftExprPositive) {
            copyOfLeftExpr = b.not(copyOfLeftExpr);
        }

        Expression copyOfRightExpr = b.copy(rightExpr);
        if (!isRightExprPositive) {
            copyOfRightExpr = b.not(copyOfRightExpr);
        }

        if (forward) {
            ctx.getRefactorings().replace(node,
                    b.infixExpr(copyOfLeftExpr, operator, copyOfRightExpr));
        } else {
            ctx.getRefactorings().replace(node,
                    b.infixExpr(copyOfRightExpr, operator, copyOfLeftExpr));
        }
    }
}
