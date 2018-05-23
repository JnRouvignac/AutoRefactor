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
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isNullLiteral;
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class BooleanEqualsRatherThanNullCheckRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Boolean equals() rather than null check";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace a null check of a Boolean followed by its value by an equality with a boolean constant.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and testing cost.";
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (hasOperator(node, CONDITIONAL_AND, CONDITIONAL_OR)) {
            final Expression leftOperand = node.getLeftOperand();
            final Expression rightOperand = node.getRightOperand();

            final InfixExpression condition = as(leftOperand, InfixExpression.class);
            final boolean isNullCheck = hasOperator(condition, EQUALS);
            final boolean isAndExpr = hasOperator(node, CONDITIONAL_AND);
            if (!node.hasExtendedOperands()
                    && (isNullCheck ^ isAndExpr) && condition != null && hasOperator(condition, EQUALS, NOT_EQUALS)) {
                Expression firstExpr = null;
                if (isNullLiteral(condition.getLeftOperand())) {
                    firstExpr = condition.getRightOperand();
                } else if (isNullLiteral(condition.getRightOperand())) {
                    firstExpr = condition.getLeftOperand();
                }

                Expression secondExpr = null;
                final PrefixExpression negateSecondExpr = as(rightOperand, PrefixExpression.class);
                final boolean isPositiveExpr;
                if (negateSecondExpr != null && hasOperator(negateSecondExpr, NOT)) {
                    secondExpr = negateSecondExpr.getOperand();
                    isPositiveExpr = false;
                } else {
                    secondExpr = rightOperand;
                    isPositiveExpr = true;
                }

                if (firstExpr != null && hasType(firstExpr, "java.lang.Boolean")
                        && isPassive(firstExpr)
                        && match(new ASTSemanticMatcher(), firstExpr, secondExpr)) {
                    replaceNullCheck(node, firstExpr, isNullCheck, isAndExpr, isPositiveExpr);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceNullCheck(final InfixExpression node, final Expression firstExpr, final boolean isNullCheck,
            final boolean isAndExpr, final boolean isPositiveExpr) {
        final ASTBuilder b = ctx.getASTBuilder();

        final Name booleanConstant = b.name("Boolean",
                isAndExpr == isPositiveExpr ? "TRUE" : "FALSE");
        final MethodInvocation equalsMethod = b.invoke(booleanConstant, "equals", b.copy(firstExpr));

        Expression newExpr = null;
        if (!isNullCheck || isAndExpr) {
            newExpr = equalsMethod;
        } else {
            newExpr = b.not(equalsMethod);
        }
        ctx.getRefactorings().replace(node,
                newExpr);
    }
}
