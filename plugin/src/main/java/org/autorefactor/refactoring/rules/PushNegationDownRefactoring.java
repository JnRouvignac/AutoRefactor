/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.allOperands;
import static org.autorefactor.refactoring.ASTHelper.getBooleanLiteral;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.OR;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class PushNegationDownRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Push negation down";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Pushes negations down, inside the negated expressions.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It disambiguates the code to reduce bug hazard.";
    }

    @Override
    public boolean visit(PrefixExpression node) {
        if (!hasOperator(node, NOT)) {
            return VISIT_SUBTREE;
        }
        final ASTBuilder b = ctx.getASTBuilder();
        final Refactorings r = ctx.getRefactorings();

        final Expression operand = removeParentheses(node.getOperand());
        if (operand instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) operand;
            if (hasOperator(pe, NOT)) {
                r.replace(node, b.move(pe.getOperand()));
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (operand instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) operand;
            final Operator reverseOp = (Operator) OperatorEnum.getOperator(ie).getReverseBooleanOperator();
            if (reverseOp != null) {
                List<Expression> allOperands = new ArrayList<Expression>(allOperands(ie));
                if (Arrays.<Operator>asList(CONDITIONAL_AND, CONDITIONAL_OR, AND, OR).contains(ie.getOperator())) {
                    for (ListIterator<Expression> it = allOperands.listIterator(); it.hasNext();) {
                        it.set(b.negate(it.next()));
                    }
                } else {
                    allOperands = b.move(allOperands);
                }
                r.replace(node, b.parenthesize(b.infixExpr(reverseOp, allOperands)));
                return DO_NOT_VISIT_SUBTREE;
            }
        } else {
            final Boolean constant = getBooleanLiteral(operand);
            if (constant != null) {
                r.replace(node, b.boolean0(!constant));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }
}
