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

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.PrefixExpression;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class PushNegationDownRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Pushes negations down, inside the negated expressions.";
    }

    @Override
    public String getName() {
        return "Push negation down";
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
                final List<Expression> allOperands = new ArrayList<Expression>(allOperands(ie));
                if (hasType(ie.getLeftOperand(), "boolean", "java.lang.Boolean")
                        && hasType(ie.getRightOperand(), "boolean", "java.lang.Boolean")) {
                    for (ListIterator<Expression> it = allOperands.listIterator(); it.hasNext();) {
                        it.set(b.negate(it.next()));
                    }
                    r.replace(node, b.parenthesize(b.infixExpr(reverseOp, allOperands)));
                    return DO_NOT_VISIT_SUBTREE;
                } else {
                    r.replace(node, b.parenthesize(b.infixExpr(reverseOp, b.move(allOperands))));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }
}
