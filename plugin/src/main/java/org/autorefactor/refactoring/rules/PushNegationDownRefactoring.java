/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Pushes negations down, inside the expressions.
 */
public class PushNegationDownRefactoring extends AbstractRefactoringRule {

    /** {@inheritDoc} */
    @Override
    public boolean visit(PrefixExpression node) {
        if (!PrefixExpression.Operator.NOT.equals(node.getOperator())) {
            return VISIT_SUBTREE;
        }
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression operand = removeParentheses(node.getOperand());
        if (operand instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) operand;
            if (PrefixExpression.Operator.NOT.equals(pe.getOperator())) {
                this.ctx.getRefactorings().replace(node,
                        b.move(pe.getOperand()));
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (operand instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) operand;
            final Object reverseOp = OperatorEnum.getOperator(ie).getReverseBooleanOperator();
            if (reverseOp != null) {
                final List<Expression> extendedOperands = new ArrayList<Expression>(extendedOperands(ie));
                if (hasType(ie.getLeftOperand(), "boolean", "java.lang.Boolean")) {
                    for (ListIterator<Expression> it = extendedOperands.listIterator(); it.hasNext();) {
                        it.set(b.negate(it.next()));
                    }
                    this.ctx.getRefactorings().replace(node,
                            b.parenthesize(b.infixExpr(
                                    b.negate(ie.getLeftOperand()),
                                    (InfixExpression.Operator) reverseOp,
                                    b.negate(ie.getRightOperand()),
                                    extendedOperands)));
                    return DO_NOT_VISIT_SUBTREE;
                } else {
                    for (ListIterator<Expression> it = extendedOperands.listIterator(); it.hasNext();) {
                        it.set(b.move(it.next()));
                    }
                    this.ctx.getRefactorings().replace(node,
                            b.parenthesize(b.infixExpr(
                                    b.move(ie.getLeftOperand()),
                                    (InfixExpression.Operator) reverseOp,
                                    b.move(ie.getRightOperand()),
                                    extendedOperands)));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }
}
