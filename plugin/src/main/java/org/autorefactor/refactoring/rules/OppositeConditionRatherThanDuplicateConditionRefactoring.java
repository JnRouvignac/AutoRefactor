/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;

import java.util.Arrays;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.Statement;

/**
 * Refactors:
 *
 * <pre>
 * if (a && b) {
 *   {{code 1}}
 * } if (a) {
 *   {{code 2}}
 * } else {
 *   {{code 3}}
 * }
 * </pre>
 *
 * into
 *
 * <pre>
 * if (!a) {
 *   {{code 3}}
 * } if (b) {
 *   {{code 1}}
 * } else {
 *   {{code 2}}
 * }
 * </pre>
 *
 * @see #getDescription()
 */
public class OppositeConditionRatherThanDuplicateConditionRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Opposite condition rather than duplicate condition";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Do not repeat the same condition in several if.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the reading, debugging and testing cost.";
    }

    @Override
    public boolean visit(IfStatement node) {
        if (node.getExpression() instanceof InfixExpression
                && node.getElseStatement() != null
                && node.getElseStatement() instanceof IfStatement) {
            final InfixExpression firstCondition = (InfixExpression) node.getExpression();

            if (!firstCondition.hasExtendedOperands()
                    && Arrays.<Operator>asList(Operator.AND, Operator.CONDITIONAL_AND)
                            .contains(firstCondition.getOperator())
                    && isPassive(firstCondition.getLeftOperand())
                    && isPassive(firstCondition.getRightOperand())) {
                final IfStatement secondIf = (IfStatement) node.getElseStatement();

                if (secondIf.getElseStatement() != null) {
                    return maybeRefactorCondition(node, secondIf, firstCondition.getLeftOperand(),
                            firstCondition.getRightOperand())
                            && maybeRefactorCondition(node, secondIf, firstCondition.getRightOperand(),
                                    firstCondition.getLeftOperand());
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorCondition(final IfStatement node, final IfStatement secondIf,
            final Expression duplicateExpr,
            final Expression notDuplicateExpr) {
        final ASTSemanticMatcher matcher = new ASTSemanticMatcher();

        if (match(matcher, duplicateExpr, secondIf.getExpression())) {
            refactorCondition(node, duplicateExpr, notDuplicateExpr, secondIf.getThenStatement(),
                    secondIf.getElseStatement());
            return DO_NOT_VISIT_SUBTREE;
        } else if (matcher.matchOpposite(duplicateExpr, secondIf.getExpression())) {
            refactorCondition(node, duplicateExpr, notDuplicateExpr, secondIf.getElseStatement(),
                    secondIf.getThenStatement());
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private void refactorCondition(final IfStatement node, final Expression duplicateExpr,
            final Expression notDuplicateExpr, final Statement positiveStmt, final Statement negativeStmt) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        Statement negativeStmtCopy;
        if (negativeStmt instanceof IfStatement) {
            negativeStmtCopy = b.block(b.move(negativeStmt));
        } else {
            negativeStmtCopy = b.move(negativeStmt);
        }

        final Expression secondCond;
        final Statement secondStmtCopy;
        final Statement thirdStmtCopy;
        final PrefixExpression negativeCond = as(notDuplicateExpr, PrefixExpression.class);

        if (negativeCond != null && PrefixExpression.Operator.NOT.equals(negativeCond.getOperator())) {
            secondCond = negativeCond.getOperand();
            secondStmtCopy = b.move(positiveStmt);
            thirdStmtCopy = b.move(node.getThenStatement());
        } else {
            secondCond = notDuplicateExpr;
            secondStmtCopy = b.move(node.getThenStatement());
            thirdStmtCopy = b.move(positiveStmt);
        }

        this.ctx.getRefactorings().replace(node,
                b.if0(b.parenthesizeIfNeeded(b.negate(removeParentheses(duplicateExpr))),
                        negativeStmtCopy,
                        b.if0(b.copy(removeParentheses(secondCond)), secondStmtCopy, thirdStmtCopy)));
    }
}
