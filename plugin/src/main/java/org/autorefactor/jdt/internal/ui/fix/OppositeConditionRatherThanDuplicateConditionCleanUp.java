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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
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
public class OppositeConditionRatherThanDuplicateConditionCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_OppositeConditionRatherThanDuplicateConditionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_OppositeConditionRatherThanDuplicateConditionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_OppositeConditionRatherThanDuplicateConditionCleanUp_reason;
    }

    @Override
    public boolean visit(IfStatement node) {
        if (node.getExpression() instanceof InfixExpression && node.getElseStatement() != null
                && node.getElseStatement() instanceof IfStatement) {
            final InfixExpression firstCondition= (InfixExpression) node.getExpression();

            if (!firstCondition.hasExtendedOperands()
                    && ASTNodes.hasOperator(firstCondition, InfixExpression.Operator.AND, InfixExpression.Operator.CONDITIONAL_AND)
                    && ASTNodes.isPassive(firstCondition.getLeftOperand()) && ASTNodes.isPassive(firstCondition.getRightOperand())) {
                final IfStatement secondIf= (IfStatement) node.getElseStatement();

                if (secondIf.getElseStatement() != null) {
                    return maybeRefactorCondition(node, secondIf, firstCondition.getLeftOperand(),
                            firstCondition.getRightOperand())
                            && maybeRefactorCondition(node, secondIf, firstCondition.getRightOperand(),
                                    firstCondition.getLeftOperand());
                }
            }
        }
        return true;
    }

    private boolean maybeRefactorCondition(final IfStatement node, final IfStatement secondIf,
            final Expression duplicateExpression, final Expression notDuplicateExpression) {
        final ASTSemanticMatcher matcher= new ASTSemanticMatcher();

        if (ASTNodes.match(matcher, duplicateExpression, secondIf.getExpression())) {
            refactorCondition(node, duplicateExpression, notDuplicateExpression, secondIf.getThenStatement(),
                    secondIf.getElseStatement());
            return false;
        } else if (matcher.matchOpposite(duplicateExpression, secondIf.getExpression())) {
            refactorCondition(node, duplicateExpression, notDuplicateExpression, secondIf.getElseStatement(),
                    secondIf.getThenStatement());
            return false;
        }

        return true;
    }

    private void refactorCondition(final IfStatement node, final Expression duplicateExpression,
            final Expression notDuplicateExpression, final Statement positiveStatement, final Statement negativeStatement) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();

        Statement negativeStmtCopy;
        if (negativeStatement instanceof IfStatement) {
            negativeStmtCopy= b.block(b.move(negativeStatement));
        } else {
            negativeStmtCopy= b.move(negativeStatement);
        }

        final Expression secondCond;
        final Statement secondStmtCopy;
        final Statement thirdStmtCopy;
        final PrefixExpression negativeCond= ASTNodes.as(notDuplicateExpression, PrefixExpression.class);

        if (negativeCond != null && ASTNodes.hasOperator(negativeCond, PrefixExpression.Operator.NOT)) {
            secondCond= negativeCond.getOperand();
            secondStmtCopy= b.move(positiveStatement);
            thirdStmtCopy= b.move(node.getThenStatement());
        } else {
            secondCond= notDuplicateExpression;
            secondStmtCopy= b.move(node.getThenStatement());
            thirdStmtCopy= b.move(positiveStatement);
        }

        this.ctx.getRefactorings().replace(node,
                b.if0(b.parenthesizeIfNeeded(b.negate(ASTNodes.getUnparenthesedExpression(duplicateExpression))), negativeStmtCopy,
                        b.if0(b.copy(ASTNodes.getUnparenthesedExpression(secondCond)), secondStmtCopy, thirdStmtCopy)));
    }
}
