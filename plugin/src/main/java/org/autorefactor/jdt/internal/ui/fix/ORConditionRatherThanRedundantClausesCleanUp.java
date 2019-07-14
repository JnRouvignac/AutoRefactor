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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasOperator;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPassive;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isPrimitive;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.OR;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;

/** See {@link #getDescription()} method. */
public class ORConditionRatherThanRedundantClausesCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_reason;
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (isPassive(node) && hasOperator(node, CONDITIONAL_OR, OR) && !node.hasExtendedOperands()) {
            final Expression leftOperand= node.getLeftOperand();
            final Expression rightOperand= node.getRightOperand();
            return maybeRefactorCondition(node, node.getOperator(), leftOperand, rightOperand, true)
                    && maybeRefactorCondition(node, node.getOperator(), rightOperand, leftOperand, false);
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorCondition(final InfixExpression node, final Operator operator,
            final Expression operand1, final Expression operand2, final boolean forward) {
        final InfixExpression complexCondition= as(operand1, InfixExpression.class);

        if (complexCondition != null && !complexCondition.hasExtendedOperands()
                && hasOperator(complexCondition, CONDITIONAL_AND, AND)) {
            final ASTSemanticMatcher matcher= new ASTSemanticMatcher();

            if (isPrimitive(complexCondition.getLeftOperand()) && isPrimitive(complexCondition.getRightOperand())
                    && isPrimitive(operand2)) {
                if (matcher.matchOpposite(complexCondition.getLeftOperand(), operand2)) {
                    replaceDuplicateExpr(node, operator, complexCondition.getRightOperand(), operand2, forward);
                    return DO_NOT_VISIT_SUBTREE;
                }

                if (matcher.matchOpposite(complexCondition.getRightOperand(), operand2)) {
                    replaceDuplicateExpr(node, operator, complexCondition.getLeftOperand(), operand2, forward);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceDuplicateExpr(final InfixExpression node, final Operator operator, final Expression leftExpr,
            final Expression rightExpr, final boolean forward) {
        final ASTBuilder b= ctx.getASTBuilder();

        if (forward) {
            ctx.getRefactorings().replace(node, b.infixExpr(b.copy(leftExpr), operator, b.copy(rightExpr)));
        } else {
            ctx.getRefactorings().replace(node, b.infixExpr(b.copy(rightExpr), operator, b.copy(leftExpr)));
        }
    }
}
