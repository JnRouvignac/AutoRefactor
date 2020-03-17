/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2019 Fabrice Tiercelin - Correctly flag the visited nodes and do not reverse the condition
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class UseStringContainsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_reason;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        if (!node.hasExtendedOperands()) {
            Expression leftOperand= ASTNodes.getUnparenthesedExpression(node.getLeftOperand());
            Expression rightOperand= ASTNodes.getUnparenthesedExpression(node.getRightOperand());

            return maybeRefactor(node, leftOperand, rightOperand, true)
                    && maybeRefactor(node, rightOperand, leftOperand, false);
        }

        return true;
    }

    private boolean maybeRefactor(final InfixExpression node, final Expression operand1, final Expression operand2,
            final boolean isMethodOnTheLeft) {
        MethodInvocation indexOf= ASTNodes.as(operand1, MethodInvocation.class);
        Long value= ASTNodes.integerLiteral(operand2);

        if (indexOf != null
                && value != null
                && (ASTNodes.usesGivenSignature(indexOf, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName()) //$NON-NLS-1$
                        || ASTNodes.usesGivenSignature(indexOf, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName()))) { //$NON-NLS-1$

            if (is(node, isMethodOnTheLeft ? InfixExpression.Operator.GREATER_EQUALS : InfixExpression.Operator.LESS_EQUALS, value, 0)
                    || is(node, InfixExpression.Operator.NOT_EQUALS, value, -1)) {
                replaceWithStringContains(node, indexOf, false);
                return false;
            }
            if (is(node, isMethodOnTheLeft ? InfixExpression.Operator.LESS : InfixExpression.Operator.GREATER, value, 0)
                    || is(node, InfixExpression.Operator.EQUALS, value, -1)) {
                replaceWithStringContains(node, indexOf, true);
                return false;
            }
        }

        return true;
    }

    private boolean is(final InfixExpression ie, final InfixExpression.Operator operator, final Long number, final int constant) {
        return ASTNodes.hasOperator(ie, operator)
                && (long) number == constant;
    }

    private void replaceWithStringContains(final InfixExpression ie, final MethodInvocation node, final boolean negate) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        rewrite.set(node, MethodInvocation.NAME_PROPERTY, ast.simpleName("contains"), null); //$NON-NLS-1$

        if (negate) {
            rewrite.replace(ie, ast.not(rewrite.createMoveTarget(node)), null);
        } else {
            rewrite.replace(ie, rewrite.createMoveTarget(node), null);
        }
    }
}
