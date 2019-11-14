/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
import org.eclipse.jdt.core.dom.InfixExpression;

/** See {@link #getDescription()} method. */
public class DoubleCompareRatherThanEqualityCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_DoubleCompareRatherThanEqualityCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_DoubleCompareRatherThanEqualityCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_DoubleCompareRatherThanEqualityCleanUp_reason;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        if (!node.hasExtendedOperands()
                && ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.GREATER_EQUALS, InfixExpression.Operator.LESS, InfixExpression.Operator.GREATER)
                && ASTNodes.hasType(node.getLeftOperand(), double.class.getSimpleName(), Double.class.getCanonicalName())
                && ASTNodes.hasType(node.getRightOperand(), double.class.getSimpleName(), Double.class.getCanonicalName())) {
            replace(node);
            return false;
        }

        return true;
    }

    private void replace(final InfixExpression node) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
                b.infixExpression(
                        b.invoke(Double.class.getSimpleName(), "compare", b.move(node.getLeftOperand()), b.move(node.getRightOperand())), //$NON-NLS-1$
                        node.getOperator(), b.number("0"))); //$NON-NLS-1$
    }
}
