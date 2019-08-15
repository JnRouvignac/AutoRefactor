/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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
public class LazyLogicalRatherThanEagerCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_LazyLogicalRatherThanEagerCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_LazyLogicalRatherThanEagerCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_LazyLogicalRatherThanEagerCleanUp_reason;
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (!node.hasExtendedOperands()
                && (ASTNodes.hasType(node.getLeftOperand(), boolean.class.getSimpleName()) || ASTNodes.hasType(node.getLeftOperand(), Boolean.class.getCanonicalName()))
                && (ASTNodes.hasType(node.getRightOperand(), boolean.class.getSimpleName()) || ASTNodes.hasType(node.getRightOperand(), Boolean.class.getCanonicalName()))
                && ASTNodes.isPassive(node.getRightOperand())
                && ASTNodes.hasOperator(node, InfixExpression.Operator.AND, InfixExpression.Operator.OR)) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.infixExpr(b.copy(node.getLeftOperand()),
                    getLazyOperator(node), b.copy(node.getRightOperand())));
            return false;
        }
        return true;
    }

    private InfixExpression.Operator getLazyOperator(final InfixExpression node) {
        if (ASTNodes.hasOperator(node, InfixExpression.Operator.AND)) {
            return InfixExpression.Operator.CONDITIONAL_AND;
        } else {
            return InfixExpression.Operator.CONDITIONAL_OR;
        }
    }
}
