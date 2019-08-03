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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasOperator;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasType;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
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
                && hasOperator(node, EQUALS, NOT_EQUALS, LESS_EQUALS, GREATER_EQUALS, LESS, GREATER)
                && hasType(node.getLeftOperand(), double.class.getSimpleName(), Double.class.getCanonicalName())
                && hasType(node.getRightOperand(), double.class.getSimpleName(), Double.class.getCanonicalName())) {
            replace(node);
            return false;
        }

        return true;
    }

    private void replace(final InfixExpression node) {
        final ASTBuilder b= this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
                b.infixExpr(
                        b.invoke("Double", "compare", b.copy(node.getLeftOperand()), b.copy(node.getRightOperand())),
                        node.getOperator(), b.number("0")));
    }
}
