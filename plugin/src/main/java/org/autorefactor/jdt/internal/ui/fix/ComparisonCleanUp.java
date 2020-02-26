/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Comparator;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class ComparisonCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ComparisonCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ComparisonCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ComparisonCleanUp_reason;
    }

    @Override
    public boolean visit(final InfixExpression node) {
        Expression leftOperand= ASTNodes.getUnparenthesedExpression(node.getLeftOperand());
        Expression rightOperand= ASTNodes.getUnparenthesedExpression(node.getRightOperand());

        return node.hasExtendedOperands() || maybeStandardizeComparison(node, leftOperand,
                rightOperand) && maybeStandardizeComparison(node, rightOperand, leftOperand);
    }

    private boolean maybeStandardizeComparison(final InfixExpression node, final Expression comparator,
            final Expression literal) {
        MethodInvocation comparisonMI= ASTNodes.as(comparator, MethodInvocation.class);

        if (comparisonMI != null
                && ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)
                        && (ASTNodes.usesGivenSignature(comparisonMI, Comparable.class.getCanonicalName(), "compareTo", Object.class.getCanonicalName()) //$NON-NLS-1$
                        || ASTNodes.usesGivenSignature(comparisonMI, Comparator.class.getCanonicalName(), "compare", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
                        || getJavaMinorVersion() >= 2
                        && ASTNodes.usesGivenSignature(comparisonMI, String.class.getCanonicalName(), "compareToIgnoreCase", String.class.getCanonicalName()))) { //$NON-NLS-1$
            Object literalValue= literal.resolveConstantExpressionValue();

            if (literalValue instanceof Number) {
                Number numberValue= (Number) literalValue;
                double doubleValue= numberValue.doubleValue();

                if (doubleValue == 0) {
                    return true;
                }

                if (doubleValue < 0) {
                    if (ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS)) {
                        refactorComparingToZero(node, comparisonMI, InfixExpression.Operator.LESS);
                    } else {
                        refactorComparingToZero(node, comparisonMI, InfixExpression.Operator.GREATER_EQUALS);
                    }
                } else if (ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS)) {
                    refactorComparingToZero(node, comparisonMI, InfixExpression.Operator.GREATER);
                } else {
                    refactorComparingToZero(node, comparisonMI, InfixExpression.Operator.LESS_EQUALS);
                }

                return false;
            }
        }

        return true;
    }

    private void refactorComparingToZero(final InfixExpression node, final MethodInvocation comparisonMI,
            final InfixExpression.Operator operator) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node, b.infixExpression(b.createMoveTarget(comparisonMI), operator, b.number("0"))); //$NON-NLS-1$
    }
}
