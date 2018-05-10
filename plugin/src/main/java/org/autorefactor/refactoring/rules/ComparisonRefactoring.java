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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class ComparisonRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Comparison to 0 rather than 1 or -1";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Fix Comparable.compareTo() usage.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It standardizes the code.";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (!node.hasExtendedOperands()) {
            if (maybeStandardizeComparison(node, node.getLeftOperand(), node.getRightOperand())
                    == DO_NOT_VISIT_SUBTREE) {
                return DO_NOT_VISIT_SUBTREE;
            }
            return maybeStandardizeComparison(node, node.getRightOperand(), node.getLeftOperand());
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeStandardizeComparison(InfixExpression node, final Expression comparator,
            final Expression literal) {
        if (removeParentheses(comparator) instanceof MethodInvocation) {
            final MethodInvocation comparisonMI = (MethodInvocation) removeParentheses(comparator);
            if (comparisonMI.getExpression() == null) {
                // TODO JNR handle same class calls and sub classes
                return VISIT_SUBTREE;
            }

            if (isMethod(comparisonMI, "java.lang.Comparable", "compareTo", "java.lang.Object")
                || isMethod(comparisonMI, "java.lang.Comparator", "compare", "java.lang.Object", "java.lang.Object")
                || (getJavaMinorVersion() >= 2
                    && isMethod(comparisonMI, "java.lang.String", "compareToIgnoreCase", "java.lang.String"))) {
                final Object literalValue = literal.resolveConstantExpressionValue();

                if (literalValue != null && literalValue instanceof Number) {
                    final Number numberValue = (Number) literalValue;
                    final double doubleValue = numberValue.doubleValue();

                    if (doubleValue == 0) {
                        return VISIT_SUBTREE;
                    }

                    if (hasOperator(node, EQUALS)) {
                        if (doubleValue < 0) {
                            refactorComparingToZero(node, comparisonMI, LESS);
                        } else {
                            refactorComparingToZero(node, comparisonMI, GREATER);
                        }
                    } else if (hasOperator(node, NOT_EQUALS)) {
                        if (doubleValue < 0) {
                            refactorComparingToZero(node, comparisonMI, GREATER_EQUALS);
                        } else {
                            refactorComparingToZero(node, comparisonMI, LESS_EQUALS);
                        }
                    } else {
                        return VISIT_SUBTREE;
                    }

                    return DO_NOT_VISIT_SUBTREE;
                }
                return VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private void refactorComparingToZero(final InfixExpression node, final MethodInvocation comparisonMI,
            final Operator operator) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
            b.infixExpr(
                b.copy(comparisonMI),
                operator,
                b.number("0")));
    }
}
