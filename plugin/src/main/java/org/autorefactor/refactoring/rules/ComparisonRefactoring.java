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
import static org.autorefactor.refactoring.ASTHelper.checkNoExtendedOperands;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;

/** See {@link #getDescription()} method. */
public class ComparisonRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Fix Comparable.compareTo() usage.";
    }

    @Override
    public String getName() {
        return "Comparison to 0 rather than 1 or -1";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            // TODO JNR handle same class calls and sub classes
            return VISIT_SUBTREE;
        }
        if (isMethod(node, "java.lang.Comparable", "compareTo", "java.lang.Object")) {
            return replaceInfixExpressionIfNeeded(node.getParent());
        } else if (isMethod(node, "java.lang.Comparator", "compare", "java.lang.Object", "java.lang.Object")) {
            return replaceInfixExpressionIfNeeded(node.getParent());
        } else if (getJavaMinorVersion() >= 2
                && isMethod(node, "java.lang.String", "compareToIgnoreCase", "java.lang.String")) {
            return replaceInfixExpressionIfNeeded(node.getParent());
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInfixExpressionIfNeeded(ASTNode expr) {
        if (expr instanceof ParenthesizedExpression) {
            return replaceInfixExpressionIfNeeded(expr.getParent());
        } else if (expr instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) expr;
            checkNoExtendedOperands(ie);
            final Object value = ie.getRightOperand().resolveConstantExpressionValue();
            if (value instanceof Number) {
                final Number nb = (Integer) value;
                if (nb.doubleValue() == 0) {
                    return VISIT_SUBTREE;
                }
                if (hasOperator(ie, EQUALS)) {
                    if (nb.doubleValue() < 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, LESS);
                    } else if (nb.doubleValue() > 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, GREATER);
                    }
                } else if (hasOperator(ie, NOT_EQUALS)) {
                    if (nb.doubleValue() < 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, GREATER_EQUALS);
                    } else if (nb.doubleValue() > 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, LESS_EQUALS);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithCorrectCheckOnCompareTo(final InfixExpression ie, final Operator operator) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(ie,
            b.infixExpr(
                b.copy(ie.getLeftOperand()),
                operator,
                b.number("0")));
        return DO_NOT_VISIT_SUBTREE;
    }
}
