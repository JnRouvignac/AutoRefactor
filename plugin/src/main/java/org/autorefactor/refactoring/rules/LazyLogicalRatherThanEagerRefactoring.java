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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isPassive;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;

/** See {@link #getDescription()} method. */
public class LazyLogicalRatherThanEagerRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Lazy logical rather than eager";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace & by && and | by || when the right operand is passive.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the time performance.";
    }

    @Override
    public boolean visit(InfixExpression node) {
        if (!node.hasExtendedOperands()
                && (hasType(node.getLeftOperand(), "boolean") || hasType(node.getLeftOperand(), "java.lang.Boolean"))
                && (hasType(node.getRightOperand(), "boolean") || hasType(node.getRightOperand(), "java.lang.Boolean"))
                && isPassive(node.getRightOperand())
                && (Operator.AND.equals(node.getOperator()) || Operator.OR.equals(node.getOperator()))) {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node,
                    b.infixExpr(
                            b.copy(node.getLeftOperand()),
                            getLazyOperator(node.getOperator()),
                            b.copy(node.getRightOperand())));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private Operator getLazyOperator(final Operator operator) {
        if (Operator.AND.equals(operator)) {
            return Operator.CONDITIONAL_AND;
        } else {
            return Operator.CONDITIONAL_OR;
        }
    }
}
