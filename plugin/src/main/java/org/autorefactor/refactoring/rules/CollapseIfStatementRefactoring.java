/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;

/** See {@link #getDescription()} method. */
public class CollapseIfStatementRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Collapse if statements";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Merge inside if statement into the parent if statement.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and testing cost.";
    }

    @Override
    public boolean visit(IfStatement node) {
        if (node.getElseStatement() == null) {
            final IfStatement is = as(node.getThenStatement(), IfStatement.class);
            if (is != null) {
                return replaceIfNoElseStatement(node, is);
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceIfNoElseStatement(IfStatement outerIf, IfStatement innerIf) {
        if (innerIf.getElseStatement() != null) {
            return VISIT_SUBTREE;
        }

        final ASTBuilder b = this.ctx.getASTBuilder();
        final InfixExpression ie = b.infixExpr(
                parenthesizeOrExpr(b, outerIf.getExpression()),
                CONDITIONAL_AND,
                parenthesizeOrExpr(b, innerIf.getExpression()));
        this.ctx.getRefactorings().replace(outerIf.getExpression(), ie);
        this.ctx.getRefactorings().replace(outerIf.getThenStatement(), b.copy(innerIf.getThenStatement()));
        return DO_NOT_VISIT_SUBTREE;
    }

    private Expression parenthesizeOrExpr(ASTBuilder b, Expression expr) {
        if (expr instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) expr;
            if (hasOperator(ie, CONDITIONAL_OR)) {
                return b.parenthesize(b.copy(ie));
            }
        }
        return b.copy(expr);
    }
}
