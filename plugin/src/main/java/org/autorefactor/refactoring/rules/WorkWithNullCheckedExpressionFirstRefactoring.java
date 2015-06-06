/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.InfixExpression.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class WorkWithNullCheckedExpressionFirstRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Refactors if statements with a null checked expression"
            + " to work with the not null case in the then clause"
            + " and then work with the null case in the else clause.";
    }

    @Override
    public boolean visit(IfStatement node) {
        final Statement thenStmt = getThenStatement(node);
        final Statement elseStmt = getElseStatement(node, thenStmt);
        if (isNullCheck(node.getExpression())
                && thenStmt != null
                && elseStmt != null
                && thenStmt.getNodeType() == elseStmt.getNodeType()
                && simpleStmt(thenStmt)) {
            return revertIfStatement(node, thenStmt, elseStmt);
        }
        return VISIT_SUBTREE;
    }

    private Statement getThenStatement(IfStatement node) {
        final List<Statement> thenStmts = asList(node.getThenStatement());
        if (thenStmts.size() == 1) {
            return thenStmts.get(0);
        }
        return null;
    }

    private Statement getElseStatement(IfStatement node, Statement thenStmt) {
        final List<Statement> elseStmts = asList(node.getElseStatement());
        if (elseStmts.size() == 1) {
            return elseStmts.get(0);
        }
        if (is(thenStmt, ReturnStatement.class)) {
            return getNextSibling(node);
        }
        return null;
    }

    private boolean isNullCheck(Expression ifExpression) {
        final InfixExpression condition = as(ifExpression, InfixExpression.class);
        return hasOperator(condition, EQUALS)
                && !condition.hasExtendedOperands()
                && (isNullLiteral(condition.getLeftOperand()) || isNullLiteral(condition.getRightOperand()));
    }

    private boolean simpleStmt(Statement stmt) {
        switch (stmt.getNodeType()) {
        case ASTNode.IF_STATEMENT:
        case ASTNode.DO_STATEMENT:
        case ASTNode.WHILE_STATEMENT:
        case ASTNode.FOR_STATEMENT:
        case ASTNode.ENHANCED_FOR_STATEMENT:
        case ASTNode.TRY_STATEMENT:
            return false;

        default:
            return true;
        }
    }

    /** Revert condition + swap then and else statements.  */
    private boolean revertIfStatement(IfStatement node, Statement thenStmt, Statement elseStmt) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        r.set(node.getExpression(), OPERATOR_PROPERTY, NOT_EQUALS);
        r.replace(thenStmt, b.move(elseStmt));
        r.replace(elseStmt, b.move(thenStmt));
        return DO_NOT_VISIT_SUBTREE;
    }
}
