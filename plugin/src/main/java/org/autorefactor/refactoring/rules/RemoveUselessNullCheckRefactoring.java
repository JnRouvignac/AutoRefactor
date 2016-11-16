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
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class RemoveUselessNullCheckRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Removes useless null checks before assignments or return statements.\n"
            + "Such useless null checks are comparing an expression against null,\n"
            + "then either assigning null or the expression depending on the result of the null check.\n"
            + "It is simpler to directly assign the expression.";
    }

    @Override
    public String getName() {
        return "Remove useless null checks";
    }

    private final ASTMatcher matcher = new ASTMatcher();

    @Override
    public boolean visit(IfStatement node) {
        final InfixExpression condition = as(node.getExpression(), InfixExpression.class);
        final Statement thenStmt = getThenStatement(node);
        final Statement elseStmt = getElseStatement(node, thenStmt);
        if (condition != null
                && !condition.hasExtendedOperands()
                && thenStmt != null
                && elseStmt != null) {
            final Assignment thenAs = asExpression(thenStmt, Assignment.class);
            final Assignment elseAs = asExpression(elseStmt, Assignment.class);
            if (hasOperator(thenAs, ASSIGN)
                    && hasOperator(elseAs, ASSIGN)
                    && match(matcher, thenAs.getLeftHandSide(), elseAs.getLeftHandSide())) {
                if (hasOperator(condition, EQUALS)
                        && isNullLiteral(thenAs.getRightHandSide())) {
                    return replaceWithStraightAssign(node, condition, elseAs);
                } else if (hasOperator(condition, NOT_EQUALS)
                        && isNullLiteral(elseAs.getRightHandSide())) {
                    return replaceWithStraightAssign(node, condition, thenAs);
                }
            } else {
                final ReturnStatement thenRS = as(thenStmt, ReturnStatement.class);
                final ReturnStatement elseRS = as(elseStmt, ReturnStatement.class);
                if (thenRS != null && elseRS != null) {
                    if (hasOperator(condition, EQUALS)) {
                        return replaceWithStraightReturn(node, condition, elseRS, thenRS, elseRS);
                    } else if (hasOperator(condition, NOT_EQUALS)) {
                        return replaceWithStraightReturn(node, condition, thenRS, elseRS, elseRS);
                    }
                }
            }
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

    private boolean replaceWithStraightAssign(IfStatement node, InfixExpression condition, Assignment as) {
        if (isNullLiteral(condition.getRightOperand())
                && match(matcher, condition.getLeftOperand(), as.getRightHandSide())) {
            return replaceWithStraightAssign(node, as.getLeftHandSide(), condition.getLeftOperand());
        } else if (isNullLiteral(condition.getLeftOperand())
                && match(matcher, condition.getRightOperand(), as.getRightHandSide())) {
            return replaceWithStraightAssign(node, as.getLeftHandSide(), condition.getRightOperand());
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithStraightAssign(IfStatement node,
            Expression leftHandSide, Expression rightHandSide) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
                b.toStmt(b.assign(
                        b.copy(leftHandSide),
                        ASSIGN,
                        b.copy(rightHandSide))));
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean replaceWithStraightReturn(IfStatement node, InfixExpression condition,
            ReturnStatement rs, ReturnStatement otherRs, Statement toRemove) {
        if (isNullLiteral(otherRs.getExpression())) {
            if (isNullLiteral(condition.getRightOperand())
                    && match(matcher, condition.getLeftOperand(), rs.getExpression())) {
                this.ctx.getRefactorings().remove(toRemove);
                return replaceWithStraightReturn(node, condition.getLeftOperand());
            } else if (isNullLiteral(condition.getLeftOperand())
                    && match(matcher, condition.getRightOperand(), rs.getExpression())) {
                this.ctx.getRefactorings().remove(toRemove);
                return replaceWithStraightReturn(node, condition.getRightOperand());
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithStraightReturn(IfStatement node, Expression returnedExpr) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
                b.return0(b.copy(returnedExpr)));
        return DO_NOT_VISIT_SUBTREE;
    }
}
