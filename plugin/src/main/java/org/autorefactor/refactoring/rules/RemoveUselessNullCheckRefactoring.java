/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;

/**
 * Refactoring removing useless null checks before assignments or return statements.
 * Such useless null checks are comparing an expression against null,
 * then either assigning null or the expression depending on the result of the null check.
 * It is simpler to directly assign the expression.
 */
public class RemoveUselessNullCheckRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private RefactoringContext ctx;
    private final ASTMatcher matcher = new ASTMatcher();

    /** Default constructor. */
    public RemoveUselessNullCheckRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        final InfixExpression condition = as(node.getExpression(), InfixExpression.class);
        final List<Statement> thenStmts = asList(node.getThenStatement());
        final List<Statement> elseStmts = asList(node.getElseStatement());
        if (condition != null
                && thenStmts.size() == 1
                && elseStmts.size() == 1) {
            final Assignment thenAs = asExpression(thenStmts.get(0), Assignment.class);
            final Assignment elseAs = asExpression(elseStmts.get(0), Assignment.class);
            if (thenAs != null
                    && elseAs != null
                    && ASSIGN.equals(thenAs.getOperator())
                    && ASSIGN.equals(elseAs.getOperator())
                    && match(matcher, thenAs.getLeftHandSide(), elseAs.getLeftHandSide())) {
                boolean thenStmtAssignsNull = as(thenAs.getRightHandSide(), NullLiteral.class) != null;
                boolean elseStmtAssignsNull = as(elseAs.getRightHandSide(), NullLiteral.class) != null;
                if (InfixExpression.Operator.EQUALS.equals(condition.getOperator())
                        && thenStmtAssignsNull) {
                    return replaceWithStraightAssign(node, condition, elseAs);
                } else if (InfixExpression.Operator.NOT_EQUALS.equals(condition.getOperator())
                        && elseStmtAssignsNull) {
                    return replaceWithStraightAssign(node, condition, thenAs);
                }
            } else {
                final ReturnStatement thenRS = as(thenStmts.get(0), ReturnStatement.class);
                final ReturnStatement elseRS = as(elseStmts.get(0), ReturnStatement.class);
                if (thenRS != null && elseRS != null) {
                    if (InfixExpression.Operator.EQUALS.equals(condition.getOperator())) {
                        return replaceWithStraightReturn(node, condition, elseRS);
                    } else if (InfixExpression.Operator.NOT_EQUALS.equals(condition.getOperator())) {
                        return replaceWithStraightReturn(node, condition, thenRS);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithStraightAssign(IfStatement node, InfixExpression condition, Assignment as) {
        boolean conditionLeftOpIsNull = as(condition.getLeftOperand(), NullLiteral.class) != null;
        boolean conditionRightOpIsNull = as(condition.getRightOperand(), NullLiteral.class) != null;
        if (conditionRightOpIsNull
                && match(matcher, condition.getLeftOperand(), as.getRightHandSide())) {
            return replaceWithStraightAssign(node, as.getLeftHandSide(), condition.getLeftOperand());
        } else if (conditionLeftOpIsNull
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
                        b.copyExpr(leftHandSide),
                        Assignment.Operator.ASSIGN,
                        b.copyExpr(rightHandSide))));
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean replaceWithStraightReturn(IfStatement node, InfixExpression condition, ReturnStatement rs) {
        boolean conditionLeftOpIsNull = as(condition.getLeftOperand(), NullLiteral.class) != null;
        boolean conditionRightOpIsNull = as(condition.getRightOperand(), NullLiteral.class) != null;
        if (conditionRightOpIsNull
                && match(matcher, condition.getLeftOperand(), rs.getExpression())) {
            return replaceWithStraightReturn(node, condition.getLeftOperand());
        } else if (conditionLeftOpIsNull
                && match(matcher, condition.getRightOperand(), rs.getExpression())) {
            return replaceWithStraightReturn(node, condition.getRightOperand());
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithStraightReturn(IfStatement node, Expression returnedExpr) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node,
                b.return0(b.copyExpr(returnedExpr)));
        return DO_NOT_VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}
