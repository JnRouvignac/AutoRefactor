/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.*;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/**
 * Collapses two consecutive if statements into just one.
 */
public class CollapseIfStatementRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private RefactoringContext ctx;

    public CollapseIfStatementRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        if (node.getElseStatement() == null) {
            final IfStatement is = as(node.getThenStatement(), IfStatement.class);
            if (is != null) {
                replaceIfNoElseStatement(node, is);
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceIfNoElseStatement(IfStatement outerIf,
            IfStatement innerIf) {
        if (innerIf.getElseStatement() != null) {
            return VISIT_SUBTREE;
        }

        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression leftOperand = b.copyExpr(outerIf.getExpression());
        final Expression rightOperand = b.copyExpr(innerIf.getExpression());

        final InfixExpression ie = b.infixExpr(
            parenthesizeInfixExpr(leftOperand),
            CONDITIONAL_AND,
            parenthesizeInfixExpr(rightOperand));

        final IfStatement is = b.if0(ie,
            b.copyStmt(innerIf.getThenStatement()));
        this.ctx.getRefactorings().replace(outerIf, is);
        return DO_NOT_VISIT_SUBTREE;
    }

    private Expression parenthesizeInfixExpr(Expression expr) {
        if (expr instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) expr;
            if (CONDITIONAL_OR.equals(ie.getOperator())) {
                return this.ctx.getASTBuilder().parenthesize(ie);
            }
        }
        return expr;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}
