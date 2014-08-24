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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Removes dead code. Use variable values analysis for determining where code is dead.
 */
public class DeadCodeEliminationRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private RefactoringContext ctx;

    /** Default constructor. */
    public DeadCodeEliminationRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    @Override
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    // TODO JNR
    // for (false) // impossible iterations
    // Remove Empty try block?
    // do this by resolvingConstantValue

    // TODO JNR remove such code:
    // public void myMethod() {
    // super.myMethod();
    // }
    // only do it when there are no annotations attached to the overriding method.

    /** {@inheritDoc} */
    @Override
    public boolean visit(Block node) {
        if (!statements(node).isEmpty()) {
            return VISIT_SUBTREE;
        }
        final ASTNode parent = node.getParent();
        if (parent instanceof IfStatement) {
            final IfStatement is = (IfStatement) parent;
            if (is.getElseStatement() == node) {
                this.ctx.getRefactorings().remove(node);
                return DO_NOT_VISIT_SUBTREE;
            } // TODO handle empty then clause
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        final Object constantCondition =
                node.getExpression().resolveConstantExpressionValue();
        final ASTBuilder b = this.ctx.getASTBuilder();
        if (Boolean.TRUE.equals(constantCondition)) {
            this.ctx.getRefactorings().replace(node, b.copyStmt(node.getThenStatement()));
            return DO_NOT_VISIT_SUBTREE;
        } else if (Boolean.FALSE.equals(constantCondition)) {
            this.ctx.getRefactorings().replace(node, b.copyStmt(node.getElseStatement()));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(WhileStatement node) {
        final Object constantCondition =
                node.getExpression().resolveConstantExpressionValue();
        if (Boolean.FALSE.equals(constantCondition)) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TryStatement node) {
        final List<Statement> tryStmts = asList(node.getBody());
        if (tryStmts.isEmpty()) {
            final List<Statement> finallyStmts = asList(node.getFinally());
            if (!finallyStmts.isEmpty()) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                this.ctx.getRefactorings().replace(node, b.copyStmt(node.getFinally()));
            } else {
                this.ctx.getRefactorings().remove(node);
            }
            return DO_NOT_VISIT_SUBTREE;
        }
    // }else {
    // for (CatchClause catchClause : (List<CatchClause>) node.catchClauses()) {
    // final List<Statement> finallyStmts = asList(catchClause.getBody());
    // if (finallyStmts.isEmpty()) {
    // // TODO cannot remove without checking what subsequent catch clauses are
    // catching
    // this.ctx.getRefactorings().remove(catchClause);
    // }
    // }
    //
    // final List<Statement> finallyStmts = asList(node.getFinally());
    // if (finallyStmts.isEmpty()) {
    // this.ctx.getRefactorings().remove(node.getFinally());
    // }
    // // TODO If all finally and catch clauses have been removed,
    // // then we can remove the whole try statement and replace it with a simple block
    // return DO_NOT_VISIT_SUBTREE; // TODO JNR is this correct?
    // }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}
