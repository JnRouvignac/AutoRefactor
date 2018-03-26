/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.getParentIgnoring;
import static org.autorefactor.refactoring.ASTHelper.is;
import static org.autorefactor.refactoring.ASTHelper.isPassive;

import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Removes empty statements.
 *
 * @see #getDescription()
 */
public class RemoveEmptyStatementRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Removes empty statements";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Removes lone semicolons.";
    }

    @Override
    public boolean visit(EmptyStatement node) {
        ASTNode parent = node.getParent();
        if (parent instanceof Block) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        parent = getParentIgnoring(node, Block.class);
        if (parent instanceof IfStatement) {
            final IfStatement is = (IfStatement) parent;
            if (isPassive(is.getExpression())) {
                final List<Statement> thenStmts = asList(is.getThenStatement());
                final List<Statement> elseStmts = asList(is.getElseStatement());
                final boolean thenIsEmptyStmt = thenStmts.size() == 1 && is(thenStmts.get(0), EmptyStatement.class);
                final boolean elseIsEmptyStmt = elseStmts.size() == 1 && is(elseStmts.get(0), EmptyStatement.class);
                if (thenIsEmptyStmt && elseIsEmptyStmt) {
                    this.ctx.getRefactorings().remove(parent);
                    return DO_NOT_VISIT_SUBTREE;
                } else if (thenIsEmptyStmt && is.getElseStatement() == null) {
                    this.ctx.getRefactorings().remove(is);
                    return DO_NOT_VISIT_SUBTREE;
                } else if (elseIsEmptyStmt) {
                    this.ctx.getRefactorings().remove(is.getElseStatement());
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        } else if (parent instanceof EnhancedForStatement) {
            final EnhancedForStatement efs = (EnhancedForStatement) parent;
            if (isPassive(efs.getExpression()) && efs.getExpression().resolveTypeBinding().isArray()) {
                return maybeRemoveEmptyStmtBody(node, efs, efs.getBody());
            }
        } else if (parent instanceof ForStatement) {
            final ForStatement fs = (ForStatement) parent;
            if (arePassive(fs.initializers()) && isPassive(fs.getExpression())) {
                return maybeRemoveEmptyStmtBody(node, fs, fs.getBody());
            }
        } else if (parent instanceof WhileStatement) {
            final WhileStatement ws = (WhileStatement) parent;
            if (isPassive(ws.getExpression())
                    && !Boolean.TRUE.equals(ws.getExpression().resolveConstantExpressionValue())) {
                return maybeRemoveEmptyStmtBody(node, ws, ws.getBody());
            }
        } else if (parent instanceof DoStatement) {
            final DoStatement ds = (DoStatement) parent;
            if (isPassive(ds.getExpression())
                    && !Boolean.TRUE.equals(ds.getExpression().resolveConstantExpressionValue())) {
                return maybeRemoveEmptyStmtBody(node, ds, ds.getBody());
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean arePassive(final List<?> initializers) {
        for (final Object initializer : initializers) {
            if (!isPassive((Expression) initializer)) {
                return false;
            }
        }
        return true;
    }

    private boolean maybeRemoveEmptyStmtBody(final EmptyStatement node, final Statement stmt, final Statement body) {
        final List<Statement> bodyStmts = asList(body);
        if (bodyStmts.size() == 1 && bodyStmts.contains(node)) {
            this.ctx.getRefactorings().remove(stmt);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}
