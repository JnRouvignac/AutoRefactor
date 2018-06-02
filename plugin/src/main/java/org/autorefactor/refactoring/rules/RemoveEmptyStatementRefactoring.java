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
        return "Removes structural statements with no substatement.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    @Override
    public boolean visit(final IfStatement node) {
        if (isPassive(node.getExpression())) {
            final boolean isThenEmpty = isEmptyCode(node.getThenStatement());
            final boolean isElseEmpty = isEmptyCode(node.getElseStatement());

            if (isThenEmpty && (isElseEmpty || node.getElseStatement() == null)) {
                this.ctx.getRefactorings().remove(node);
                return DO_NOT_VISIT_SUBTREE;
            } else if (isElseEmpty) {
                this.ctx.getRefactorings().remove(node.getElseStatement());
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(final EnhancedForStatement node) {
        if (isPassive(node.getExpression()) && node.getExpression().resolveTypeBinding().isArray()) {
            return maybeRemoveStmtWithEmptyBody(node, node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(final ForStatement node) {
        if (node.getExpression() != null && !Boolean.TRUE.equals(node.getExpression().resolveConstantExpressionValue())
                && arePassive(node.initializers()) && isPassive(node.getExpression())) {
            return maybeRemoveStmtWithEmptyBody(node, node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(final WhileStatement node) {
        if (isPassive(node.getExpression())
                && !Boolean.TRUE.equals(node.getExpression().resolveConstantExpressionValue())) {
            return maybeRemoveStmtWithEmptyBody(node, node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(final DoStatement node) {
        if (isPassive(node.getExpression())
                && !Boolean.TRUE.equals(node.getExpression().resolveConstantExpressionValue())) {
            return maybeRemoveStmtWithEmptyBody(node, node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(final Block node) {
        return maybeRemoveCode(node);
    }

    @Override
    public boolean visit(final EmptyStatement node) {
        return maybeRemoveCode(node);
    }

    private boolean maybeRemoveCode(final Statement node) {
        final ASTNode parent = node.getParent();

        if (parent instanceof Block && isEmptyCode(node)) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private boolean arePassive(final List<?> initializers) {
        if (initializers != null) {
            for (final Object initializer : initializers) {
                if (!isPassive((Expression) initializer)) {
                    return false;
                }
            }
        }
        return true;
    }

    private boolean maybeRemoveStmtWithEmptyBody(final Statement node, final Statement emptyCode) {
        if (isEmptyCode(emptyCode)) {
            this.ctx.getRefactorings().remove(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean isEmptyCode(final Statement emptyCode) {
        if (emptyCode instanceof EmptyStatement) {
            return true;
        } else if (emptyCode instanceof Block) {
            final Block block = (Block) emptyCode;
            return block.statements() == null || block.statements().isEmpty();
        }
        return false;
    }
}
