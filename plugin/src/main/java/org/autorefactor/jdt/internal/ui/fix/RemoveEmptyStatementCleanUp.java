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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
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
public class RemoveEmptyStatementCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveEmptyStatementCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveEmptyStatementCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveEmptyStatementCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        if (ASTNodes.isPassive(node.getExpression())) {
            final boolean isThenEmpty= isEmptyCode(node.getThenStatement());
            final boolean isElseEmpty= isEmptyCode(node.getElseStatement());

            if (isThenEmpty && (isElseEmpty || node.getElseStatement() == null)) {
                this.ctx.getRefactorings().remove(node);
                return false;
            }
            if (isElseEmpty) {
                this.ctx.getRefactorings().remove(node.getElseStatement());
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean visit(final EnhancedForStatement node) {
        return !ASTNodes.isPassive(node.getExpression()) || !node.getExpression().resolveTypeBinding().isArray() || maybeRemoveStmtWithEmptyBody(node, node.getBody());
    }

    @Override
    public boolean visit(final ForStatement node) {
        return node.getExpression() == null || Boolean.TRUE.equals(node.getExpression().resolveConstantExpressionValue()) || !arePassive(node.initializers()) || !ASTNodes.isPassive(node.getExpression()) || maybeRemoveStmtWithEmptyBody(node, node.getBody());
    }

    @Override
    public boolean visit(final WhileStatement node) {
        return !ASTNodes.isPassive(node.getExpression()) || Boolean.TRUE.equals(node.getExpression().resolveConstantExpressionValue()) || maybeRemoveStmtWithEmptyBody(node, node.getBody());
    }

    @Override
    public boolean visit(final DoStatement node) {
        return !ASTNodes.isPassive(node.getExpression()) || Boolean.TRUE.equals(node.getExpression().resolveConstantExpressionValue()) || maybeRemoveStmtWithEmptyBody(node, node.getBody());
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
        final ASTNode parent= node.getParent();

        if (parent instanceof Block && isEmptyCode(node)) {
            this.ctx.getRefactorings().remove(node);
            return false;
        }

        return true;
    }

    private boolean arePassive(final List<?> initializers) {
        if (initializers != null) {
            for (Object initializer : initializers) {
                if (!ASTNodes.isPassive((Expression) initializer)) {
                    return false;
                }
            }
        }
        return true;
    }

    private boolean maybeRemoveStmtWithEmptyBody(final Statement node, final Statement emptyCode) {
        if (isEmptyCode(emptyCode)) {
            this.ctx.getRefactorings().remove(node);
            return false;
        }
        return true;
    }

    private boolean isEmptyCode(final Statement emptyCode) {
        if (emptyCode instanceof EmptyStatement) {
            return true;
        }
        if (emptyCode instanceof Block) {
            final Block block= (Block) emptyCode;
            return block.statements() == null || block.statements().isEmpty();
        }
        return false;
    }
}
