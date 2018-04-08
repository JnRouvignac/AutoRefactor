/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Inline the blocks
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
import static org.autorefactor.refactoring.ASTHelper.isPassive;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class RemoveEmptyIfRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove empty if";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Removes empty if or else block.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters."
                + " It also improves the time performance.";
    }

    @Override
    public boolean visit(IfStatement node) {
        final Refactorings r = this.ctx.getRefactorings();

        final Statement thenStmt = node.getThenStatement();
        final Statement elseStmt = node.getElseStatement();
        if (elseStmt != null && asList(elseStmt).isEmpty()) {
            r.remove(elseStmt);
            return DO_NOT_VISIT_SUBTREE;
        } else if (thenStmt != null && asList(thenStmt).isEmpty()) {
            final ASTBuilder b = this.ctx.getASTBuilder();

            final Expression condition = node.getExpression();
            if (elseStmt != null) {
                r.replace(node,
                          b.if0(b.negate(condition),
                                b.move(elseStmt)));
            } else if (isPassive(condition)) {
                removeBlock(node, r, b);
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    private void removeBlock(final IfStatement node, final Refactorings r, final ASTBuilder b) {
        if (node.getParent() instanceof IfStatement
                || node.getParent() instanceof EnhancedForStatement
                || node.getParent() instanceof ForStatement
                || node.getParent() instanceof WhileStatement
                || node.getParent() instanceof DoStatement) {
            r.replace(node,
                    b.block());
        } else {
            r.remove(node);
        }
    }
}
