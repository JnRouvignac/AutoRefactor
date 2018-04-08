/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.getLocalVariableIdentifiers;
import static org.autorefactor.refactoring.ASTHelper.getNextSiblings;
import static org.autorefactor.refactoring.ASTHelper.statements;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class RemoveUselessBlockRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove useless block";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Removes lone and embedded block.";
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
    public boolean visit(Block node) {
        final List<Statement> stmts = statements(node);
        if (stmts.size() == 1
                && stmts.get(0) instanceof Block) {
            replaceBlock((Block) stmts.get(0));
            return DO_NOT_VISIT_SUBTREE;
        } else if (node.getParent() instanceof Block) {
            final Set<String> ifVariableNames = getLocalVariableIdentifiers(node, false);

            final Set<String> followingVariableNames = new HashSet<String>();
            for (final Statement statement : getNextSiblings(node)) {
                followingVariableNames.addAll(getLocalVariableIdentifiers(statement, true));
            }

            if (!ifVariableNames.removeAll(followingVariableNames)) {
                replaceBlock(node);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    @SuppressWarnings("unchecked")
    private void replaceBlock(final Block node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        r.replace(node, b.copyRange(node.statements()));
    }
}
