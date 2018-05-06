/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.getPreviousSibling;
import static org.autorefactor.refactoring.ASTHelper.match;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class DoWhileRatherThanDuplicateCodeRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Do/while rather than duplicate code";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Replace while by do/while when the loop statements are duplicated before the loop.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and testing cost.";
    }

    @Override
    public boolean visit(WhileStatement node) {
        final List<Statement> whileStmts = asList(node.getBody());

        if (whileStmts.isEmpty()) {
            return VISIT_SUBTREE;
        }

        final List<Statement> previousStmts = new ArrayList<Statement>(whileStmts.size());
        final ASTSemanticMatcher matcher = new ASTSemanticMatcher();

        Statement previousStmt = getPreviousSibling(node);
        int i = whileStmts.size() - 1;
        while (i >= 0) {
            if (previousStmt == null || !match(matcher, previousStmt,
                    whileStmts.get(i))) {
                return VISIT_SUBTREE;
            }
            i--;
            previousStmts.add(previousStmt);
            previousStmt = getPreviousSibling(previousStmt);
        }

        replaceWithDoWhile(node, previousStmts);
        return DO_NOT_VISIT_SUBTREE;
    }

    private void replaceWithDoWhile(final WhileStatement node, final List<Statement> previousStmts) {
        final Refactorings r = this.ctx.getRefactorings();
        r.remove(previousStmts);

        final ASTBuilder b = this.ctx.getASTBuilder();
        r.replace(node, b.doWhile(b.copy(node.getExpression()), b.copy(node.getBody())));
    }
}
