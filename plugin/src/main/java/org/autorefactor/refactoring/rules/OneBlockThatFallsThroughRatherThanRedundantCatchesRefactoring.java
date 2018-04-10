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
import static org.autorefactor.refactoring.ASTHelper.fallsThrough;
import static org.autorefactor.refactoring.ASTHelper.getNextSibling;
import static org.autorefactor.refactoring.ASTHelper.match;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.refactoring.BlockSubVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class OneBlockThatFallsThroughRatherThanRedundantCatchesRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "One block that falls through rather than redundant catches";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Merge catch statements that end with a jump statement into the following same code.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and tesing cost.";
    }

    @Override
    public boolean visit(Block node) {
        final CatchesAndFollowingCodeVisitor catchesAndFollowingCodeVisitor =
                new CatchesAndFollowingCodeVisitor(ctx, node);
        node.accept(catchesAndFollowingCodeVisitor);
        return catchesAndFollowingCodeVisitor.getResult();
    }

    private static final class CatchesAndFollowingCodeVisitor extends BlockSubVisitor {

        public CatchesAndFollowingCodeVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(TryStatement node) {
            if (getResult() == VISIT_SUBTREE && node.getFinally() == null) {
                final List<Statement> referenceStmts = new ArrayList<Statement>();

                Statement nextSibling = getNextSibling(node);
                while (nextSibling != null && !fallsThrough(nextSibling)) {
                    referenceStmts.add(nextSibling);
                    nextSibling = getNextSibling(nextSibling);
                }

                if (nextSibling != null) {
                    referenceStmts.add(nextSibling);

                    final List<CatchClause> catchClauses = node.catchClauses();
                    for (final CatchClause catchClause : catchClauses) {
                        List<Statement> catchStmts = asList(catchClause.getBody());
                        if (catchStmts.size() > referenceStmts.size()) {
                            catchStmts = catchStmts.subList(catchStmts.size() - referenceStmts.size(),
                                    catchStmts.size());
                        }

                        if (match(referenceStmts, catchStmts)) {
                            getCtx().getRefactorings().remove(catchStmts);
                            setResult(DO_NOT_VISIT_SUBTREE);
                        }
                    }

                    return getResult();
                }
            }
            return VISIT_SUBTREE;
        }
    }
}
