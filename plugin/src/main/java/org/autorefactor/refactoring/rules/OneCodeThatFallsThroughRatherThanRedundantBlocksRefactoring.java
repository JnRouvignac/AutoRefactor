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

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.BlockSubVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class OneCodeThatFallsThroughRatherThanRedundantBlocksRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "One code that falls through rather than redundant blocks";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Merge blocks that end with a jump statement into the following same code.";
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
                final List<Statement> redundantStmts = new ArrayList<Statement>();
                for (final CatchClause catchClause : (List<CatchClause>) node.catchClauses()) {
                    redundantStmts.add(catchClause.getBody());
                }

                return maybeRemoveRedundantCode(node, redundantStmts);
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(IfStatement node) {
            if (getResult() == VISIT_SUBTREE) {
                final List<Statement> redundantStmts = new ArrayList<Statement>();
                redundantStmts.add(node.getThenStatement());
                extractStmt(node, redundantStmts);

                return maybeRemoveRedundantCode(node, redundantStmts);
            }
            return VISIT_SUBTREE;
        }

        private void extractStmt(final IfStatement node, final List<Statement> redundantStmts) {
            Statement subIfStmt = node.getElseStatement();
            if (subIfStmt != null) {
                if (subIfStmt instanceof IfStatement) {
                    redundantStmts.add(((IfStatement) subIfStmt).getThenStatement());
                    extractStmt((IfStatement) subIfStmt, redundantStmts);
                } else {
                    redundantStmts.add(subIfStmt);
                }
            }
        }

        private boolean maybeRemoveRedundantCode(final Statement node, final List<Statement> redundantStmts) {
            final List<Statement> referenceStmts = new ArrayList<Statement>();

            Statement nextSibling = getNextSibling(node);
            while (nextSibling != null && !fallsThrough(nextSibling)) {
                referenceStmts.add(nextSibling);
                nextSibling = getNextSibling(nextSibling);
            }

            if (nextSibling != null) {
                referenceStmts.add(nextSibling);

                for (final Statement redundantStmt : redundantStmts) {
                    List<Statement> stmtsToCompare = asList(redundantStmt);
                    if (stmtsToCompare.size() > referenceStmts.size()) {
                        stmtsToCompare = stmtsToCompare.subList(stmtsToCompare.size() - referenceStmts.size(),
                                stmtsToCompare.size());
                    }

                    if (match(referenceStmts, stmtsToCompare)) {
                        Refactorings r = ctx.getRefactorings();
                        if (redundantStmt instanceof Block) {
                            r.remove(stmtsToCompare);
                        } else {
                            ASTBuilder b = ctx.getASTBuilder();
                            r.replace(redundantStmt, b.block());

                        }
                        setResult(DO_NOT_VISIT_SUBTREE);
                    }
                }

                return getResult();
            }
            return VISIT_SUBTREE;
        }
    }
}
