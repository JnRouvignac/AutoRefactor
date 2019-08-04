/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017-2019 Fabrice Tiercelin - Initial API and implementation
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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.asList;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.fallsThrough;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getNextSibling;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.match;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_reason;
    }

    @Override
    public boolean visit(Block node) {
        final CatchesAndFollowingCodeVisitor catchesAndFollowingCodeVisitor= new CatchesAndFollowingCodeVisitor(ctx,
                node);
        node.accept(catchesAndFollowingCodeVisitor);
        return catchesAndFollowingCodeVisitor.getResult();
    }

    private static final class CatchesAndFollowingCodeVisitor extends BlockSubVisitor {
        public CatchesAndFollowingCodeVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(TryStatement node) {
            return visitStmt(node);
        }

        @Override
        public boolean visit(IfStatement node) {
            return visitStmt(node);
        }

        private boolean visitStmt(Statement node) {
            if (!getResult()) {
                return true;
            }

            final List<Statement> redundantStmts= new ArrayList<Statement>();
            collectStmts(node, redundantStmts);
            return maybeRemoveRedundantCode(node, redundantStmts);
        }

        @SuppressWarnings("unchecked")
        private void collectStmts(Statement node, final List<Statement> redundantStmts) {
            if (node == null) {
                return;
            }

            TryStatement ts= as(node, TryStatement.class);
            IfStatement is= as(node, IfStatement.class);

            if (ts != null && ts.getFinally() == null) {
                for (final CatchClause catchClause : (List<CatchClause>) ts.catchClauses()) {
                    doCollectStmts(catchClause.getBody(), redundantStmts);
                }
            } else if (is != null) {
                doCollectStmts(is.getThenStatement(), redundantStmts);
                doCollectStmts(is.getElseStatement(), redundantStmts);
            }
        }

        private void doCollectStmts(Statement node, final List<Statement> redundantStmts) {
            if (node == null) {
                return;
            }

            redundantStmts.add(node);
            List<Statement> stmts= asList(node);

            if (stmts == null || stmts.isEmpty()) {
                return;
            }

            node= stmts.get(stmts.size() - 1);
            collectStmts(node, redundantStmts);
        }

        private boolean maybeRemoveRedundantCode(final Statement node, final List<Statement> redundantStmts) {
            if (redundantStmts.isEmpty()) {
                return true;
            }

            final List<Statement> referenceStmts= new ArrayList<Statement>();

            Statement nextSibling= getNextSibling(node);
            while (nextSibling != null && !fallsThrough(nextSibling)) {
                referenceStmts.add(nextSibling);
                nextSibling= getNextSibling(nextSibling);
            }

            if (nextSibling != null) {
                referenceStmts.add(nextSibling);
                ASTBuilder b= ctx.getASTBuilder();

                for (final Statement redundantStmt : redundantStmts) {
                    List<Statement> stmtsToCompare= asList(redundantStmt);

                    if (stmtsToCompare.size() > referenceStmts.size()) {
                        stmtsToCompare= stmtsToCompare.subList(stmtsToCompare.size() - referenceStmts.size(),
                                stmtsToCompare.size());
                    }

                    if (match(referenceStmts, stmtsToCompare)) {
                        Refactorings r= ctx.getRefactorings();

                        if (redundantStmt instanceof Block) {
                            r.remove(stmtsToCompare);
                        } else {
                            r.replace(redundantStmt, b.block());
                        }

                        setResult(false);
                        return false;
                    }
                }
            }

            return true;
        }
    }
}
