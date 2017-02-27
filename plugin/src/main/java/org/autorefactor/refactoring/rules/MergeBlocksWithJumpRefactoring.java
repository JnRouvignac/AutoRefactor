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
import static org.autorefactor.refactoring.ASTHelper.getNextSibling;
import static org.autorefactor.refactoring.ASTHelper.match;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.BlockSubVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThrowStatement;

/** See {@link #getDescription()} method. */
public class MergeBlocksWithJumpRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return "Merge following if statements with same code block that end with a jump statement.";
    }

    @Override
    public String getName() {
        return "One block with jump rather than duplicate blocks";
    }

    @Override
    public boolean visit(Block node) {
        final SuccessiveIfVisitor returnStatementVisitor = new SuccessiveIfVisitor(ctx, node);
        node.accept(returnStatementVisitor);
        return returnStatementVisitor.getResult();
    }

    private static final class SuccessiveIfVisitor extends BlockSubVisitor {

        public SuccessiveIfVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(IfStatement node) {
            if (getResult() == VISIT_SUBTREE) {
                final List<IfStatement> duplicateIfBlocks = new ArrayList<IfStatement>();
                duplicateIfBlocks.add(node);
                while (addOneMoreIf(duplicateIfBlocks)) {
                    // OK continue
                }

                if (duplicateIfBlocks.size() > 1) {
                    mergeCode(duplicateIfBlocks);

                    setResult(DO_NOT_VISIT_SUBTREE);
                    return DO_NOT_VISIT_SUBTREE;
                }
                return VISIT_SUBTREE;
            }
            return DO_NOT_VISIT_SUBTREE;
        }

        private boolean addOneMoreIf(final List<IfStatement> duplicateIfBlocks) {
            if (duplicateIfBlocks.get(duplicateIfBlocks.size() - 1).getElseStatement() == null) {
                final Statement nextSibling = getNextSibling(duplicateIfBlocks.get(duplicateIfBlocks.size() - 1));

                if (nextSibling != null
                        && nextSibling instanceof IfStatement
                        && ((IfStatement) nextSibling).getElseStatement() == null
                        && !getCtx().getRefactorings().hasBeenRefactored(nextSibling)) {
                    final IfStatement nextIf = (IfStatement) nextSibling;

                    final List<Statement> lastIfStmts = asList(duplicateIfBlocks.get(duplicateIfBlocks.size() - 1)
                            .getThenStatement());
                    final List<Statement> nextIfStmts = asList(nextIf.getThenStatement());
                    if (lastIfStmts != null && !lastIfStmts.isEmpty()
                            && breaksControlFlow(lastIfStmts.get(lastIfStmts.size() - 1))
                            && isSameCode(lastIfStmts, nextIfStmts)) {
                        duplicateIfBlocks.add(nextIf);
                        return true;
                    }
                }
            }

            return false;
        }

        private void mergeCode(final List<IfStatement> duplicateIfBlocks) {
            final ASTBuilder b = getCtx().getASTBuilder();
            final Refactorings r = getCtx().getRefactorings();

            Iterator<IfStatement> iterator = duplicateIfBlocks.iterator();
            Expression newCondition = b.parenthesizeIfNeeded(b.copy(iterator.next()
                    .getExpression()));

            while (iterator.hasNext()) {
                newCondition = b.infixExpr(newCondition,
                        InfixExpression.Operator.CONDITIONAL_OR, b.parenthesizeIfNeeded(b.copy(iterator.next()
                                .getExpression())));
            }

            final IfStatement newIf = b.if0(newCondition,
                    b.copy(duplicateIfBlocks.get(0).getThenStatement()));

            iterator = duplicateIfBlocks.iterator();
            r.replace(iterator.next(), newIf);
            while (iterator.hasNext()) {
                r.remove(iterator.next());
            }
        }

        private static boolean breaksControlFlow(final Statement stmt) {
            return stmt instanceof ReturnStatement
                    || stmt instanceof BreakStatement
                    || stmt instanceof ContinueStatement
                    || stmt instanceof ThrowStatement;
        }

        private boolean isSameCode(final List<Statement> referenceStmts, final List<Statement> comparedStmts) {
            if (referenceStmts.size() != comparedStmts.size()) {
                return false;
            }

            final ASTMatcher matcher = new ASTMatcher();

            for (int codeLine = 0; codeLine < referenceStmts.size(); codeLine++) {
                if (!match(matcher, referenceStmts.get(codeLine),
                        comparedStmts.get(codeLine))) {
                    return false;
                }
            }
            return true;
        }
    }
}
