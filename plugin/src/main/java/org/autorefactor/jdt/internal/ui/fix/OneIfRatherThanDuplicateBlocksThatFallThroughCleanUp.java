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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_reason;
    }

    @Override
    public boolean visit(Block node) {
        final SuccessiveIfVisitor successiveIfVisitor= new SuccessiveIfVisitor(ctx, node);
        node.accept(successiveIfVisitor);
        return successiveIfVisitor.getResult();
    }

    private static final class SuccessiveIfVisitor extends BlockSubVisitor {
        public SuccessiveIfVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(IfStatement node) {
            if (getResult()) {
                final List<IfStatement> duplicateIfBlocks= new ArrayList<>();
                duplicateIfBlocks.add(node);
                while (addOneMoreIf(duplicateIfBlocks)) {
                    // OK continue
                }

                if (duplicateIfBlocks.size() > 1) {
                    mergeCode(duplicateIfBlocks);

                    setResult(false);
                    return false;
                }

                return true;
            }

            return false;
        }

        private boolean addOneMoreIf(final List<IfStatement> duplicateIfBlocks) {
            if (duplicateIfBlocks.get(duplicateIfBlocks.size() - 1).getElseStatement() == null) {
                final Statement nextSibling= ASTNodes.getNextSibling(duplicateIfBlocks.get(duplicateIfBlocks.size() - 1));

                if (nextSibling instanceof IfStatement && ((IfStatement) nextSibling).getElseStatement() == null
                        && !ctx.getRefactorings().hasBeenRefactored(nextSibling)) {
                    final IfStatement nextIf= (IfStatement) nextSibling;

                    final List<Statement> lastIfStatements= ASTNodes.asList(
                            duplicateIfBlocks.get(duplicateIfBlocks.size() - 1).getThenStatement());
                    final List<Statement> nextIfStatements= ASTNodes.asList(nextIf.getThenStatement());
                    if (lastIfStatements != null && !lastIfStatements.isEmpty()
                            && ASTNodes.fallsThrough(lastIfStatements.get(lastIfStatements.size() - 1))
                            && ASTNodes.match(lastIfStatements, nextIfStatements)) {
                        duplicateIfBlocks.add(nextIf);
                        return true;
                    }
                }
            }

            return false;
        }

        private void mergeCode(final List<IfStatement> duplicateIfBlocks) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            final Refactorings r= ctx.getRefactorings();

            Iterator<IfStatement> iterator= duplicateIfBlocks.iterator();
            Expression newCondition= b.parenthesizeIfNeeded(b.move(iterator.next().getExpression()));

            while (iterator.hasNext()) {
                newCondition= b.infixExpression(newCondition, InfixExpression.Operator.CONDITIONAL_OR,
                        b.parenthesizeIfNeeded(b.move(iterator.next().getExpression())));
            }

            final IfStatement newIf= b.if0(newCondition, b.move(duplicateIfBlocks.get(0).getThenStatement()));

            iterator= duplicateIfBlocks.iterator();
            r.replace(iterator.next(), newIf);
            while (iterator.hasNext()) {
                r.remove(iterator.next());
            }
        }
    }
}
