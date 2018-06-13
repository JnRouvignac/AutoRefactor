/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.match;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTBuilder.Copy;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class MergeConditionalBlocksRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Merge conditional statements";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Merge adjacent if / else if / else statements with same code block.";
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
    public boolean visit(IfStatement node) {
        final List<Statement> elseCode = asList(node.getElseStatement());

        if (elseCode != null && elseCode.size() == 1 && elseCode.get(0) instanceof IfStatement) {
            final IfStatement subNode = (IfStatement) elseCode.get(0);
            final Expression secondCondition = subNode.getExpression();

            final Expression firstCondition = node.getExpression();
            final List<Statement> ifCode = asList(node.getThenStatement());

            return maybeMergeBlocks(firstCondition, ifCode, subNode, secondCondition, subNode.getThenStatement(),
                    subNode.getElseStatement(), true)
                    && maybeMergeBlocks(firstCondition, ifCode, subNode, secondCondition,
                            subNode.getElseStatement(), subNode.getThenStatement(), false);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeMergeBlocks(final Expression firstCondition, final List<Statement> ifCode,
            final IfStatement subNode, final Expression secondCondition, final Statement doubleStmts,
            final Statement remainingStmts, final boolean isPositive) {
        if (doubleStmts != null && isSameCode(ifCode, asList(doubleStmts))) {
            refactorBlocks(firstCondition, subNode, secondCondition, remainingStmts, isPositive);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private void refactorBlocks(final Expression firstCondition, final IfStatement subNode,
            final Expression secondCondition, final Statement remainingStmts, final boolean isPositive) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        final Expression additionalCondition;
        if (isPositive) {
            additionalCondition = b.copy(secondCondition);
        } else {
            additionalCondition = b.negate(secondCondition, Copy.COPY);
        }

        r.replace(firstCondition, b.infixExpr(b.parenthesizeIfNeeded(b.copy(firstCondition)),
                InfixExpression.Operator.CONDITIONAL_OR, b.parenthesizeIfNeeded(additionalCondition)));
        if (remainingStmts != null) {
            r.replace(subNode, b.copy(remainingStmts));
        } else {
            r.remove(subNode);
        }
    }

    private boolean isSameCode(final List<Statement> referenceStmts, final List<Statement> comparedStmts) {
        if (referenceStmts.size() != comparedStmts.size()) {
            return false;
        }

        final ASTSemanticMatcher matcher = new ASTSemanticMatcher();

        for (int codeLine = 0; codeLine < referenceStmts.size(); codeLine++) {
            if (!match(matcher, referenceStmts.get(codeLine),
                    comparedStmts.get(codeLine))) {
                return false;
            }
        }
        return true;
    }
}
