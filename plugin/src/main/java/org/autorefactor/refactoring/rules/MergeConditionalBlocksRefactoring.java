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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.ASTBuilder.Copy;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;

import static org.autorefactor.refactoring.ASTHelper.*;

/** See {@link #getDescription()} method. */
public class MergeConditionalBlocksRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Merge adjacent if / else if / else statements with same code block.";
    }

    @Override
    public String getName() {
        return "Merge conditional statements";
    }

    @Override
    public boolean visit(IfStatement node) {
        final Expression firstCondition = node.getExpression();
        final List<Statement> ifCode = asList(node.getThenStatement());
        final List<Statement> elseCode = asList(node.getElseStatement());

        if (elseCode != null && elseCode.size() == 1 && elseCode.get(0) instanceof IfStatement) {
            final IfStatement subNode = (IfStatement) elseCode.get(0);
            final Expression secondCondition = subNode.getExpression();

            return maybeMergeBlocks(firstCondition, ifCode, subNode, secondCondition, subNode.getThenStatement(),
                    subNode.getElseStatement(), true)
                    && maybeMergeBlocks(firstCondition, ifCode, subNode, secondCondition,
                            subNode.getElseStatement(), subNode.getThenStatement(), false);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeMergeBlocks(final Expression firstCondition, final List<Statement> ifCode,
            final IfStatement subNode, final Expression secondCondition, final Statement doubleStatements,
            final Statement remainingStatements, final boolean isPositive) {
        if (isSameCode(ifCode, asList(doubleStatements))) {
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
            if (remainingStatements != null) {
                r.replace(subNode, b.copy(remainingStatements));
            } else {
                r.remove(subNode);
            }
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean isSameCode(final List<Statement> referenceStatements, final List<Statement> comparedStatements) {
        if (referenceStatements.size() != comparedStatements.size()) {
            return false;
        }

        final ASTMatcher matcher = new ASTMatcher();

        for (int codeLine = 0; codeLine < referenceStatements.size(); codeLine++) {
            if (!ASTHelper.match(matcher, referenceStatements.get(codeLine),
                    comparedStatements.get(codeLine))) {
                return false;
            }
        }
        return true;
    }
}
