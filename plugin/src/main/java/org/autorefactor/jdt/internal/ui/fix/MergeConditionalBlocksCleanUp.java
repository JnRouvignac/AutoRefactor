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
package org.autorefactor.jdt.internal.ui.fix;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.asList;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.match;

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.ASTBuilder.Copy;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class MergeConditionalBlocksCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_reason;
    }

    @Override
    public boolean visit(IfStatement node) {
        final List<Statement> elseCode= asList(node.getElseStatement());

        if (elseCode != null && elseCode.size() == 1 && elseCode.get(0) instanceof IfStatement) {
            final IfStatement subNode= (IfStatement) elseCode.get(0);

            return maybeMergeBlocks(node, subNode, subNode.getThenStatement(), subNode.getElseStatement(), true)
                    && maybeMergeBlocks(node, subNode, subNode.getElseStatement(), subNode.getThenStatement(), false);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeMergeBlocks(final IfStatement node, final IfStatement subNode, final Statement doubleStmts,
            final Statement remainingStmts, final boolean isPositive) {
        if (doubleStmts != null && match(new ASTSemanticMatcher(), node.getThenStatement(), doubleStmts)) {
            refactorBlocks(node.getExpression(), subNode, remainingStmts, isPositive);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private void refactorBlocks(final Expression firstCondition, final IfStatement subNode,
            final Statement remainingStmts, final boolean isPositive) {
        final ASTBuilder b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();

        final Expression additionalCondition;
        if (isPositive) {
            additionalCondition= b.copy(subNode.getExpression());
        } else {
            additionalCondition= b.negate(subNode.getExpression(), Copy.COPY);
        }

        r.replace(firstCondition, b.infixExpr(b.parenthesizeIfNeeded(b.copy(firstCondition)),
                InfixExpression.Operator.CONDITIONAL_OR, b.parenthesizeIfNeeded(additionalCondition)));
        if (remainingStmts != null) {
            r.replace(subNode, b.copy(remainingStmts));
        } else {
            r.remove(subNode);
        }
    }
}
