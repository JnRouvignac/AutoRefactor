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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory.Copy;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
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
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        List<Statement> elseCode= ASTNodes.asList(node.getElseStatement());

        if (elseCode != null && elseCode.size() == 1) {
            IfStatement subNode= ASTNodes.as(elseCode.get(0), IfStatement.class);

            if (subNode != null && ASTNodes.getNbOperands(node.getExpression()) + ASTNodes.getNbOperands(subNode.getExpression()) < ASTNodes.EXCESSIVE_OPERAND_NUMBER) {
                return maybeMergeBlocks(node, subNode, subNode.getThenStatement(), subNode.getElseStatement(), true)
                        && maybeMergeBlocks(node, subNode, subNode.getElseStatement(), subNode.getThenStatement(), false);
            }
        }

        return true;
    }

    private boolean maybeMergeBlocks(final IfStatement node, final IfStatement subNode, final Statement doubleStatements,
            final Statement remainingStatements, final boolean isPositive) {
        if (doubleStatements != null && ASTNodes.match(node.getThenStatement(), doubleStatements)) {
            refactorBlocks(node.getExpression(), subNode, remainingStatements, isPositive);
            return false;
        }

        return true;
    }

    private void refactorBlocks(final Expression firstCondition, final IfStatement subNode,
            final Statement remainingStatements, final boolean isPositive) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        Expression additionalCondition;
        if (isPositive) {
            additionalCondition= rewrite.createMoveTarget(subNode.getExpression());
        } else {
            additionalCondition= ast.negate(subNode.getExpression(), Copy.COPY);
        }

        rewrite.replace(firstCondition, ast.infixExpression(ast.parenthesizeIfNeeded(rewrite.createMoveTarget(firstCondition)),
                InfixExpression.Operator.CONDITIONAL_OR, ast.parenthesizeIfNeeded(additionalCondition)));

        if (remainingStatements != null) {
            rewrite.replace(subNode, rewrite.createMoveTarget(remainingStatements));
        } else {
            rewrite.remove(subNode);
        }
    }
}
