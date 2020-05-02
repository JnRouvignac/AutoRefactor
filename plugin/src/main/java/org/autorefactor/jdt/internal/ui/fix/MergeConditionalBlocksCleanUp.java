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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class MergeConditionalBlocksCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_MergeConditionalBlocksCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement node) {
		if (node.getElseStatement() != null) {
			List<IfStatement> duplicateIfBlocks= new ArrayList<>(4);
			List<Boolean> isThenStatement= new ArrayList<>(4);
			AtomicInteger operandCount= new AtomicInteger(ASTNodes.getNbOperands(node.getExpression()));
			duplicateIfBlocks.add(node);
			isThenStatement.add(Boolean.TRUE);

			while (addOneMoreIf(duplicateIfBlocks, isThenStatement, operandCount)) {
				// OK continue
			}

			if (duplicateIfBlocks.size() > 1) {
				mergeCode(duplicateIfBlocks, isThenStatement);
				return false;
			}
		}

		return true;
	}

	private boolean addOneMoreIf(final List<IfStatement> duplicateIfBlocks, final List<Boolean> isThenStatement, final AtomicInteger operandCount) {
		IfStatement lastBlock= Utils.getLast(duplicateIfBlocks);
		Statement previousStatement= Utils.getLast(isThenStatement) ? lastBlock.getThenStatement() : lastBlock.getElseStatement();
		Statement nextStatement= Utils.getLast(isThenStatement) ? lastBlock.getElseStatement() : lastBlock.getThenStatement();

		if (nextStatement != null) {
			IfStatement nextElse= ASTNodes.as(nextStatement, IfStatement.class);

			if (nextElse != null
					&& !cuRewrite.getASTRewrite().hasBeenRefactored(nextElse)
					&& operandCount.get() + ASTNodes.getNbOperands(nextElse.getExpression()) < ASTNodes.EXCESSIVE_OPERAND_NUMBER) {
				if (ASTNodes.match(previousStatement, nextElse.getThenStatement())) {
					operandCount.addAndGet(ASTNodes.getNbOperands(nextElse.getExpression()));
					duplicateIfBlocks.add(nextElse);
					isThenStatement.add(Boolean.TRUE);
					return true;
				}

				if (nextElse.getElseStatement() != null
						&& ASTNodes.match(previousStatement, nextElse.getElseStatement())) {
					operandCount.addAndGet(ASTNodes.getNbOperands(nextElse.getExpression()));
					duplicateIfBlocks.add(nextElse);
					isThenStatement.add(Boolean.FALSE);
					return true;
				}
			}
		}

		return false;
	}

	private void mergeCode(final List<IfStatement> duplicateIfBlocks, final List<Boolean> isThenStatement) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		List<Expression> newConditions= new ArrayList<>(duplicateIfBlocks.size());

		for (int i= 0; i < duplicateIfBlocks.size(); i++) {
			if (isThenStatement.get(i)) {
				newConditions.add(ast.parenthesizeIfNeeded(ASTNodes.createMoveTarget(rewrite, duplicateIfBlocks.get(i).getExpression())));
			} else {
				newConditions.add(ast.parenthesizeIfNeeded(ast.negate(duplicateIfBlocks.get(i).getExpression())));
			}
		}

		IfStatement lastBlock= Utils.getLast(duplicateIfBlocks);
		Statement remainingStatement= Utils.getLast(isThenStatement) ? lastBlock.getElseStatement() : lastBlock.getThenStatement();
		InfixExpression newCondition= ast.infixExpression(InfixExpression.Operator.CONDITIONAL_OR, newConditions);

		rewrite.replace(duplicateIfBlocks.get(0).getExpression(), newCondition, null);

		if (remainingStatement != null) {
			rewrite.replace(duplicateIfBlocks.get(0).getElseStatement(), ASTNodes.createMoveTarget(rewrite, remainingStatement), null);
		} else if (duplicateIfBlocks.get(0).getElseStatement() != null) {
			rewrite.remove(duplicateIfBlocks.get(0).getElseStatement(), null);
		}
	}
}
