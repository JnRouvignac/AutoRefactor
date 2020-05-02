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
import java.util.concurrent.atomic.AtomicInteger;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;

/** See {@link #getDescription()} method. */
public class OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_OneIfRatherThanDuplicateBlocksThatFallThroughCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		SuccessiveIfVisitor successiveIfVisitor= new SuccessiveIfVisitor();
		successiveIfVisitor.visitNode(node);
		return successiveIfVisitor.result;
	}

	private final class SuccessiveIfVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final IfStatement node) {
			if (result
					&& ASTNodes.fallsThrough(node.getThenStatement())) {
				List<IfStatement> duplicateIfBlocks= new ArrayList<>(4);
				AtomicInteger operandCount= new AtomicInteger(ASTNodes.getNbOperands(node.getExpression()));
				duplicateIfBlocks.add(node);

				while (addOneMoreIf(duplicateIfBlocks, operandCount)) {
					// OK continue
				}

				if (duplicateIfBlocks.size() > 1) {
					mergeCode(duplicateIfBlocks);
					this.result= false;
					return false;
				}
			}

			return true;
		}

		private boolean addOneMoreIf(final List<IfStatement> duplicateIfBlocks, final AtomicInteger operandCount) {
			IfStatement lastBlock= Utils.getLast(duplicateIfBlocks);

			if (lastBlock.getElseStatement() == null) {
				IfStatement nextSibling= ASTNodes.as(ASTNodes.getNextSibling(lastBlock), IfStatement.class);

				if (nextSibling != null && nextSibling.getElseStatement() == null
						&& operandCount.get() + ASTNodes.getNbOperands(nextSibling.getExpression()) < ASTNodes.EXCESSIVE_OPERAND_NUMBER
						&& !cuRewrite.getASTRewrite().hasBeenRefactored(nextSibling)
						&& ASTNodes.match(lastBlock.getThenStatement(), nextSibling.getThenStatement())) {
					operandCount.addAndGet(ASTNodes.getNbOperands(nextSibling.getExpression()));
					duplicateIfBlocks.add(nextSibling);
					return true;
				}
			}

			return false;
		}

		private void mergeCode(final List<IfStatement> duplicateIfBlocks) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			List<Expression> newConditions= new ArrayList<>(duplicateIfBlocks.size());

			for (IfStatement ifStatement : duplicateIfBlocks) {
				newConditions.add(ast.parenthesizeIfNeeded(ASTNodes.createMoveTarget(rewrite, ifStatement.getExpression())));
			}

			InfixExpression newCondition= ast.infixExpression(InfixExpression.Operator.CONDITIONAL_OR, newConditions);
			Iterator<IfStatement> iterator= duplicateIfBlocks.iterator();
			IfStatement oldIf= iterator.next();
			rewrite.replace(oldIf.getExpression(), newCondition, null);

			while (iterator.hasNext()) {
				rewrite.remove(iterator.next(), null);
			}
		}
	}
}
