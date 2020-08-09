/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;

/** See {@link #getDescription()} method. */
public class AssignRatherThanTernaryFilterThenAssignAnywayCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_AssignRatherThanTernaryFilterThenAssignAnywayCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_AssignRatherThanTernaryFilterThenAssignAnywayCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_AssignRatherThanTernaryFilterThenAssignAnywayCleanUp_reason;
	}

	@Override
	public boolean visit(final ConditionalExpression node) {
		InfixExpression condition= ASTNodes.as(node.getExpression(), InfixExpression.class);

		if (condition != null
				&& ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)
				&& !condition.hasExtendedOperands()) {
			Expression hardCodedExpression;
			Expression valuedExpression;

			if (ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS)) {
				hardCodedExpression= node.getThenExpression();
				valuedExpression= node.getElseExpression();
			} else {
				hardCodedExpression= node.getElseExpression();
				valuedExpression= node.getThenExpression();
			}

			if (ASTNodes.isHardCoded(hardCodedExpression) && ASTNodes.isPassive(valuedExpression)) {
				return maybeReplaceWithValue(node, hardCodedExpression, valuedExpression, condition.getRightOperand(), condition.getLeftOperand())
						&& maybeReplaceWithValue(node, hardCodedExpression, valuedExpression, condition.getLeftOperand(), condition.getRightOperand());
			}
		}

		return true;
	}

	private boolean maybeReplaceWithValue(final ConditionalExpression node, final Expression hardCodedExpression,
			final Expression valuedExpression, final Expression hardCodedOperand, final Expression valuedOperand) {
		if (ASTNodes.match(hardCodedOperand, hardCodedExpression)
				&& ASTNodes.match(valuedOperand, valuedExpression)) {
			replaceWithValue(node, valuedExpression);
			return false;
		}

		return true;
	}

	private void replaceWithValue(final ConditionalExpression node, final Expression valuedExpression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		rewrite.replace(node, ASTRewrite.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, valuedExpression)), null);
	}
}
