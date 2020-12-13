/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - Initial implementation
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
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class OperandFactorizationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.OperandFactorizationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.OperandFactorizationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.OperandFactorizationCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (!visited.hasExtendedOperands()
				&& ASTNodes.hasOperator(visited, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR)) {
			InfixExpression firstCondition= ASTNodes.as(visited.getLeftOperand(), InfixExpression.class);
			InfixExpression secondCondition= ASTNodes.as(visited.getRightOperand(), InfixExpression.class);

			if (firstCondition != null
					&& secondCondition != null
					&& !firstCondition.hasExtendedOperands()
					&& !secondCondition.hasExtendedOperands()
					&& ASTNodes.hasOperator(firstCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)
					&& ASTNodes.hasOperator(secondCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)
					&& ASTNodes.isPrimitive(firstCondition.getLeftOperand())
					&& ASTNodes.isPrimitive(firstCondition.getRightOperand())
					&& ASTNodes.isPrimitive(secondCondition.getLeftOperand())
					&& ASTNodes.isPrimitive(secondCondition.getRightOperand())
					&& ASTNodes.isPassiveWithoutFallingThrough(firstCondition.getLeftOperand())
					&& ASTNodes.isPassiveWithoutFallingThrough(firstCondition.getRightOperand())
					&& ASTNodes.isPassiveWithoutFallingThrough(secondCondition.getLeftOperand())
					&& ASTNodes.isPassiveWithoutFallingThrough(secondCondition.getRightOperand())) {
				return maybeReplaceDuplicateExpression(visited, firstCondition, firstCondition.getLeftOperand(),
						secondCondition.getLeftOperand(), firstCondition.getRightOperand(), secondCondition.getRightOperand())
						&& maybeReplaceDuplicateExpression(visited, firstCondition, firstCondition.getLeftOperand(),
								secondCondition.getRightOperand(), firstCondition.getRightOperand(), secondCondition.getLeftOperand())
						&& maybeReplaceDuplicateExpression(visited, firstCondition, firstCondition.getRightOperand(),
								secondCondition.getLeftOperand(), firstCondition.getLeftOperand(), secondCondition.getRightOperand())
						&& maybeReplaceDuplicateExpression(visited, firstCondition, firstCondition.getRightOperand(),
								secondCondition.getRightOperand(), firstCondition.getLeftOperand(), secondCondition.getLeftOperand());
			}
		}

		return true;
	}

	private boolean maybeReplaceDuplicateExpression(final InfixExpression visited, final InfixExpression firstCondition,
			final Expression firstExpression, final Expression firstOppositeExpression, final Expression secondExpression, final Expression secondOppositeExpression) {
		if (ASTNodes.match(firstExpression, firstOppositeExpression)
				&& !ASTNodes.match(secondExpression, secondOppositeExpression)
				&& !ASTSemanticMatcher.INSTANCE.matchNegative(secondExpression, secondOppositeExpression)) {
			replaceDuplicateExpression(visited, firstCondition, firstExpression, secondExpression, secondOppositeExpression);
			return false;
		}

		return true;
	}

	private void replaceDuplicateExpression(final InfixExpression visited, final InfixExpression firstCondition,
			final Expression firstExpression, final Expression secondExpression, final Expression secondOppositeExpression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.OperandFactorizationCleanUp_description);

		InfixExpression newInnerInfixExpression= ast.newInfixExpression();
		newInnerInfixExpression.setOperator(visited.getOperator());
		newInnerInfixExpression.setLeftOperand(ASTNodes.createMoveTarget(rewrite, secondExpression));
		newInnerInfixExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, secondOppositeExpression));

		InfixExpression newMainInfixExpression= ast.newInfixExpression();
		newMainInfixExpression.setOperator(firstCondition.getOperator());
		newMainInfixExpression.setLeftOperand(ASTNodes.createMoveTarget(rewrite, firstExpression));
		newMainInfixExpression.setRightOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, newInnerInfixExpression));

		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodeFactory.parenthesizeIfNeeded(ast, newMainInfixExpression), group);
	}
}
