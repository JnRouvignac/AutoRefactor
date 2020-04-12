/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;

/** See {@link #getDescription()} method. */
public class ORConditionRatherThanRedundantClausesCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_ORConditionRatherThanRedundantClausesCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR)) {
			List<Expression> operands= ASTNodes.allOperands(node);

			for (int i= 1; i < operands.size(); i++) {
				Expression leftOperand= operands.get(i - 1);
				Expression rightOperand= operands.get(i);

				if (!maybeRefactorCondition(leftOperand, rightOperand) || !maybeRefactorCondition(rightOperand, leftOperand)) {
					return false;
				}
			}
		}

		return true;
	}

	private boolean maybeRefactorCondition(final Expression operandWithRedundance, final Expression redundantOperand) {
		InfixExpression complexCondition= ASTNodes.as(operandWithRedundance, InfixExpression.class);

		if (ASTNodes.isPrimitive(redundantOperand)
				&& ASTNodes.isPassive(redundantOperand)
				&& complexCondition != null
				&& ASTNodes.hasOperator(complexCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)) {
			List<Expression> operands= ASTNodes.allOperands(complexCondition);

			for (int i= 0; i < operands.size(); i++) {
				List<Expression> previousOperands= operands.subList(0, i);
				Expression duplicateOperand= operands.get(i);
				List<Expression> nextOperands= operands.subList(i + 1, operands.size());

				if (ASTNodes.isPrimitive(duplicateOperand)
						&& ASTNodes.isPassive(duplicateOperand)
						&& isPrimitiveAndPassive(nextOperands)
						&& ASTSemanticMatcher.INSTANCE.matchOpposite(duplicateOperand, redundantOperand)) {
					replaceDuplicateExpression(previousOperands, nextOperands, operandWithRedundance, complexCondition.getOperator());
					return false;
				}
			}
		}

		return true;
	}

	private boolean isPrimitiveAndPassive(final List<Expression> operands) {
		for (Expression operand : operands) {
			if (!ASTNodes.isPrimitive(operand) || !ASTNodes.isPassive(operand)) {
				return false;
			}
		}

		return true;
	}

	private void replaceDuplicateExpression(final List<Expression> previousOperands, final List<Expression> nextOperands, final Expression operandWithRedundance, final InfixExpression.Operator operator) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		List<Expression> copyOfOperands= rewrite.createMoveTarget(previousOperands);
		copyOfOperands.addAll(rewrite.createMoveTarget(nextOperands));

		if (copyOfOperands.size() == 1) {
			rewrite.replace(operandWithRedundance, copyOfOperands.get(0), null);
		} else {
			rewrite.replace(operandWithRedundance, ast.infixExpression(operator, copyOfOperands), null);
		}
	}
}
