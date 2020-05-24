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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ForLoops;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ForLoopContent;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class NoLoopIterationRatherThanEmptyCheckCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_NoLoopIterationRatherThanEmptyCheckCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_NoLoopIterationRatherThanEmptyCheckCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_NoLoopIterationRatherThanEmptyCheckCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement node) {
		if (node.getElseStatement() == null) {
			List<Statement> statements= ASTNodes.asList(node.getThenStatement());

			if (statements != null
					&& statements.size() == 1) {
				Expression container= getContainer(statements);

				if (ASTNodes.isArray(container) && ASTNodes.isPassive(container)) {
					InfixExpression condition= ASTNodes.as(node.getExpression(), InfixExpression.class);

					if (isConditionValid(condition, container)) {
						ASTRewrite rewrite= cuRewrite.getASTRewrite();
						rewrite.replace(node, ASTNodes.createMoveTarget(rewrite, statements.get(0)), null);
						return false;
					}

					if (ASTNodes.hasOperator(condition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)) {
						List<Expression> operands= ASTNodes.allOperands(condition);
						Expression operand= ASTNodes.as(Utils.getLast(operands), InfixExpression.class);

						if (isConditionValid(operand, container)) {
							removeCondition(condition, operands);
							return false;
						}
					}
				}
			}
		}

		return true;
	}

	private boolean isConditionValid(final Expression expression, final Expression container) {
		InfixExpression condition= ASTNodes.as(expression, InfixExpression.class);
		return condition != null
				&& !condition.hasExtendedOperands() && ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS,
						InfixExpression.Operator.GREATER,
						InfixExpression.Operator.GREATER_EQUALS,
						InfixExpression.Operator.LESS,
						InfixExpression.Operator.LESS_EQUALS)
				&& (isConditionValid(condition, container, condition.getLeftOperand(), condition.getRightOperand(), true)
						|| isConditionValid(condition, container, condition.getRightOperand(), condition.getLeftOperand(), false));
	}

	private boolean isConditionValid(final InfixExpression condition, final Expression container, final Expression arrayOperand,
			final Expression literalOperand, final boolean isArrayOnLeft) {
		Expression array= getArray(container, arrayOperand);
		Long literal= ASTNodes.getIntegerLiteral(literalOperand);

		if (array != null
				&& literal != null) {
			long value= literal;

			if (ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS)) {
				return value == 0;
			}
			if (ASTNodes.hasOperator(condition, InfixExpression.Operator.GREATER)) {
				return isArrayOnLeft && value == 0;
			}
			if (ASTNodes.hasOperator(condition, InfixExpression.Operator.GREATER_EQUALS)) {
				return isArrayOnLeft && value == 1;
			}
			if (ASTNodes.hasOperator(condition, InfixExpression.Operator.LESS)) {
				return !isArrayOnLeft && value == 0;
			}
			if (ASTNodes.hasOperator(condition, InfixExpression.Operator.LESS_EQUALS)) {
				return !isArrayOnLeft && value == 1;
			}
		}

		return false;
	}

	private Expression getArray(final Expression container, final Expression operand) {
		FieldAccess fieldAccess= ASTNodes.as(operand, FieldAccess.class);
		QualifiedName name= ASTNodes.as(operand, QualifiedName.class);

		if (fieldAccess != null) {
			if (ASTNodes.isSameVariable(fieldAccess.getExpression(), container) && "length".equals(fieldAccess.getName().getIdentifier())) { //$NON-NLS-1$
				return fieldAccess.getExpression();
			}
		} else if (name != null && ASTNodes.isSameVariable(name.getQualifier(), container) && "length".equals(name.getName().getIdentifier())) { //$NON-NLS-1$
			return name.getQualifier();
		}

		return null;
	}

	private Expression getContainer(final List<Statement> statements) {
		ForStatement forStatement= ASTNodes.as(statements.get(0), ForStatement.class);
		EnhancedForStatement enhancedForStatement= ASTNodes.as(statements.get(0), EnhancedForStatement.class);

		if (forStatement != null) {
			ForLoopContent loopContent= ForLoops.iterateOverContainer(forStatement);

			if (loopContent != null) {
				return loopContent.getContainerVariable();
			}
		} else if (enhancedForStatement != null) {
			return enhancedForStatement.getExpression();
		}

		return null;
	}

	private void removeCondition(final InfixExpression condition, final List<Expression> operands) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (operands.size() == 2) {
			rewrite.replace(condition, ASTNodes.createMoveTarget(rewrite, operands.get(0)), null);
		} else {
			operands.remove(operands.size() - 1);
			InfixExpression newCondition= ast.infixExpression(condition.getOperator(), rewrite.createMoveTarget(operands));

			rewrite.replace(condition, newCondition, null);
		}
	}
}
