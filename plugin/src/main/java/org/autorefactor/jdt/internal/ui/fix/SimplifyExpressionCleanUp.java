/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class SimplifyExpressionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.SimplifyExpressionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.SimplifyExpressionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.SimplifyExpressionCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_OR)) {
			AtomicBoolean hasUselessOperand= new AtomicBoolean(false);
			List<Expression> remainingOperands= removeUselessOperands(node, hasUselessOperand, false, true);

			if (hasUselessOperand.get()) {
				replaceWithNewInfixExpression(node, remainingOperands);
				return false;
			}
		} else if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_AND)) {
			AtomicBoolean hasUselessOperand= new AtomicBoolean(false);
			List<Expression> remainingOperands= removeUselessOperands(node, hasUselessOperand, true, false);

			if (hasUselessOperand.get()) {
				replaceWithNewInfixExpression(node, remainingOperands);
				return false;
			}
			List<Expression> operands= ASTNodes.allOperands(node);

			if (operands.size() > 2) {
				for (int i= 0; i < operands.size() - 1; i++) {
					Expression nullCheckedExpression= ASTNodes.getNullCheckedExpression(operands.get(i));

					if (nullCheckedExpression != null && isNullCheckRedundant(operands.get(i + 1), nullCheckedExpression)) {
						operands.remove(i);

						ASTRewrite rewrite= cuRewrite.getASTRewrite();

						ASTNodeFactory ast= cuRewrite.getASTBuilder();
						TextEditGroup group= new TextEditGroup(MultiFixMessages.SimplifyExpressionCleanUp_description);

						ASTNodes.replaceButKeepComment(rewrite, node, ast.newInfixExpression(node.getOperator(), rewrite.createMoveTarget(operands)), group);
						return false;
					}
				}
			} else {
				Expression lhs= node.getLeftOperand();
				Expression rhs= node.getRightOperand();
				Expression nullCheckedExpressionLHS= ASTNodes.getNullCheckedExpression(lhs);

				if (nullCheckedExpressionLHS != null && isNullCheckRedundant(rhs, nullCheckedExpressionLHS)) {
					replaceBy(node, rhs);
					return false;
				}
			}
		} else if (ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.XOR) && !node.hasExtendedOperands()
				&& !maybeReduceBooleanExpression(node)) {
			return false;
		}

		return true;
	}

	private List<Expression> removeUselessOperands(final InfixExpression node, final AtomicBoolean hasUselessOperand, final Boolean neutralElement, final Boolean shortCircuitValue) {
		List<Expression> allOperands= ASTNodes.allOperands(node);

		for (ListIterator<Expression> it= allOperands.listIterator(); it.hasNext();) {
			Expression operand= it.next();
			Object value= operand.resolveConstantExpressionValue();

			if (shortCircuitValue.equals(value)) {
				while (it.hasNext()) {
					hasUselessOperand.set(true);
					it.next();
					it.remove();
				}
				break;
			}

			if (neutralElement.equals(value)) {
				hasUselessOperand.set(true);
				it.remove();
			}
		}

		return allOperands;
	}

	private boolean maybeReduceBooleanExpression(final InfixExpression node) {
		Expression leftExpression= node.getLeftOperand();
		Expression rightExpression= node.getRightOperand();

		Boolean leftBoolean= ASTNodes.getBooleanLiteral(leftExpression);

		if (leftBoolean != null) {
			return replace(node, leftBoolean, rightExpression);
		}

		Boolean rightBoolean= ASTNodes.getBooleanLiteral(rightExpression);

		if (rightBoolean != null) {
			return replace(node, rightBoolean, leftExpression);
		}

		Expression leftOppositeExpression= null;
		PrefixExpression leftPrefix= ASTNodes.as(leftExpression, PrefixExpression.class);
		if (leftPrefix != null && ASTNodes.hasOperator(leftPrefix, PrefixExpression.Operator.NOT)) {
			leftOppositeExpression= leftPrefix.getOperand();
		}

		Expression rightOppositeExpression= null;
		PrefixExpression rightPrefix= ASTNodes.as(rightExpression, PrefixExpression.class);
		if (rightPrefix != null && ASTNodes.hasOperator(rightPrefix, PrefixExpression.Operator.NOT)) {
			rightOppositeExpression= rightPrefix.getOperand();
		}

		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.SimplifyExpressionCleanUp_description);

		if (leftOppositeExpression != null) {
			if (rightOppositeExpression != null) {
				ASTNodes.replaceButKeepComment(rewrite, node,
						ast.newInfixExpression(ASTNodes.createMoveTarget(rewrite, leftOppositeExpression), getAppropriateOperator(node), ASTNodes.createMoveTarget(rewrite, rightOppositeExpression)), group);
			} else {
				InfixExpression.Operator reverseOp= getReverseOperator(node);
				ASTNodes.replaceButKeepComment(rewrite, node, ast.newInfixExpression(ASTNodes.createMoveTarget(rewrite, leftOppositeExpression), reverseOp, ASTNodes.createMoveTarget(rewrite, rightExpression)), group);
			}

			return false;
		}

		if (rightOppositeExpression != null) {
			InfixExpression.Operator reverseOp= getReverseOperator(node);
			ASTNodes.replaceButKeepComment(rewrite, node, ast.newInfixExpression(ASTNodes.createMoveTarget(rewrite, leftExpression), reverseOp, ASTNodes.createMoveTarget(rewrite, rightOppositeExpression)), group);
			return false;
		}

		return true;
	}

	private InfixExpression.Operator getAppropriateOperator(final InfixExpression node) {
		if (ASTNodes.hasOperator(node, InfixExpression.Operator.NOT_EQUALS)) {
			return InfixExpression.Operator.XOR;
		}

		return node.getOperator();
	}

	private InfixExpression.Operator getReverseOperator(final InfixExpression node) {
		if (ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS)) {
			return InfixExpression.Operator.XOR;
		}

		return InfixExpression.Operator.EQUALS;
	}

	private boolean replace(final InfixExpression node, final boolean isTrue, final Expression exprToCopy) {
		ASTNodes.checkNoExtendedOperands(node);

		if (!ASTNodes.isPrimitive(node.getLeftOperand(), boolean.class.getSimpleName()) && !ASTNodes.isPrimitive(node.getRightOperand(), boolean.class.getSimpleName())) {
			return true;
		}

		// Either:
		// - Two boolean primitives: no possible NPE
		// - One boolean primitive and one Boolean object, this code already run
		// the risk of an NPE, so we can replace the infix expression without
		// fearing we would introduce a previously non existing NPE.
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.SimplifyExpressionCleanUp_description);

		Expression operand;
		if (isTrue == ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS)) {
			operand= ASTNodes.createMoveTarget(rewrite, exprToCopy);
		} else {
			operand= ast.negate(exprToCopy, true);
		}

		ASTNodes.replaceButKeepComment(rewrite, node, operand, group);
		return false;
	}

	private void replaceWithNewInfixExpression(final InfixExpression node, final List<Expression> remainingOperands) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.SimplifyExpressionCleanUp_description);

		if (remainingOperands.size() == 1) {
			ASTNodes.replaceButKeepComment(rewrite, node, ASTNodes.createMoveTarget(rewrite, remainingOperands.get(0)), group);
		} else {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			ASTNodes.replaceButKeepComment(rewrite, node, ast.newInfixExpression(node.getOperator(), rewrite.createMoveTarget(remainingOperands)), group);
		}
	}

	private void replaceBy(final ASTNode node, final Expression expression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.SimplifyExpressionCleanUp_description);
		ASTNodes.replaceButKeepComment(rewrite, node, ASTNodes.createMoveTarget(rewrite, expression), group);
	}

	/**
	 * The previous null check is redundant if:
	 * <ul>
	 * <li>the null checked expression is reused in an instanceof expression</li>
	 * <li>the null checked expression is reused in an expression checking for
	 * object equality against an expression that resolves to a non null
	 * constant</li>
	 * </ul>
	 */
	private boolean isNullCheckRedundant(final Expression e, final Expression nullCheckedExpression) {
		if (nullCheckedExpression != null) {
			if (e instanceof InstanceofExpression) {
				Expression expression= ((InstanceofExpression) e).getLeftOperand();
				return expression.subtreeMatch(ASTSemanticMatcher.INSTANCE, nullCheckedExpression);
			}

			if (e instanceof MethodInvocation) {
				MethodInvocation expression= (MethodInvocation) e;

				if (expression.getExpression() != null && expression.getExpression().resolveConstantExpressionValue() != null
						&& expression.arguments().size() == 1
						&& ((Expression) expression.arguments().get(0)).subtreeMatch(ASTSemanticMatcher.INSTANCE, nullCheckedExpression)) {
					// Did we invoke java.lang.Object.equals() or
					// java.lang.String.equalsIgnoreCase()?
					return ASTNodes.usesGivenSignature(expression, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(expression, String.class.getCanonicalName(), "equalsIgnoreCase", String.class.getCanonicalName()); //$NON-NLS-1$
				}
			}
		}

		return false;
	}
}
