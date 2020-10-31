/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2020 Jean-Noël Rouvignac - initial API and implementation
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
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class EqualsNullableCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.EqualsNullableCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.EqualsNullableCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.EqualsNullableCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (ASTNodes.hasOperator(visited, InfixExpression.Operator.CONDITIONAL_AND)) {
			List<Expression> operands= ASTNodes.allOperands(visited);

			if (operands.size() > 2) {
				for (int i= 0; i < operands.size() - 1; i++) {
					Expression nullCheckedExpression= ASTNodes.getNullCheckedExpression(operands.get(i));

					if (nullCheckedExpression != null && isNullCheckRedundant(operands.get(i + 1), nullCheckedExpression)) {
						operands.remove(i);

						ASTRewrite rewrite= cuRewrite.getASTRewrite();
						ASTNodeFactory ast= cuRewrite.getASTBuilder();
						TextEditGroup group= new TextEditGroup(MultiFixMessages.EqualsNullableCleanUp_description);

						InfixExpression newInfixExpression= ast.newInfixExpression(visited.getOperator(), rewrite.createMoveTarget(operands));
						ASTNodes.replaceButKeepComment(rewrite, visited, newInfixExpression, group);
						return false;
					}
				}
			} else {
				Expression leftOperand= visited.getLeftOperand();
				Expression rightOperand= visited.getRightOperand();
				Expression nullCheckedExpressionLHS= ASTNodes.getNullCheckedExpression(leftOperand);

				if (nullCheckedExpressionLHS != null && isNullCheckRedundant(rightOperand, nullCheckedExpressionLHS)) {
					replaceBy(visited, rightOperand);
					return false;
				}
			}
		}

		return true;
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
	private boolean isNullCheckRedundant(final Expression expression, final Expression nullCheckedExpression) {
		if (nullCheckedExpression != null) {
			if (expression instanceof InstanceofExpression) {
				Expression leftOperand= ((InstanceofExpression) expression).getLeftOperand();
				return leftOperand.subtreeMatch(ASTSemanticMatcher.INSTANCE, nullCheckedExpression);
			}

			if (expression instanceof MethodInvocation) {
				MethodInvocation methodInvocation= (MethodInvocation) expression;

				if (methodInvocation.getExpression() != null && methodInvocation.getExpression().resolveConstantExpressionValue() != null
						&& methodInvocation.arguments().size() == 1
						&& ((Expression) methodInvocation.arguments().get(0)).subtreeMatch(ASTSemanticMatcher.INSTANCE, nullCheckedExpression)) {
					// Did we invoke java.lang.Object.equals() or
					// java.lang.String.equalsIgnoreCase()?
					return ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, String.class.getCanonicalName(), "equalsIgnoreCase", String.class.getCanonicalName()); //$NON-NLS-1$
				}
			}
		}

		return false;
	}

	private void replaceBy(final ASTNode visited, final Expression expression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.EqualsNullableCleanUp_description);
		ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, expression), group);
	}
}
