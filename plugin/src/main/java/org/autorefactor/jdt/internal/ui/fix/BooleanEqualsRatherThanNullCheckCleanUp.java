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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class BooleanEqualsRatherThanNullCheckCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.BooleanEqualsRatherThanNullCheckCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.BooleanEqualsRatherThanNullCheckCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.BooleanEqualsRatherThanNullCheckCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (ASTNodes.hasOperator(visited, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.AND, InfixExpression.Operator.OR)) {
			Expression leftOperand= visited.getLeftOperand();
			Expression rightOperand= visited.getRightOperand();

			InfixExpression condition= ASTNodes.as(leftOperand, InfixExpression.class);
			boolean isNullCheck= ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS);
			boolean isAndExpression= ASTNodes.hasOperator(visited, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND);

			if (!visited.hasExtendedOperands()
					&& isNullCheck ^ isAndExpression
					&& condition != null
					&& ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)) {
				Expression firstExpression= null;

				if (ASTNodes.is(condition.getLeftOperand(), NullLiteral.class)) {
					firstExpression= condition.getRightOperand();
				} else if (ASTNodes.is(condition.getRightOperand(), NullLiteral.class)) {
					firstExpression= condition.getLeftOperand();
				}

				Expression secondExpression= null;
				PrefixExpression negateSecondExpression= ASTNodes.as(rightOperand, PrefixExpression.class);

				boolean isPositiveExpression;
				if (negateSecondExpression != null && ASTNodes.hasOperator(negateSecondExpression, PrefixExpression.Operator.NOT)) {
					secondExpression= negateSecondExpression.getOperand();
					isPositiveExpression= false;
				} else {
					secondExpression= rightOperand;
					isPositiveExpression= true;
				}

				if (firstExpression != null
						&& ASTNodes.hasType(firstExpression, Boolean.class.getCanonicalName())
						&& ASTNodes.isPassive(firstExpression)
						&& ASTNodes.match(firstExpression, secondExpression)) {
					replaceNullCheck(visited, firstExpression, isNullCheck, isAndExpression, isPositiveExpression);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceNullCheck(final InfixExpression visited, final Expression firstExpression, final boolean isNullCheck,
			final boolean isAndExpression, final boolean isPositiveExpression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BooleanEqualsRatherThanNullCheckCleanUp_description);

		Name booleanConstant= ASTNodeFactory.newName(ast, Boolean.class.getSimpleName(), isAndExpression == isPositiveExpression ? "TRUE" : "FALSE"); //$NON-NLS-1$ //$NON-NLS-2$

		MethodInvocation equalsMethod= ast.newMethodInvocation();
		equalsMethod.setExpression(booleanConstant);
		equalsMethod.setName(ast.newSimpleName("equals")); //$NON-NLS-1$
		equalsMethod.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(firstExpression)));

		Expression newExpression;
		if (!isNullCheck || isAndExpression) {
			newExpression= equalsMethod;
		} else {
			newExpression= ast.not(equalsMethod);
		}

		rewrite.replace(visited, newExpression, group);
	}
}
