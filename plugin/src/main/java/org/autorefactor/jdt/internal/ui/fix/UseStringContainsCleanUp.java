/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2019 Fabrice Tiercelin - Correctly flag the visited nodes and do not reverse the condition
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
import org.autorefactor.jdt.internal.corext.dom.OrderedInfixExpression;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class UseStringContainsCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		OrderedInfixExpression<MethodInvocation, Expression> orderedExpression= ASTNodes.orderedInfix(node, MethodInvocation.class, Expression.class);

		if (orderedExpression != null) {
			MethodInvocation indexOf= orderedExpression.getFirstOperand();
			Long value= ASTNodes.getIntegerLiteral(orderedExpression.getSecondOperand());

			if (value != null
					&& (ASTNodes.usesGivenSignature(indexOf, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(indexOf, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName()))) { //$NON-NLS-1$

				if (is(orderedExpression.getOperator(), InfixExpression.Operator.GREATER_EQUALS, value, 0)
						|| is(orderedExpression.getOperator(), InfixExpression.Operator.NOT_EQUALS, value, -1)) {
					replaceWithStringContains(node, indexOf, true);
					return false;
				}

				if (is(orderedExpression.getOperator(), InfixExpression.Operator.LESS, value, 0)
						|| is(orderedExpression.getOperator(), InfixExpression.Operator.EQUALS, value, -1)) {
					replaceWithStringContains(node, indexOf, false);
					return false;
				}
			}
		}

		return true;
	}

	private boolean is(final InfixExpression.Operator actualOperator, final InfixExpression.Operator expectedOperator, final Long actualNumber, final int expectedNumber) {
		return Utils.equalNotNull(expectedOperator, actualOperator)
				&& (long) actualNumber == expectedNumber;
	}

	private void replaceWithStringContains(final InfixExpression node, final MethodInvocation method, final boolean isPositive) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		rewrite.set(method, MethodInvocation.NAME_PROPERTY, ast.simpleName("contains"), null); //$NON-NLS-1$

		if (isPositive) {
			rewrite.replace(node, ASTNodes.createMoveTarget(rewrite, method), null);
		} else {
			rewrite.replace(node, ast.not(ASTNodes.createMoveTarget(rewrite, method)), null);
		}
	}
}
