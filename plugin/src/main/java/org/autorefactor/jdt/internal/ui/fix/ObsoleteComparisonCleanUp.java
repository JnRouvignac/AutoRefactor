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

import java.util.Arrays;
import java.util.Comparator;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.OrderedInfixExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteComparisonCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteComparisonCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteComparisonCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteComparisonCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		OrderedInfixExpression<MethodInvocation, Expression> orderedCondition= ASTNodes.orderedInfix(visited, MethodInvocation.class, Expression.class);

		if (orderedCondition != null
				&& Arrays.asList(InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS).contains(orderedCondition.getOperator())) {
			MethodInvocation comparisonMI= orderedCondition.getFirstOperand();
			Long literalValue= ASTNodes.getIntegerLiteral(orderedCondition.getSecondOperand());

			if (literalValue != null
					&& literalValue.compareTo(0L) != 0
					&& comparisonMI.getExpression() != null
					&& !ASTNodes.is(comparisonMI.getExpression(), ThisExpression.class)
					&& (ASTNodes.usesGivenSignature(comparisonMI, Comparable.class.getCanonicalName(), "compareTo", Object.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(comparisonMI, Comparator.class.getCanonicalName(), "compare", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
					|| getJavaMinorVersion() >= 2
					&& ASTNodes.usesGivenSignature(comparisonMI, String.class.getCanonicalName(), "compareToIgnoreCase", String.class.getCanonicalName()))) { //$NON-NLS-1$
				if (literalValue.compareTo(0L) < 0) {
					if (InfixExpression.Operator.EQUALS.equals(orderedCondition.getOperator())) {
						refactorComparingToZero(visited, comparisonMI, InfixExpression.Operator.LESS);
					} else {
						refactorComparingToZero(visited, comparisonMI, InfixExpression.Operator.GREATER_EQUALS);
					}
				} else if (InfixExpression.Operator.EQUALS.equals(orderedCondition.getOperator())) {
					refactorComparingToZero(visited, comparisonMI, InfixExpression.Operator.GREATER);
				} else {
					refactorComparingToZero(visited, comparisonMI, InfixExpression.Operator.LESS_EQUALS);
				}

				return false;
			}
		}

		return true;
	}

	private void refactorComparingToZero(final InfixExpression visited, final MethodInvocation comparisonMethod,
			final InfixExpression.Operator operator) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteComparisonCleanUp_description);

		InfixExpression newInfixExpression= ast.newInfixExpression();
		newInfixExpression.setLeftOperand(ASTNodes.createMoveTarget(rewrite, comparisonMethod));
		newInfixExpression.setOperator(operator);
		newInfixExpression.setRightOperand(ast.newNumberLiteral("0")); //$NON-NLS-1$

		ASTNodes.replaceButKeepComment(rewrite, visited, newInfixExpression, group);
	}
}
