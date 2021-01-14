/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.math.BigDecimal;
import java.math.BigInteger;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ComparisonRatherThanEqualsCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ComparisonRatherThanEqualsCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ComparisonRatherThanEqualsCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ComparisonRatherThanEqualsCleanUp_reason;
	}

	@Override
	public boolean visit(final PrefixExpression visited) {
		MethodInvocation methodInvocation= ASTNodes.as(visited.getOperand(), MethodInvocation.class);

		if (methodInvocation != null && ASTNodes.hasOperator(visited, PrefixExpression.Operator.NOT)) {
			return maybeReplaceEquals(false, visited, methodInvocation);
		}

		return true;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() == null) {
			return true;
		}

		if (!(visited.getParent() instanceof PrefixExpression)
				|| !ASTNodes.hasOperator((PrefixExpression) visited.getParent(), PrefixExpression.Operator.NOT)) {
			return maybeReplaceEquals(true, visited, visited);
		}

		return true;
	}

	private boolean maybeReplaceEquals(final boolean isPositive, final Expression visited, final MethodInvocation methodInvocation) {
		if (ASTNodes.usesGivenSignature(methodInvocation, BigDecimal.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, BigInteger.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
			Expression arg0= (Expression) methodInvocation.arguments().get(0);

			if (ASTNodes.hasType(arg0, BigDecimal.class.getCanonicalName(), BigInteger.class.getCanonicalName())) {
				replaceEquals(isPositive, visited, methodInvocation);
				return false;
			}
		}

		return true;
	}

	private void replaceEquals(final boolean isPositive, final Expression visited,
			final MethodInvocation methodInvocation) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ComparisonRatherThanEqualsCleanUp_description);

		if (isInStringAppend(methodInvocation.getParent())) {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			ASTNodes.replaceButKeepComment(rewrite, visited, ast.newParenthesizedExpression(getCompareToNode(isPositive, methodInvocation)), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, visited, getCompareToNode(isPositive, methodInvocation), group);
		}
	}

	private boolean isInStringAppend(final ASTNode visited) {
		if (visited instanceof InfixExpression) {
			InfixExpression expression= (InfixExpression) visited;

			if (ASTNodes.hasOperator(expression, InfixExpression.Operator.PLUS)
					|| ASTNodes.hasType(expression.getLeftOperand(), String.class.getCanonicalName())
					|| ASTNodes.hasType(expression.getRightOperand(), String.class.getCanonicalName())) {
				return true;
			}
		}

		return false;
	}

	private InfixExpression getCompareToNode(final boolean isPositive, final MethodInvocation visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		MethodInvocation methodInvocation= ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, visited.getExpression()), "compareTo", ASTNodes.createMoveTarget(rewrite, (Expression) visited.arguments().get(0))); //$NON-NLS-1$

		InfixExpression newInfixExpression= ast.newInfixExpression();
		newInfixExpression.setLeftOperand(methodInvocation);
		newInfixExpression.setOperator(isPositive ? InfixExpression.Operator.EQUALS : InfixExpression.Operator.NOT_EQUALS);
		newInfixExpression.setRightOperand(ast.newNumberLiteral(0));
		return newInfixExpression;
	}
}
