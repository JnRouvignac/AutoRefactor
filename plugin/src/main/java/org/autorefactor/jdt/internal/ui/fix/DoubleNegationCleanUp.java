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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class DoubleNegationCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.DoubleNegationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.DoubleNegationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.DoubleNegationCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (!visited.hasExtendedOperands()
				&& ASTNodes.hasOperator(visited, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.XOR)) {
			Expression leftExpression= visited.getLeftOperand();
			Expression rightExpression= visited.getRightOperand();

			Expression leftNegatedExpression= null;
			PrefixExpression leftPrefix= ASTNodes.as(leftExpression, PrefixExpression.class);
			if (leftPrefix != null && ASTNodes.hasOperator(leftPrefix, PrefixExpression.Operator.NOT)) {
				leftNegatedExpression= leftPrefix.getOperand();
			}

			Expression rightNegatedExpression= null;
			PrefixExpression rightPrefix= ASTNodes.as(rightExpression, PrefixExpression.class);
			if (rightPrefix != null && ASTNodes.hasOperator(rightPrefix, PrefixExpression.Operator.NOT)) {
				rightNegatedExpression= rightPrefix.getOperand();
			}

			if (leftNegatedExpression != null || rightNegatedExpression != null) {
				removeDoubleNegation(visited, leftExpression, rightExpression, leftNegatedExpression,
						rightNegatedExpression);
				return false;
			}
		}

		return true;
	}

	private void removeDoubleNegation(final InfixExpression visited, final Expression leftExpression,
			final Expression rightExpression, final Expression leftNegatedExpression, final Expression rightNegatedExpression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.DoubleNegationCleanUp_description);

		InfixExpression newInfixExpression= ast.newInfixExpression();

		if (leftNegatedExpression != null) {
			newInfixExpression.setLeftOperand(ASTNodes.createMoveTarget(rewrite, leftNegatedExpression));

			if (rightNegatedExpression != null) {
				newInfixExpression.setOperator(getAppropriateOperator(visited));
				newInfixExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, rightNegatedExpression));
			} else {
				newInfixExpression.setOperator(getNegatedOperator(visited));
				newInfixExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, rightExpression));
			}
		} else {
			newInfixExpression.setLeftOperand(ASTNodes.createMoveTarget(rewrite, leftExpression));
			newInfixExpression.setOperator(getNegatedOperator(visited));
			newInfixExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, rightNegatedExpression));
		}

		rewrite.replace(visited, newInfixExpression, group);
	}

	private InfixExpression.Operator getAppropriateOperator(final InfixExpression visited) {
		if (ASTNodes.hasOperator(visited, InfixExpression.Operator.NOT_EQUALS)) {
			return InfixExpression.Operator.XOR;
		}

		return visited.getOperator();
	}

	private InfixExpression.Operator getNegatedOperator(final InfixExpression node) {
		if (ASTNodes.hasOperator(node, InfixExpression.Operator.EQUALS)) {
			return InfixExpression.Operator.XOR;
		}

		return InfixExpression.Operator.EQUALS;
	}
}
