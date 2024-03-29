/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoletePushNegationDownCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoletePushNegationDownCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoletePushNegationDownCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoletePushNegationDownCleanUp_reason;
	}

	@Override
	public boolean visit(final PrefixExpression node) {
		if (!ASTNodes.hasOperator(node, PrefixExpression.Operator.NOT)) {
			return true;
		}

		Expression replacement= getOppositeExpression(node.getOperand());

		if (replacement != null) {
			TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoletePushNegationDownCleanUp_description);
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			ASTNodes.replaceButKeepComment(rewrite, node, replacement, group);
			return false;
		}

		return true;
	}

	private Expression getOppositeExpression(final Expression negativeExpression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		Expression operand= ASTNodes.getUnparenthesedExpression(negativeExpression);

		if (operand instanceof PrefixExpression) {
			PrefixExpression pe= (PrefixExpression) operand;

			if (ASTNodes.hasOperator(pe, PrefixExpression.Operator.NOT)) {
				return ASTNodes.createMoveTarget(rewrite, pe.getOperand());
			}
		} else if (operand instanceof InfixExpression) {
			InfixExpression infixExpression= (InfixExpression) operand;
			InfixExpression.Operator reverseOp= (InfixExpression.Operator) OperatorEnum.getOperator(infixExpression).getReverseBooleanOperator();

			if (reverseOp != null) {
				List<Expression> allOperands= new ArrayList<>(ASTNodes.allOperands(infixExpression));

				if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.AND, InfixExpression.Operator.OR)) {
					for (ListIterator<Expression> it= allOperands.listIterator(); it.hasNext();) {
						Expression anOperand= it.next();
						Expression oppositeOperand= getOppositeExpression(anOperand);

						it.set(oppositeOperand != null ? oppositeOperand : ast.negate(anOperand, true));
					}
				} else {
					allOperands= ASTNodes.createMoveTarget(rewrite, allOperands);
				}

				return ast.newParenthesizedExpression(ast.newInfixExpression(reverseOp, allOperands));
			}
		} else {
			Boolean constant= ASTNodes.getBooleanLiteral(operand);

			if (constant != null) {
				return ast.newBooleanLiteral(!constant);
			}
		}

		return null;
	}
}
