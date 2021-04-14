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
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class RedundantTruthCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RedundantTruthCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RedundantTruthCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RedundantTruthCleanUp_reason;
	}

	@Override
	public boolean visit(final ParenthesizedExpression visited) {
		InfixExpression originalCondition= ASTNodes.as(visited, InfixExpression.class);

		if (originalCondition != null) {
			return maybeRefactorInfixExpression(visited, originalCondition);
		}

		return true;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		return maybeRefactorInfixExpression(visited, visited);
	}

	private boolean maybeRefactorInfixExpression(final ASTNode visited, final InfixExpression originalCondition) {
		if (!originalCondition.hasExtendedOperands()
				&& ASTNodes.hasOperator(originalCondition, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.XOR)
				// Either:
				// - Two boolean primitives: no possible NPE
				// - One boolean primitive and one Boolean object, this code already run
				// the risk of an NPE, so we can replace the infix expression without
				// fearing we would introduce a previously non existing NPE.
				&& (ASTNodes.isPrimitive(originalCondition.getLeftOperand(), boolean.class.getSimpleName()) || ASTNodes.isPrimitive(originalCondition.getRightOperand(), boolean.class.getSimpleName()))) {
			Expression leftExpression= originalCondition.getLeftOperand();
			Expression rightExpression= originalCondition.getRightOperand();
			boolean isEquals= ASTNodes.hasOperator(originalCondition, InfixExpression.Operator.EQUALS);

			return maybeRemoveConstantOperand(visited, leftExpression, rightExpression, isEquals)
					&& maybeRemoveConstantOperand(visited, rightExpression, leftExpression, isEquals);
		}

		return true;
	}

	private boolean maybeRemoveConstantOperand(final ASTNode visited, final Expression dynamicOperand,
			final Expression hardCodedOperand, final boolean isEquals) {
		Boolean booleanLiteral= ASTNodes.getBooleanLiteral(hardCodedOperand);

		if (booleanLiteral != null) {
			removeBooleanConstant(visited, dynamicOperand, booleanLiteral, isEquals);
			return false;
		}

		return true;
	}

	private void removeBooleanConstant(final ASTNode visited, final Expression expressionToCopy,
			final boolean isTrue, final boolean isEquals) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.RedundantTruthCleanUp_description);

		Expression operand;
		if (isTrue == isEquals) {
			operand= ASTNodes.createMoveTarget(rewrite, expressionToCopy);
		} else {
			operand= ast.negate(expressionToCopy, true);
		}

		rewrite.replace(visited, ASTNodeFactory.parenthesizeIfNeeded(ast, operand), group);
	}
}
