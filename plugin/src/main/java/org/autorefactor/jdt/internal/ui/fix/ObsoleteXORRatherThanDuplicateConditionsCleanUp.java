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

import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteXORRatherThanDuplicateConditionsCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteXORRatherThanDuplicateConditionsCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteXORRatherThanDuplicateConditionsCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteXORRatherThanDuplicateConditionsCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		if (ASTNodes.hasOperator(node, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR)
				&& !node.hasExtendedOperands()) {
			InfixExpression firstCondition= ASTNodes.as(node.getLeftOperand(), InfixExpression.class);
			InfixExpression secondCondition= ASTNodes.as(node.getRightOperand(), InfixExpression.class);

			if (firstCondition != null
					&& secondCondition != null
					&& !firstCondition.hasExtendedOperands()
					&& !secondCondition.hasExtendedOperands()
					&& ASTNodes.hasOperator(firstCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)
					&& ASTNodes.hasOperator(secondCondition, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.AND)
					&& ASTNodes.isPassive(firstCondition.getLeftOperand())
					&& ASTNodes.isPassive(firstCondition.getRightOperand())
					&& ASTNodes.isPassive(secondCondition.getLeftOperand())
					&& ASTNodes.isPassive(secondCondition.getRightOperand())) {
				return maybeReplaceDuplicateExpression(node, firstCondition.getLeftOperand(), secondCondition.getLeftOperand(),
						firstCondition.getRightOperand(), secondCondition.getRightOperand())
						&& maybeReplaceDuplicateExpression(node, firstCondition.getLeftOperand(), secondCondition.getRightOperand(),
								firstCondition.getRightOperand(), secondCondition.getLeftOperand());
			}
		}

		return true;
	}

	private boolean maybeReplaceDuplicateExpression(final InfixExpression node, final Expression firstExpression,
			final Expression firstOppositeExpression, final Expression secondExpression, final Expression secondOppositeExpression) {
		if (ASTSemanticMatcher.INSTANCE.matchNegative(firstExpression, firstOppositeExpression)
				&& ASTSemanticMatcher.INSTANCE.matchNegative(secondExpression, secondOppositeExpression)) {
			AtomicBoolean isFirstExprPositive= new AtomicBoolean();
			AtomicBoolean isSecondExprPositive= new AtomicBoolean();

			Expression firstBasicExpression= getBasisExpression(firstExpression, isFirstExprPositive);
			Expression secondBasicExpression= getBasisExpression(secondExpression, isSecondExprPositive);

			replaceDuplicateExpression(node, firstBasicExpression, secondBasicExpression, isFirstExprPositive, isSecondExprPositive);
			return false;
		}

		return true;
	}

	private Expression getBasisExpression(final Expression originalExpression, final AtomicBoolean isExprPositive) {
		Expression basisExpression= null;
		PrefixExpression negateExpression= ASTNodes.as(originalExpression, PrefixExpression.class);

		if (ASTNodes.hasOperator(negateExpression, PrefixExpression.Operator.NOT)) {
			basisExpression= negateExpression.getOperand();
			isExprPositive.lazySet(false);
		} else {
			basisExpression= originalExpression;
			isExprPositive.lazySet(true);
		}

		return basisExpression;
	}

	private void replaceDuplicateExpression(final InfixExpression node, final Expression firstExpression,
			final Expression secondExpression, final AtomicBoolean isFirstExprPositive,
			final AtomicBoolean isSecondExprPositive) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteXORRatherThanDuplicateConditionsCleanUp_description);

		InfixExpression newInfixExpression= ast.newInfixExpression();
		newInfixExpression.setLeftOperand(ASTNodes.createMoveTarget(rewrite, firstExpression));
		newInfixExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, secondExpression));

		if (isFirstExprPositive.get() == isSecondExprPositive.get()) {
			newInfixExpression.setOperator(InfixExpression.Operator.EQUALS);
		} else {
			newInfixExpression.setOperator(InfixExpression.Operator.XOR);
		}

		ASTNodes.replaceButKeepComment(rewrite, node, newInfixExpression, group);
	}
}
