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
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class RedundantBooleanCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RedundantBooleanCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RedundantBooleanCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RedundantBooleanCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (ASTNodes.hasOperator(visited, InfixExpression.Operator.CONDITIONAL_OR)) {
			AtomicBoolean hasUselessOperand= new AtomicBoolean(false);
			List<Expression> remainingOperands= removeUselessOperands(visited, Boolean.FALSE, Boolean.TRUE, hasUselessOperand);

			if (hasUselessOperand.get()) {
				replaceWithNewInfixExpression(visited, remainingOperands);
				return false;
			}
		} else if (ASTNodes.hasOperator(visited, InfixExpression.Operator.CONDITIONAL_AND)) {
			AtomicBoolean hasUselessOperand= new AtomicBoolean(false);
			List<Expression> remainingOperands= removeUselessOperands(visited, Boolean.TRUE, Boolean.FALSE, hasUselessOperand);

			if (hasUselessOperand.get()) {
				replaceWithNewInfixExpression(visited, remainingOperands);
				return false;
			}
		}

		return true;
	}

	private List<Expression> removeUselessOperands(final InfixExpression visited, final Boolean neutralElement, final Boolean shortCircuitValue, final AtomicBoolean hasUselessOperand) {
		List<Expression> allOperands= ASTNodes.allOperands(visited);

		for (ListIterator<Expression> iterator= allOperands.listIterator(); iterator.hasNext();) {
			Expression operand= iterator.next();
			Object value= operand.resolveConstantExpressionValue();

			if (neutralElement.equals(value)) {
				hasUselessOperand.set(true);
				iterator.remove();
			} else if (shortCircuitValue.equals(value)) {
				while (iterator.hasNext()) {
					hasUselessOperand.set(true);
					iterator.next();
					iterator.remove();
				}

				return allOperands;
			}
		}

		return allOperands;
	}

	private void replaceWithNewInfixExpression(final InfixExpression visited, final List<Expression> remainingOperands) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.RedundantBooleanCleanUp_description);

		if (remainingOperands.size() == 1) {
			ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, remainingOperands.get(0)), group);
		} else {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			InfixExpression newInfixExpression= ast.newInfixExpression(visited.getOperator(), rewrite.createMoveTarget(remainingOperands));
			ASTNodes.replaceButKeepComment(rewrite, visited, newInfixExpression, group);
		}
	}
}
