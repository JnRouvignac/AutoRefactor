/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class DoubleCompareRatherThanEqualityCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.DoubleCompareRatherThanEqualityCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.DoubleCompareRatherThanEqualityCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.DoubleCompareRatherThanEqualityCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (!visited.hasExtendedOperands()
				&& ASTNodes.hasOperator(visited, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.GREATER_EQUALS, InfixExpression.Operator.LESS, InfixExpression.Operator.GREATER)
				&& ASTNodes.hasType(visited.getLeftOperand(), double.class.getSimpleName(), Double.class.getCanonicalName())
				&& ASTNodes.hasType(visited.getRightOperand(), double.class.getSimpleName(), Double.class.getCanonicalName())) {
			replace(visited);
			return false;
		}

		return true;
	}

	private void replace(final InfixExpression visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.DoubleCompareRatherThanEqualityCleanUp_description);

		InfixExpression newInfixExpression= ast.newInfixExpression();
		newInfixExpression.setLeftOperand(ast.newMethodInvocation(Double.class.getSimpleName(), "compare", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(visited.getLeftOperand())), ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(visited.getRightOperand())))); //$NON-NLS-1$
		newInfixExpression.setOperator(visited.getOperator());
		newInfixExpression.setRightOperand(ast.newNumberLiteral("0")); //$NON-NLS-1$

		ASTNodes.replaceButKeepComment(rewrite, visited, newInfixExpression, group);
	}
}
