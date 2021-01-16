/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class StringValueOfRatherThanConcatCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.StringValueOfRatherThanConcatCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.StringValueOfRatherThanConcatCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.StringValueOfRatherThanConcatCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression visited) {
		if (ASTNodes.hasOperator(visited, InfixExpression.Operator.PLUS)) {
			Expression leftOperand= visited.getLeftOperand();
			Expression rightOperand= visited.getRightOperand();

			return maybeReplaceStringConcatenation(visited, leftOperand, rightOperand)
					// If not replaced then try the other way round
					&& maybeReplaceStringConcatenation(visited, rightOperand, leftOperand);
		}

		return true;
	}

	private boolean maybeReplaceStringConcatenation(final InfixExpression visited, final Expression expression,
			final Expression variable) {
		StringLiteral stringLiteral= ASTNodes.as(expression, StringLiteral.class);

		if (stringLiteral != null
				&& stringLiteral.getLiteralValue().matches("") //$NON-NLS-1$
				&& !ASTNodes.hasType(variable, String.class.getCanonicalName(), "char[]")) { //$NON-NLS-1$
			replaceStringConcatenation(visited, variable);
			return false;
		}

		return true;
	}

	private void replaceStringConcatenation(final InfixExpression visited, final Expression variable) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.StringValueOfRatherThanConcatCleanUp_description);

		MethodInvocation newInvoke= ast.newMethodInvocation();
		newInvoke.setExpression(ASTNodeFactory.newName(ast, String.class.getSimpleName()));
		newInvoke.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
		newInvoke.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(variable)));

		if (visited.hasExtendedOperands()) {
			List<Expression> extendedOperands= visited.extendedOperands();
			List<Expression> newOperands= new ArrayList<>(1 + extendedOperands.size());
			newOperands.add(newInvoke);
			newOperands.addAll(ASTNodes.createMoveTarget(rewrite, extendedOperands));

			ASTNodes.replaceButKeepComment(rewrite, visited, ast.newInfixExpression(InfixExpression.Operator.PLUS, newOperands), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, visited, newInvoke, group);
		}
	}
}
