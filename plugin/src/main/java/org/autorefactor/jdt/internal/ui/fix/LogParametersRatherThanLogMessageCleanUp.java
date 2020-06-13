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

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
public class LogParametersRatherThanLogMessageCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_LogParametersRatherThanLogMessageCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		return maybeRefactorMethod(node, "debug") && maybeRefactorMethod(node, "error") //$NON-NLS-1$ //$NON-NLS-2$
				&& maybeRefactorMethod(node, "info") && maybeRefactorMethod(node, "trace") //$NON-NLS-1$ //$NON-NLS-2$
				&& maybeRefactorMethod(node, "warn"); //$NON-NLS-1$
	}

	private boolean maybeRefactorMethod(final MethodInvocation node, final String methodName) {
		if (ASTNodes.usesGivenSignature(node, "org.slf4j.Logger", methodName, String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(node, "ch.qos.logback.classic.Logger", methodName, String.class.getCanonicalName())) { //$NON-NLS-1$
			@SuppressWarnings("unchecked")
			List<Expression> args= node.arguments();

			if (args != null && args.size() == 1) {
				InfixExpression message= ASTNodes.as(args.get(0), InfixExpression.class);

				if (message != null) {
					return maybeReplaceConcatenation(node, methodName, message);
				}
			}
		}

		return true;
	}

	private boolean maybeReplaceConcatenation(final MethodInvocation node, final String methodName,
			final InfixExpression message) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		StringBuilder messageBuilder= new StringBuilder();
		List<Expression> allOperands= ASTNodes.allOperands(message);
		List<Expression> params= new ArrayList<>(allOperands.size());
		boolean hasLiteral= false;
		boolean hasObjects= false;

		for (Expression string : allOperands) {
			if (string instanceof StringLiteral) {
				hasLiteral= true;
				String literal= (String) string.resolveConstantExpressionValue();

				if (literal != null && (literal.contains("{") || literal.contains("}"))) { //$NON-NLS-1$ //$NON-NLS-2$
					return true;
				}

				messageBuilder.append(literal);
			} else {
				hasObjects= true;
				messageBuilder.append("{}"); //$NON-NLS-1$

				if (ASTNodes.hasType(string, Throwable.class.getCanonicalName())) {
					params.add(ast.newMethodInvocation(String.class.getSimpleName(), "valueOf", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(string)))); //$NON-NLS-1$
				} else {
					params.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(string)));
				}
			}
		}

		if (hasLiteral && hasObjects) {
			replaceConcatenation(node, methodName, messageBuilder, params);
			return false;
		}

		return true;
	}

	private void replaceConcatenation(final MethodInvocation node, final String methodName, final StringBuilder messageBuilder,
			final List<Expression> params) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		params.add(0, ast.string(messageBuilder.toString()));
		rewrite.replace(node, ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, node.getExpression()), methodName, params), null);
	}
}
