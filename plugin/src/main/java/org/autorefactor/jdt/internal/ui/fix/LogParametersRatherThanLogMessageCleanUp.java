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
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class LogParametersRatherThanLogMessageCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.LogParametersRatherThanLogMessageCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.LogParametersRatherThanLogMessageCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.LogParametersRatherThanLogMessageCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		return maybeRefactorMethod(visited, "debug") //$NON-NLS-1$
				&& maybeRefactorMethod(visited, "error") //$NON-NLS-1$
				&& maybeRefactorMethod(visited, "info") //$NON-NLS-1$
				&& maybeRefactorMethod(visited, "trace") //$NON-NLS-1$
				&& maybeRefactorMethod(visited, "warn"); //$NON-NLS-1$
	}

	private boolean maybeRefactorMethod(final MethodInvocation visited, final String methodName) {
		if (ASTNodes.usesGivenSignature(visited, "org.slf4j.Logger", methodName, String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, "ch.qos.logback.classic.Logger", methodName, String.class.getCanonicalName())) { //$NON-NLS-1$
			List<Expression> args= visited.arguments();

			if (args != null && args.size() == 1) {
				InfixExpression message= ASTNodes.as(args.get(0), InfixExpression.class);

				if (message != null) {
					return maybeReplaceConcatenation(visited, methodName, message);
				}
			}
		}

		return true;
	}

	private boolean maybeReplaceConcatenation(final MethodInvocation visited, final String methodName,
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

				// Due to a bug in JDT Core, the literal may be null
				if (literal == null || literal.contains("{") || literal.contains("}")) { //$NON-NLS-1$ //$NON-NLS-2$
					return true;
				}

				messageBuilder.append(literal);
			} else {
				hasObjects= true;
				messageBuilder.append("{}"); //$NON-NLS-1$

				if (ASTNodes.hasType(string, Throwable.class.getCanonicalName())) {
					MethodInvocation valueOfMethod= ast.newMethodInvocation();
					valueOfMethod.setExpression(ASTNodeFactory.newName(ast, String.class.getSimpleName()));
					valueOfMethod.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
					valueOfMethod.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(string)));
					MethodInvocation newMethodInvocation= valueOfMethod;
					params.add(newMethodInvocation);
				} else {
					params.add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(string)));
				}
			}
		}

		if (hasLiteral && hasObjects) {
			replaceConcatenation(visited, methodName, messageBuilder, params);
			return false;
		}

		return true;
	}

	private void replaceConcatenation(final MethodInvocation visited, final String methodName, final StringBuilder messageBuilder,
			final List<Expression> params) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.LogParametersRatherThanLogMessageCleanUp_description);

		params.add(0, ast.newStringLiteral(messageBuilder.toString()));
		MethodInvocation newMethodInvocation= ast.newMethodInvocation();
		newMethodInvocation.setExpression(ASTNodes.createMoveTarget(rewrite, visited.getExpression()));
		newMethodInvocation.setName(ast.newSimpleName(methodName));
		newMethodInvocation.arguments().addAll(params);
		rewrite.replace(visited, newMethodInvocation, group);
	}
}
