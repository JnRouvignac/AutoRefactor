/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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
import java.util.Collections;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class SeparateAssertionsRatherThanBooleanExpressionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.SeparateAssertionsRatherThanBooleanExpressionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.SeparateAssertionsRatherThanBooleanExpressionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.SeparateAssertionsRatherThanBooleanExpressionCleanUp_reason;
	}

	@Override
	public boolean visit(final ExpressionStatement visited) {
		if (!(visited.getExpression() instanceof MethodInvocation)) {
			return true;
		}

		MethodInvocation originalMethod= (MethodInvocation) visited.getExpression();

		return maybeRefactorAssertion(visited, originalMethod, "assertTrue", InfixExpression.Operator.CONDITIONAL_AND) //$NON-NLS-1$
				&& maybeRefactorAssertion(visited, originalMethod, "assertFalse", InfixExpression.Operator.CONDITIONAL_OR); //$NON-NLS-1$
	}

	private boolean maybeRefactorAssertion(final ExpressionStatement visited, final MethodInvocation originalMethod,
			final String methodName, final InfixExpression.Operator operator) {
		if (ASTNodes.usesGivenSignature(originalMethod, "org.junit.Assert", methodName, boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "junit.framework.Assert", methodName, boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "org.junit.jupiter.api.Assertions", methodName, boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "org.junit.jupiter.api.Assertions", methodName, boolean.class.getSimpleName(), String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "org.testng.Assert", methodName, boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "org.testng.Assert", methodName, boolean.class.getSimpleName(), String.class.getCanonicalName())) { //$NON-NLS-1$
			return maybeRefactorMethod(visited, originalMethod, operator, 0);
		}

		if (ASTNodes.usesGivenSignature(originalMethod, "org.junit.Assert", methodName, String.class.getCanonicalName(), boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "junit.framework.Assert", methodName, String.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorMethod(visited, originalMethod, operator, 1);
		}

		return true;
	}

	private boolean maybeRefactorMethod(final ExpressionStatement visited, final MethodInvocation originalMethod,
			final InfixExpression.Operator operator, final int parameterIndex) {
		InfixExpression booleanExpression= ASTNodes.as((Expression) originalMethod.arguments().get(parameterIndex), InfixExpression.class);

		if (booleanExpression != null && ASTNodes.hasOperator(booleanExpression, operator)) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.SeparateAssertionsRatherThanBooleanExpressionCleanUp_description);

			List<Expression> allOperands= ASTNodes.allOperands(booleanExpression);
			rewrite.replace(booleanExpression, ASTNodes.createMoveTarget(rewrite, allOperands.remove(0)), group);
			List<Statement> expressionStatements= new ArrayList<>(allOperands.size());

			for (Expression operand : allOperands) {
				List<Expression> newArguments= new ArrayList<>(originalMethod.arguments().size());

				for (Object argument : originalMethod.arguments()) {
					newArguments.add(rewrite.createCopyTarget(ASTNodes.getUnparenthesedExpression((Expression) argument)));
				}

				newArguments.set(parameterIndex, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(operand)));
				MethodInvocation newMethod;

				if (originalMethod.getExpression() != null) {
					MethodInvocation newMethodInvocation= ast.newMethodInvocation();
					newMethodInvocation.setExpression(rewrite.createCopyTarget(originalMethod.getExpression()));
					newMethodInvocation.setName(ast.newSimpleName(originalMethod.getName().getIdentifier()));
					newMethodInvocation.arguments().addAll(newArguments);
					newMethod= newMethodInvocation;
				} else {
					MethodInvocation newMethodInvocation= ast.newMethodInvocation();
					newMethodInvocation.setExpression(null);
					newMethodInvocation.setName(ast.newSimpleName(originalMethod.getName().getIdentifier()));
					newMethodInvocation.arguments().addAll(newArguments);
					newMethod= newMethodInvocation;
				}

				ExpressionStatement newStatement= ast.newExpressionStatement(newMethod);

				expressionStatements.add(newStatement);
			}

			if (ASTNodes.canHaveSiblings(visited)) {
				Collections.reverse(expressionStatements);

				for (Statement expressionStatement : expressionStatements) {
					rewrite.insertAfter(expressionStatement, visited, group);
				}
			} else {
				expressionStatements.add(0, ASTNodes.createMoveTarget(rewrite, visited));
				Block newBlock= ast.newBlock();
				newBlock.statements().addAll(expressionStatements);
				ASTNodes.replaceButKeepComment(rewrite, visited, newBlock, group);
			}

			return false;
		}

		return true;
	}
}
