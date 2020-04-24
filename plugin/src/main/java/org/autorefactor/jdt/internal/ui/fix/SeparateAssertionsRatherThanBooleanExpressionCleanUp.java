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
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class SeparateAssertionsRatherThanBooleanExpressionCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_SeparateAssertionsRatherThanBooleanExpressionCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_SeparateAssertionsRatherThanBooleanExpressionCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_SeparateAssertionsRatherThanBooleanExpressionCleanUp_reason;
	}

	@Override
	public boolean visit(final ExpressionStatement node) {
		if (!(node.getExpression() instanceof MethodInvocation)) {
			return true;
		}

		MethodInvocation originalMethod= (MethodInvocation) node.getExpression();

		return maybeRefactorAssertion(node, originalMethod, "assertTrue", InfixExpression.Operator.CONDITIONAL_AND) //$NON-NLS-1$
				&& maybeRefactorAssertion(node, originalMethod, "assertFalse", InfixExpression.Operator.CONDITIONAL_OR); //$NON-NLS-1$
	}

	private boolean maybeRefactorAssertion(final ExpressionStatement node, final MethodInvocation originalMethod,
			final String methodName, final Operator operator) {
		if (ASTNodes.usesGivenSignature(originalMethod, "org.testng.Assert", methodName, boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "org.testng.Assert", methodName, boolean.class.getSimpleName(), String.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "org.junit.Assert", methodName, boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "junit.framework.Assert", methodName, boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorMethod(node, originalMethod, operator, 0);
		}

		if (ASTNodes.usesGivenSignature(originalMethod, "org.junit.Assert", methodName, String.class.getCanonicalName(), boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(originalMethod, "junit.framework.Assert", methodName, String.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
			return maybeRefactorMethod(node, originalMethod, operator, 1);
		}

		return true;
	}

	private boolean maybeRefactorMethod(final ExpressionStatement node, final MethodInvocation originalMethod,
			final Operator operator, final int parameterIndex) {
		InfixExpression booleanExpression= ASTNodes.as((Expression) originalMethod.arguments().get(parameterIndex), InfixExpression.class);

		if (booleanExpression != null && ASTNodes.hasOperator(booleanExpression, operator)) {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			List<Expression> allOperands= ASTNodes.getAllOperands(booleanExpression);
			rewrite.replace(booleanExpression, rewrite.createMoveTarget(allOperands.remove(0)), null);
			List<Statement> expressionStatements= new ArrayList<>(allOperands.size());

			for (Expression operand : allOperands) {
				List<Expression> newArguments= new ArrayList<>(originalMethod.arguments().size());

				for (Object argument : originalMethod.arguments()) {
					newArguments.add(rewrite.createCopyTarget(ASTNodes.getUnparenthesedExpression((Expression) argument)));
				}

				newArguments.set(parameterIndex, rewrite.createMoveTarget(ASTNodes.getUnparenthesedExpression(operand)));
				MethodInvocation newMethod;

				if (originalMethod.getExpression() != null) {
					newMethod= ast.newMethodInvocation(rewrite.createCopyTarget(originalMethod.getExpression()), originalMethod.getName().getIdentifier(), newArguments);
				} else {
					newMethod= ast.newMethodInvocation(null, originalMethod.getName().getIdentifier(), newArguments);
				}

				ExpressionStatement newStatement= ast.toStatement(newMethod);

				expressionStatements.add(newStatement);
			}

			if (ASTNodes.canHaveSiblings(node)) {
				Collections.reverse(expressionStatements);

				for (Statement expressionStatement : expressionStatements) {
					rewrite.insertAfter(expressionStatement, node, null);
				}
			} else {
				expressionStatements.add(0, rewrite.createMoveTarget(node));
				Block newBlock= ast.block(expressionStatements);
				rewrite.replace(node, newBlock, null);
			}

			return false;
		}

		return true;
	}
}
