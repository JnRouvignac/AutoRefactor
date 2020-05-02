/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.OrderedInfixExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ThisExpression;

/** See {@link #getDescription()} method. */
public class IsEmptyRatherThanSizeCleanUp extends AbstractCleanUpRule {
	private static final String IS_EMPTY_METHOD= "isEmpty"; //$NON-NLS-1$
	private static final String LENGTH_METHOD= "length"; //$NON-NLS-1$
	private static final String SIZE_METHOD= "size"; //$NON-NLS-1$

	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_IsEmptyRatherThanSizeCleanUp_reason;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		OrderedInfixExpression<MethodInvocation, Expression> orderedCondition= ASTNodes.orderedInfix(node, MethodInvocation.class, Expression.class);

		if (orderedCondition != null) {
			MethodInvocation miToReplace= orderedCondition.getFirstOperand();
			Long literalSize= ASTNodes.getIntegerLiteral(orderedCondition.getSecondOperand());

			if (literalSize != null
					&& miToReplace.getExpression() != null
					&& !ASTNodes.is(miToReplace.getExpression(), ThisExpression.class)
					&& (ASTNodes.usesGivenSignature(miToReplace, Collection.class.getCanonicalName(), SIZE_METHOD) || ASTNodes.usesGivenSignature(miToReplace, Map.class.getCanonicalName(), SIZE_METHOD)
							|| ASTNodes.usesGivenSignature(miToReplace, String.class.getCanonicalName(), LENGTH_METHOD) && getJavaMinorVersion() >= 6)) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				ASTNodeFactory ast= cuRewrite.getASTBuilder();

				if (literalSize == 0) {
					if (Arrays.asList(InfixExpression.Operator.GREATER, InfixExpression.Operator.NOT_EQUALS).contains(orderedCondition.getOperator())) {
						rewrite.replace(node, ast.not(ast.newMethodInvocation(ast.copyExpression(miToReplace), IS_EMPTY_METHOD)), null);
						return false;
					}

					if (Arrays.asList(InfixExpression.Operator.EQUALS, InfixExpression.Operator.LESS_EQUALS).contains(orderedCondition.getOperator())) {
						rewrite.replace(node, ast.newMethodInvocation(ast.copyExpression(miToReplace), IS_EMPTY_METHOD), null);
						return false;
					}
				} else if (literalSize == 1) {
					if (InfixExpression.Operator.GREATER_EQUALS.equals(orderedCondition.getOperator())) {
						rewrite.replace(node, ast.not(ast.newMethodInvocation(ast.copyExpression(miToReplace), IS_EMPTY_METHOD)), null);
						return false;
					}

					if (InfixExpression.Operator.LESS.equals(orderedCondition.getOperator())) {
						rewrite.replace(node, ast.newMethodInvocation(ast.copyExpression(miToReplace), IS_EMPTY_METHOD), null);
						return false;
					}
				}
			}
		}

		return true;
	}
}
