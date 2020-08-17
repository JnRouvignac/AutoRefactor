/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice TIERCELIN - initial API and implementation
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

import java.util.Collection;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ContainsAllRatherThanLoopCleanUp extends AbstractCollectionMethodRatherThanLoopCleanUp {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_ContainsAllRatherThanLoopCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_ContainsAllRatherThanLoopCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_ContainsAllRatherThanLoopCleanUp_reason;
	}

	@Override
	protected Expression getExpressionToFind(final MethodInvocation condition, final Expression forVar, final Expression iterable) {
		Expression expression= ASTNodes.getUnparenthesedExpression(condition.getExpression());
		Expression arg0= ASTNodes.getUnparenthesedExpression((Expression) condition.arguments().get(0));

		if (ASTNodes.isSameVariable(forVar, arg0) || ASTNodes.match(forVar, arg0)) {
			return expression;
		}

		return null;
	}

	@Override
	protected MethodInvocation getMethodToReplace(final Expression condition) {
		PrefixExpression negation= ASTNodes.as(condition, PrefixExpression.class);

		if (negation != null && ASTNodes.hasOperator(negation, PrefixExpression.Operator.NOT)) {
			MethodInvocation method= ASTNodes.as(negation.getOperand(), MethodInvocation.class);

			if (ASTNodes.usesGivenSignature(method, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName()) //$NON-NLS-1$
					&& method.getExpression() != null
					&& !ASTNodes.is(method.getExpression(), ThisExpression.class)) {
				return method;
			}
		}

		return null;
	}

	@Override
	protected Expression newMethod(final Expression iterable, final Expression toFind, final boolean isPositive, final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_ContainsAllRatherThanLoopCleanUp_name);

		MethodInvocation invoke= ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, toFind), "containsAll", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(iterable))); //$NON-NLS-1$

		if (isPositive) {
			return ast.not(invoke);
		}

		return invoke;
	}
}
