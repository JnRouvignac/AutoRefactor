/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2019 Fabrice TIERCELIN - Reuse for Collection.containsAll()
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

import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ThisExpression;

/** See {@link #getDescription()} method. */
public class ContainsRatherThanLoopCleanUp extends AbstractCollectionMethodRatherThanLoopCleanUp {
	@Override
	public String getName() {
		return MultiFixMessages.ContainsRatherThanLoopCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ContainsRatherThanLoopCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ContainsRatherThanLoopCleanUp_reason;
	}

	@Override
	protected Expression getExpressionToFind(final MethodInvocation condition, final Expression forVar, final Expression iterable) {
		if (ASTNodes.as(iterable, ThisExpression.class) != null) {
			return null;
		}

		Expression expression= ASTNodes.getUnparenthesedExpression(condition.getExpression());
		Expression arg0= ASTNodes.getUnparenthesedExpression((Expression) condition.arguments().get(0));

		if (ASTNodes.isSameVariable(forVar, expression)) {
			return arg0;
		}

		if (ASTNodes.isSameVariable(forVar, arg0)) {
			return expression;
		}

		if (ASTNodes.match(forVar, expression)) {
			return arg0;
		}

		if (ASTNodes.match(forVar, arg0)) {
			return expression;
		}

		return null;
	}

	@Override
	protected MethodInvocation getMethodToReplace(final Expression condition) {
		MethodInvocation method= ASTNodes.as(condition, MethodInvocation.class);

		if (ASTNodes.usesGivenSignature(method, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
			return method;
		}

		return null;
	}

	@Override
	protected Expression newMethod(final Expression iterable, final Expression toFind, final boolean isPositive, final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		MethodInvocation invoke= ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, iterable), "contains", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(toFind))); //$NON-NLS-1$

		if (isPositive) {
			return invoke;
		}

		return ast.not(invoke);
	}
}
