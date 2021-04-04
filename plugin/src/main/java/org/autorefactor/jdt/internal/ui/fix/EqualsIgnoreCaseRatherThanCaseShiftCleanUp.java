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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class EqualsIgnoreCaseRatherThanCaseShiftCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.EqualsIgnoreCaseRatherThanCaseShiftCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.EqualsIgnoreCaseRatherThanCaseShiftCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.EqualsIgnoreCaseRatherThanCaseShiftCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
			MethodInvocation leftInvocation= ASTNodes.as(visited.getExpression(), MethodInvocation.class);
			MethodInvocation rightInvocation= ASTNodes.as((Expression) visited.arguments().get(0), MethodInvocation.class);

			if (leftInvocation != null
					&& rightInvocation != null
					&& (ASTNodes.usesGivenSignature(leftInvocation, String.class.getCanonicalName(), "toLowerCase") //$NON-NLS-1$
							&& ASTNodes.usesGivenSignature(rightInvocation, String.class.getCanonicalName(), "toLowerCase") //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(leftInvocation, String.class.getCanonicalName(), "toUpperCase") //$NON-NLS-1$
									&& ASTNodes.usesGivenSignature(rightInvocation, String.class.getCanonicalName(), "toUpperCase"))) { //$NON-NLS-1$
				refactorEquals(visited, leftInvocation, rightInvocation);
				return false;
			}
		} else if (ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "equalsIgnoreCase", String.class.getCanonicalName())) { //$NON-NLS-1$
			Expression leftExpression= getReducedStringExpression(visited.getExpression());
			Expression rightExpression= getReducedStringExpression((Expression) visited.arguments().get(0));

			if (leftExpression != null || rightExpression != null) {
				refactorEqualsIgnoreCase(visited, leftExpression, rightExpression);
				return false;
			}
		}

		return true;
	}

	private Expression getReducedStringExpression(final Expression stringExpression) {
		MethodInvocation casingInvocation= ASTNodes.as(stringExpression, MethodInvocation.class);

		if (casingInvocation != null
				&& (ASTNodes.usesGivenSignature(casingInvocation, String.class.getCanonicalName(), "toLowerCase") //$NON-NLS-1$
						|| ASTNodes.usesGivenSignature(casingInvocation, String.class.getCanonicalName(), "toUpperCase"))) { //$NON-NLS-1$
			return casingInvocation.getExpression();
		}

		return null;
	}

	private void refactorEquals(final MethodInvocation visited, final MethodInvocation leftInvocation,
			final MethodInvocation rightInvocation) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.EqualsIgnoreCaseRatherThanCaseShiftCleanUp_description);

		rewrite.replace(leftInvocation, ASTNodes.createMoveTarget(rewrite, leftInvocation.getExpression()), group);
		rewrite.replace(visited.getName(), ast.newSimpleName("equalsIgnoreCase"), group); //$NON-NLS-1$
		rewrite.replace(rightInvocation, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(rightInvocation.getExpression())), group);
	}

	private void refactorEqualsIgnoreCase(final MethodInvocation visited, final Expression leftExpressionIfChanged,
			final Expression rightExpressionIfChanged) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.EqualsIgnoreCaseRatherThanCaseShiftCleanUp_description);

		if (leftExpressionIfChanged != null) {
			rewrite.replace(visited.getExpression(), ASTNodes.createMoveTarget(rewrite, leftExpressionIfChanged), group);
		}

		if (rightExpressionIfChanged != null) {
			rewrite.replace((Expression) visited.arguments().get(0), ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(rightExpressionIfChanged)), group);
		}
	}
}
