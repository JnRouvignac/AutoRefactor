/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 * <p>
 * TODO JNR use CFG and expression analysis to find extra information about
 * expression nullness.
 * </p>
 */
public class ObsoleteInvertEqualsCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteInvertEqualsCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteInvertEqualsCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteInvertEqualsCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation visited) {
		if (visited.getExpression() == null || ASTNodes.is(visited.getExpression(), ThisExpression.class)) {
			return true;
		}

		if (ASTNodes.usesGivenSignature(visited, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(visited, String.class.getCanonicalName(), "equalsIgnoreCase", String.class.getCanonicalName())) { //$NON-NLS-1$
			Expression expression= visited.getExpression();
			Expression arg0= (Expression) visited.arguments().get(0);

			if (!ASTNodes.isConstant(expression)
					&& ASTNodes.isConstant(arg0)
					&& !ASTNodes.isPrimitive(arg0)) {
				invertEqualsInvocation(visited, expression, arg0);
				return false;
			}
		}

		return true;
	}

	private void invertEqualsInvocation(final MethodInvocation visited, final Expression expression, final Expression arg0) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteInvertEqualsCleanUp_description);

		ASTNodes.replaceButKeepComment(rewrite, visited.getExpression(), ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, arg0)), group);
		ASTNodes.replaceButKeepComment(rewrite, arg0, ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(expression)), group);
	}
}
