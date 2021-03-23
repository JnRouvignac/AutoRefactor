/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice TIERCELIN - initial API and implementation
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
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.TypeLiteral;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteInstanceofRatherThanIsInstanceCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteInstanceofRatherThanIsInstanceCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteInstanceofRatherThanIsInstanceCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteInstanceofRatherThanIsInstanceCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		TypeLiteral klass= ASTNodes.as(node.getExpression(), TypeLiteral.class);

		if (klass != null
				&& !klass.getType().resolveBinding().isPrimitive()
				&& ASTNodes.usesGivenSignature(node, Class.class.getCanonicalName(), "isInstance", Object.class.getCanonicalName())) { //$NON-NLS-1$
			replace(node, klass);
			return false;
		}

		return true;
	}

	private void replace(final MethodInvocation node, final TypeLiteral klass) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteInstanceofRatherThanIsInstanceCleanUp_description);

		InstanceofExpression newInstanceofExpression= ast.newInstanceofExpression();
		newInstanceofExpression.setLeftOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, (Expression) node.arguments().get(0))));
		newInstanceofExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, klass.getType()));

		ASTNodes.replaceButKeepComment(rewrite, node, ASTNodeFactory.parenthesizeIfNeeded(ast, newInstanceofExpression), group);
	}
}
