/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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

import java.util.Map;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class MethodOnMapRatherThanMethodOnKeySetCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.MethodOnMapRatherThanMethodOnKeySetCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.MethodOnMapRatherThanMethodOnKeySetCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.MethodOnMapRatherThanMethodOnKeySetCleanUp_reason;
	}

	@Override
	public boolean visit(final MethodInvocation node) {
		MethodInvocation miExpression= ASTNodes.as(node.getExpression(), MethodInvocation.class);

		if (miExpression != null
				&& miExpression.getExpression() != null
				&& !ASTNodes.is(miExpression.getExpression(), ThisExpression.class)
				&& ASTNodes.usesGivenSignature(miExpression, Map.class.getCanonicalName(), "keySet")) { //$NON-NLS-1$
			if (ASTNodes.usesGivenSignature(node, Set.class.getCanonicalName(), "clear") //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Set.class.getCanonicalName(), "size") //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Set.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(node, Set.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
							// If parent is not an expression statement, the MethodInvocation must return a
							// boolean.
							// In that case, we cannot replace because `Map.removeKey(key) != null`
							// is not strictly equivalent to `Map.keySet().remove(key)`
							&& node.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				TextEditGroup group= new TextEditGroup(MultiFixMessages.MethodOnMapRatherThanMethodOnKeySetCleanUp_description);

				rewrite.replace(node.getExpression(), ASTNodes.createMoveTarget(rewrite, miExpression.getExpression()), group);
				return false;
			}

			if (ASTNodes.usesGivenSignature(node, Set.class.getCanonicalName(), "contains", Object.class.getCanonicalName())) { //$NON-NLS-1$
				replaceContains(node, miExpression);
				return false;
			}
		}

		return true;
	}

	private void replaceContains(final MethodInvocation node, final MethodInvocation miExpression) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.MethodOnMapRatherThanMethodOnKeySetCleanUp_description);

		rewrite.replace(node.getExpression(), ASTNodes.createMoveTarget(rewrite, miExpression.getExpression()), group);
		rewrite.replace(node.getName(), ast.newSimpleName("containsKey"), group); //$NON-NLS-1$
	}
}
