/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Inline the blocks
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
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class RemoveEmptyIfCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveEmptyIfCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveEmptyIfCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveEmptyIfCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptyIfCleanUp_description);

		Statement thenStatement= node.getThenStatement();
		Statement elseStatement= node.getElseStatement();

		if (elseStatement != null && ASTNodes.asList(elseStatement).isEmpty()) {
			rewrite.remove(elseStatement, group);
			return false;
		}

		if (thenStatement != null && ASTNodes.asList(thenStatement).isEmpty()) {
			Expression condition= node.getExpression();

			if (elseStatement != null) {
				ASTNodes.replaceButKeepComment(rewrite, condition, cuRewrite.getASTBuilder().negate(condition), group);
				ASTNodes.replaceButKeepComment(rewrite, node.getThenStatement(), ASTNodes.createMoveTarget(rewrite, elseStatement), group);
			} else if (ASTNodes.isPassiveWithoutFallingThrough(condition)) {
				removeBlock(node);
				return false;
			}
		}

		return true;
	}

	private void removeBlock(final IfStatement node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptyIfCleanUp_description);

		if (ASTNodes.canHaveSiblings(node) || node.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
			rewrite.remove(node, group);
		} else {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			ASTNodes.replaceButKeepComment(rewrite, node, ast.newBlock(), group);
		}
	}
}
