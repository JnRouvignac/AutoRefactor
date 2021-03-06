/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Removes empty statements.
 *
 * @see #getDescription()
 */
public class RemoveEmptyStatementCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveEmptyStatementCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveEmptyStatementCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveEmptyStatementCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement visited) {
		if (ASTNodes.isPassiveWithoutFallingThrough(visited.getExpression())) {
			boolean isThenEmpty= isEmptyCode(visited.getThenStatement());
			boolean isElseEmpty= isEmptyCode(visited.getElseStatement());

			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptyStatementCleanUp_description);

			if (isThenEmpty && (isElseEmpty || visited.getElseStatement() == null)) {
				if (ASTNodes.canHaveSiblings(visited) || visited.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
					rewrite.remove(visited, group);
				} else {
					ASTNodeFactory ast= cuRewrite.getASTBuilder();

					ASTNodes.replaceButKeepComment(rewrite, visited, ast.newBlock(), group);
				}

				return false;
			}
			if (isElseEmpty) {
				rewrite.remove(visited.getElseStatement(), group);
				return false;
			}
		}

		return true;
	}

	@Override
	public boolean visit(final EnhancedForStatement visited) {
		if (visited.getExpression().resolveTypeBinding() != null
				&& visited.getExpression().resolveTypeBinding().isArray()
				&& ASTNodes.isPassiveWithoutFallingThrough(visited.getExpression())) {
			return maybeRemoveStmtWithEmptyBody(visited, visited.getBody());
		}

		return true;
	}

	@Override
	public boolean visit(final ForStatement visited) {
		if (visited.getExpression() != null
				&& !Boolean.TRUE.equals(visited.getExpression().resolveConstantExpressionValue())
				&& arePassive(visited.initializers())
				&& ASTNodes.isPassiveWithoutFallingThrough(visited.getExpression())) {
			return maybeRemoveStmtWithEmptyBody(visited, visited.getBody());
		}

		return true;
	}

	@Override
	public boolean visit(final WhileStatement visited) {
		if (ASTNodes.isPassiveWithoutFallingThrough(visited.getExpression())
				&& !Boolean.TRUE.equals(visited.getExpression().resolveConstantExpressionValue())) {
			return maybeRemoveStmtWithEmptyBody(visited, visited.getBody());
		}

		return true;
	}

	@Override
	public boolean visit(final DoStatement visited) {
		if (ASTNodes.isPassiveWithoutFallingThrough(visited.getExpression())
				&& !Boolean.TRUE.equals(visited.getExpression().resolveConstantExpressionValue())) {
			return maybeRemoveStmtWithEmptyBody(visited, visited.getBody());
		}

		return true;
	}

	@Override
	public boolean visit(final Block visited) {
		if ((ASTNodes.canHaveSiblings(visited) || visited.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) && isEmptyCode(visited)) {
			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptyStatementCleanUp_description);
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			rewrite.remove(visited, group);
			return false;
		}

		return true;
	}

	@Override
	public boolean visit(final EmptyStatement visited) {
		if (isEmptyCode(visited)) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptyStatementCleanUp_description);

			if (ASTNodes.canHaveSiblings(visited) || visited.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
				rewrite.remove(visited, group);
				return false;
			}

			if (visited instanceof EmptyStatement) {
				ASTNodeFactory ast= cuRewrite.getASTBuilder();

				ASTNodes.replaceButKeepComment(rewrite, visited, ast.newBlock(), group);
				return false;
			}
		}

		return true;
	}

	private boolean arePassive(final List<?> initializers) {
		if (initializers != null) {
			for (Object initializer : initializers) {
				if (!ASTNodes.isPassiveWithoutFallingThrough((Expression) initializer)) {
					return false;
				}
			}
		}

		return true;
	}

	private boolean maybeRemoveStmtWithEmptyBody(final Statement visited, final Statement emptyCode) {
		if (isEmptyCode(emptyCode)) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveEmptyStatementCleanUp_description);

			if (ASTNodes.canHaveSiblings(visited) || visited.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
				rewrite.remove(visited, group);
				return false;
			}

			if (visited instanceof EmptyStatement) {
				ASTNodeFactory ast= cuRewrite.getASTBuilder();

				ASTNodes.replaceButKeepComment(rewrite, visited, ast.newBlock(), group);
				return false;
			}
		}

		return true;
	}

	private boolean isEmptyCode(final Statement emptyCode) {
		if (emptyCode instanceof EmptyStatement) {
			return true;
		}

		if (emptyCode instanceof Block) {
			Block block= (Block) emptyCode;
			return Utils.isEmpty(block.statements());
		}

		return false;
	}
}
