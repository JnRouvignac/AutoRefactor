/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class InlineCodeRatherThanPeremptoryConditionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.InlineCodeRatherThanPeremptoryConditionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.InlineCodeRatherThanPeremptoryConditionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.InlineCodeRatherThanPeremptoryConditionCleanUp_reason;
	}

	@Override
	public boolean visit(final Block visited) {
		IfAndFollowingCodeVisitor ifAndFollowingCodeVisitor= new IfAndFollowingCodeVisitor();
		ifAndFollowingCodeVisitor.visitNode(visited);
		return ifAndFollowingCodeVisitor.result;
	}

	private final class IfAndFollowingCodeVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final TryStatement visited) {
			if (result && visited.resources().isEmpty()) {
				List<Statement> tryStatements= ASTNodes.asList(visited.getBody());

				if (tryStatements.isEmpty()) {
					List<Statement> finallyStatements= ASTNodes.asList(visited.getFinally());

					if (!finallyStatements.isEmpty()) {
						return maybeInlineBlock(visited, visited.getFinally());
					}

					remove(visited);
					result= false;
					return false;
				}
			}

			return true;
		}

		@Override
		public boolean visit(final IfStatement visited) {
			if (result && ASTNodes.isPassiveWithoutFallingThrough(visited.getExpression())) {
				Object constantCondition= ASTNodes.peremptoryValue(visited.getExpression());

				if (Boolean.TRUE.equals(constantCondition)) {
					return maybeInlineBlock(visited, visited.getThenStatement());
				}

				if (Boolean.FALSE.equals(constantCondition)) {
					if (visited.getElseStatement() != null) {
						return maybeInlineBlock(visited, visited.getElseStatement());
					}

					remove(visited);
					result= false;
					return false;
				}
			}

			return true;
		}

		private boolean maybeInlineBlock(final Statement visited, final Statement unconditionnalStatement) {
			if (ASTNodes.fallsThrough(unconditionnalStatement)) {
				replaceBlockByPlainCode(visited, unconditionnalStatement);
				removeForwardCode(visited, unconditionnalStatement);
				result= false;
				return false;
			}

			Set<SimpleName> ifVariableNames= ASTNodes.getLocalVariableIdentifiers(unconditionnalStatement, false);
			Set<SimpleName> followingVariableNames= new HashSet<>();

			for (Statement statement : ASTNodes.getNextSiblings(visited)) {
				followingVariableNames.addAll(ASTNodes.getLocalVariableIdentifiers(statement, true));
			}

			for (SimpleName ifVariableName : ifVariableNames) {
				for (SimpleName followingVariableName : followingVariableNames) {
					if (Utils.equalNotNull(ifVariableName.getIdentifier(), followingVariableName.getIdentifier())) {
						return true;
					}
				}
			}

			replaceBlockByPlainCode(visited, unconditionnalStatement);
			result= false;
			return false;
		}
	}

	private void replaceBlockByPlainCode(final Statement sourceNode, final Statement unconditionnalStatement) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.InlineCodeRatherThanPeremptoryConditionCleanUp_description);

		if (unconditionnalStatement instanceof Block && ASTNodes.canHaveSiblings(sourceNode)) {
			ASTNodes.replaceButKeepComment(rewrite, sourceNode, ast.copyRange((List<Statement>) ((Block) unconditionnalStatement).statements()), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, sourceNode, ASTNodes.createMoveTarget(rewrite, unconditionnalStatement), group);
		}
	}

	private void removeForwardCode(final Statement statement, final Statement unconditionnalStatement) {
		if (ASTNodes.canHaveSiblings(statement)) {
			TextEditGroup group= new TextEditGroup(MultiFixMessages.InlineCodeRatherThanPeremptoryConditionCleanUp_description);
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			rewrite.remove(ASTNodes.getNextSiblings(statement), group);
			removeForwardCode((Block) statement.getParent(), unconditionnalStatement);
		} else if (statement.getParent() instanceof TryStatement) {
			removeForwardCode((TryStatement) statement.getParent(), unconditionnalStatement);
		}
	}

	private void remove(final Statement visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.InlineCodeRatherThanPeremptoryConditionCleanUp_description);

		if (ASTNodes.canHaveSiblings(visited) || visited.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
			rewrite.remove(visited, group);
		} else {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			ASTNodes.replaceButKeepComment(rewrite, visited, ast.newBlock(), group);
		}
	}
}
