/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class EndOfLoopRatherThanContinueCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.EndOfLoopRatherThanContinueCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.EndOfLoopRatherThanContinueCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.EndOfLoopRatherThanContinueCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		ContinueInBlockVisitor continueInBlockVisitor= new ContinueInBlockVisitor();
		continueInBlockVisitor.visitNode(node);
		return continueInBlockVisitor.result;
	}

	private final class ContinueInBlockVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final ContinueStatement node) {
			if (result
					&& node.getLabel() == null
					&& isLastStatementInLoop(node)) {
				ASTRewrite rewrite= cuRewrite.getASTRewrite();
				TextEditGroup group= new TextEditGroup(MultiFixMessages.EndOfLoopRatherThanContinueCleanUp_description);

				if (startNode.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY
						&& ASTNodes.asList(startNode).size() == 1
						&& Utils.equalNotNull(ASTNodes.asList(startNode).get(0), node)) {
					rewrite.remove(startNode, group);
				} else if (ASTNodes.canHaveSiblings(node) || node.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
					rewrite.remove(node, group);
				} else {
					ASTNodeFactory ast= cuRewrite.getASTBuilder();

					ASTNodes.replaceButKeepComment(rewrite, node, ast.newBlock(), group);
				}

				result= false;
				return false;
			}

			return true;
		}

		private boolean isLastStatementInLoop(final Statement node) {
			Statement nextStatement= ASTNodes.getNextStatement(node);

			if (nextStatement == null) {
				if (node.getParent() instanceof WhileStatement || node.getParent() instanceof EnhancedForStatement
						|| node.getParent() instanceof ForStatement || node.getParent() instanceof DoStatement) {
					return true;
				}

				if (node.getParent() instanceof MethodDeclaration) {
					return false;
				}

				if (node.getParent() instanceof Statement) {
					return isLastStatementInLoop((Statement) node.getParent());
				}
			}

			return false;
		}
	}
}
