/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ReduceIndentationCleanUp extends AbstractCleanUpRule {
	private static final class IndentationVisitor extends ASTVisitor {
		private int indentation;

		public int getIndentation() {
			return indentation;
		}

		@Override
		public boolean visit(final IfStatement node) {
			computeGreatestIndentation(node.getThenStatement());

			if (node.getElseStatement() != null) {
				computeGreatestIndentation(node.getElseStatement());
			}

			return false;
		}

		@Override
		public boolean visit(final WhileStatement node) {
			computeGreatestIndentation(node.getBody());
			return false;
		}

		@Override
		public boolean visit(final DoStatement node) {
			computeGreatestIndentation(node.getBody());
			return false;
		}

		@Override
		public boolean visit(final ForStatement node) {
			computeGreatestIndentation(node.getBody());
			return false;
		}

		@Override
		public boolean visit(final EnhancedForStatement node) {
			computeGreatestIndentation(node.getBody());
			return false;
		}

		@Override
		public boolean visit(final TryStatement node) {
			computeGreatestIndentation(node.getBody());

			for (Object object : node.catchClauses()) {
				CatchClause clause= (CatchClause) object;
				computeGreatestIndentation(clause.getBody());
			}

			if (node.getFinally() != null) {
				computeGreatestIndentation(node.getFinally());
			}

			if (node.getFinally() != null) {
				computeGreatestIndentation(node.getFinally());
			}

			return false;
		}

		@Override
		public boolean visit(final Block node) {
			computeGreatestIndentation(node);
			return false;
		}

		private void computeGreatestIndentation(final Statement statements) {
			for (Statement statement : ASTNodes.asList(statements)) {
				IndentationVisitor visitor= new IndentationVisitor();

				statement.accept(visitor);

				indentation= Math.max(indentation, visitor.getIndentation() + 1);
			}
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.ReduceIndentationCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ReduceIndentationCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ReduceIndentationCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement node) {
		if (node.getElseStatement() != null && !ASTNodes.isInElse(node)) {
			if (ASTNodes.fallsThrough(node.getThenStatement())) {
				if (ASTNodes.fallsThrough(node.getElseStatement())) {
					if (ASTNodes.getNextSiblings(node).isEmpty()) {
						int thenIndentation= getIndentation(node.getThenStatement());
						int elseIndentation= getIndentation(node.getElseStatement());

						if (thenIndentation <= elseIndentation || node.getElseStatement() instanceof IfStatement) {
							moveElseStatement(node);
						} else {
							moveThenStatement(node);
						}

						return false;
					}
				} else if (!ASTNodes.hasVariableConflict(node, node.getElseStatement())) {
					moveElseStatement(node);
					return false;
				}
			} else if (ASTNodes.fallsThrough(node.getElseStatement()) && !ASTNodes.hasVariableConflict(node, node.getThenStatement()) && !(node.getElseStatement() instanceof IfStatement)) {
				moveThenStatement(node);
				return false;
			}
		}

		return true;
	}

	private int getIndentation(final Statement statementInIf) {
		IndentationVisitor visitor= new IndentationVisitor();
		statementInIf.accept(visitor);
		return visitor.getIndentation() + (statementInIf instanceof Block ? -1 : 0);
	}

	private void moveThenStatement(final IfStatement node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ReduceIndentationCleanUp_description);

		List<Statement> statementsToMove= ASTNodes.asList(node.getThenStatement());

		if (ASTNodes.canHaveSiblings(node)) {
			for (int i= statementsToMove.size() - 1; i >= 0; i--) {
				rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, statementsToMove.get(i)), node, group);
			}

			ASTNodes.replaceButKeepComment(rewrite, node.getExpression(), ast.negate(node.getExpression(), true), group);
			ASTNodes.replaceButKeepComment(rewrite, node.getThenStatement(), ASTNodes.createMoveTarget(rewrite, node.getElseStatement()), group);
			rewrite.remove(node.getElseStatement(), group);
		} else {
			List<Statement> copyOfStatements= new ArrayList<>(statementsToMove.size() + 1);

			for (Statement statement : statementsToMove) {
				copyOfStatements.add(ASTNodes.createMoveTarget(rewrite, statement));
			}

			ASTNodes.replaceButKeepComment(rewrite, node.getExpression(), ast.negate(node.getExpression(), true), group);
			ASTNodes.replaceButKeepComment(rewrite, node.getThenStatement(), ASTNodes.createMoveTarget(rewrite, node.getElseStatement()), group);
			copyOfStatements.add(0, ASTNodes.createMoveTarget(rewrite, node));

			Block block= ast.newBlock(copyOfStatements);
			ASTNodes.replaceButKeepComment(rewrite, node, block, group);
		}
	}

	private void moveElseStatement(final IfStatement node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ReduceIndentationCleanUp_description);

		List<Statement> statementsToMove= ASTNodes.asList(node.getElseStatement());

		if (ASTNodes.canHaveSiblings(node)) {
			for (int i= statementsToMove.size() - 1; i >= 0; i--) {
				rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, statementsToMove.get(i)), node, group);
			}

			rewrite.remove(node.getElseStatement(), group);
		} else {
			List<Statement> copyOfStatements= new ArrayList<>(statementsToMove.size() + 1);

			for (Statement statement : statementsToMove) {
				copyOfStatements.add(ASTNodes.createMoveTarget(rewrite, statement));
			}

			rewrite.remove(node.getElseStatement(), group);
			copyOfStatements.add(0, ASTNodes.createMoveTarget(rewrite, node));

			Block block= ast.newBlock(copyOfStatements);
			ASTNodes.replaceButKeepComment(rewrite, node, block, group);
		}
	}
}
