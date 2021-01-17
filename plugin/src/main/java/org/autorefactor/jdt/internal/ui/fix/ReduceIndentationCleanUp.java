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
		public boolean visit(final IfStatement visited) {
			computeGreatestIndentation(visited.getThenStatement());

			if (visited.getElseStatement() != null) {
				computeGreatestIndentation(visited.getElseStatement());
			}

			return false;
		}

		@Override
		public boolean visit(final WhileStatement visited) {
			computeGreatestIndentation(visited.getBody());
			return false;
		}

		@Override
		public boolean visit(final DoStatement visited) {
			computeGreatestIndentation(visited.getBody());
			return false;
		}

		@Override
		public boolean visit(final ForStatement visited) {
			computeGreatestIndentation(visited.getBody());
			return false;
		}

		@Override
		public boolean visit(final EnhancedForStatement visited) {
			computeGreatestIndentation(visited.getBody());
			return false;
		}

		@Override
		public boolean visit(final TryStatement visited) {
			computeGreatestIndentation(visited.getBody());

			for (Object object : visited.catchClauses()) {
				CatchClause clause= (CatchClause) object;
				computeGreatestIndentation(clause.getBody());
			}

			if (visited.getFinally() != null) {
				computeGreatestIndentation(visited.getFinally());
			}

			if (visited.getFinally() != null) {
				computeGreatestIndentation(visited.getFinally());
			}

			return false;
		}

		@Override
		public boolean visit(final Block visited) {
			computeGreatestIndentation(visited);
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
	public boolean visit(final IfStatement visited) {
		if (visited.getElseStatement() != null && !ASTNodes.isInElse(visited)) {
			if (ASTNodes.fallsThrough(visited.getThenStatement())) {
				if (ASTNodes.fallsThrough(visited.getElseStatement())) {
					if (ASTNodes.getNextSiblings(visited).isEmpty()) {
						int thenIndentation= getIndentation(visited.getThenStatement());
						int elseIndentation= getIndentation(visited.getElseStatement());

						if (thenIndentation <= elseIndentation || visited.getElseStatement() instanceof IfStatement) {
							moveElseStatement(visited);
						} else {
							moveThenStatement(visited);
						}

						return false;
					}
				} else if (!ASTNodes.hasVariableConflict(visited, visited.getElseStatement())) {
					moveElseStatement(visited);
					return false;
				}
			} else if (ASTNodes.fallsThrough(visited.getElseStatement())
					&& !ASTNodes.hasVariableConflict(visited, visited.getThenStatement())
					&& !(visited.getElseStatement() instanceof IfStatement)) {
				moveThenStatement(visited);
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

	private void moveThenStatement(final IfStatement visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ReduceIndentationCleanUp_description);

		List<Statement> statementsToMove= ASTNodes.asList(visited.getThenStatement());

		if (ASTNodes.canHaveSiblings(visited)) {
			for (int i= statementsToMove.size() - 1; i >= 0; i--) {
				rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, statementsToMove.get(i)), visited, group);
			}

			ASTNodes.replaceButKeepComment(rewrite, visited.getExpression(), ast.negate(visited.getExpression(), true), group);
			ASTNodes.replaceButKeepComment(rewrite, visited.getThenStatement(), ASTNodes.createMoveTarget(rewrite, visited.getElseStatement()), group);
			rewrite.remove(visited.getElseStatement(), group);
		} else {
			List<Statement> copyOfStatements= new ArrayList<>(statementsToMove.size() + 1);

			for (Statement statement : statementsToMove) {
				copyOfStatements.add(ASTNodes.createMoveTarget(rewrite, statement));
			}

			ASTNodes.replaceButKeepComment(rewrite, visited.getExpression(), ast.negate(visited.getExpression(), true), group);
			ASTNodes.replaceButKeepComment(rewrite, visited.getThenStatement(), ASTNodes.createMoveTarget(rewrite, visited.getElseStatement()), group);
			copyOfStatements.add(0, ASTNodes.createMoveTarget(rewrite, visited));
			Block newBlock= ast.newBlock();
			newBlock.statements().addAll(copyOfStatements);

			ASTNodes.replaceButKeepComment(rewrite, visited, newBlock, group);
		}
	}

	private void moveElseStatement(final IfStatement visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ReduceIndentationCleanUp_description);

		List<Statement> statementsToMove= ASTNodes.asList(visited.getElseStatement());

		if (ASTNodes.canHaveSiblings(visited)) {
			for (int i= statementsToMove.size() - 1; i >= 0; i--) {
				rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, statementsToMove.get(i)), visited, group);
			}

			rewrite.remove(visited.getElseStatement(), group);
		} else {
			List<Statement> copyOfStatements= new ArrayList<>(statementsToMove.size() + 1);

			for (Statement statement : statementsToMove) {
				copyOfStatements.add(ASTNodes.createMoveTarget(rewrite, statement));
			}

			rewrite.remove(visited.getElseStatement(), group);
			copyOfStatements.add(0, ASTNodes.createMoveTarget(rewrite, visited));
			Block newBlock= ast.newBlock();
			newBlock.statements().addAll(copyOfStatements);

			ASTNodes.replaceButKeepComment(rewrite, visited, newBlock, group);
		}
	}
}
