/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017-2019 Fabrice Tiercelin - Initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_OneCodeThatFallsThroughRatherThanRedundantBlocksCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		CatchesAndFollowingCodeVisitor catchesAndFollowingCodeVisitor= new CatchesAndFollowingCodeVisitor(cuRewrite,
				node);
		node.accept(catchesAndFollowingCodeVisitor);
		return catchesAndFollowingCodeVisitor.getResult();
	}

	private static final class CatchesAndFollowingCodeVisitor extends BlockSubVisitor {
		public CatchesAndFollowingCodeVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
			super(cuRewrite, startNode);
		}

		@Override
		public boolean visit(final TryStatement node) {
			return visitStatement(node);
		}

		@Override
		public boolean visit(final IfStatement node) {
			return visitStatement(node);
		}

		private boolean visitStatement(final Statement node) {
			if (getResult()) {
				List<Statement> redundantStatements= new ArrayList<>();
				collectStatements(node, redundantStatements);
				return maybeRemoveRedundantCode(node, redundantStatements);
			}

			return true;
		}

		@SuppressWarnings("unchecked")
		private void collectStatements(final Statement node, final List<Statement> redundantStatements) {
			if (node == null) {
				return;
			}

			TryStatement ts= ASTNodes.as(node, TryStatement.class);
			IfStatement is= ASTNodes.as(node, IfStatement.class);

			if (ts != null && ts.getFinally() == null) {
				for (CatchClause catchClause : (List<CatchClause>) ts.catchClauses()) {
					doCollectStatements(catchClause.getBody(), redundantStatements);
				}
			} else if (is != null) {
				doCollectStatements(is.getThenStatement(), redundantStatements);
				doCollectStatements(is.getElseStatement(), redundantStatements);
			}
		}

		private void doCollectStatements(Statement node, final List<Statement> redundantStatements) {
			if (node == null) {
				return;
			}

			redundantStatements.add(node);
			List<Statement> statements= ASTNodes.asList(node);

			if (Utils.isEmpty(statements)) {
				return;
			}

			node= statements.get(statements.size() - 1);
			collectStatements(node, redundantStatements);
		}

		private boolean maybeRemoveRedundantCode(final Statement node, final List<Statement> redundantStatements) {
			if (redundantStatements.isEmpty()) {
				return true;
			}

			List<Statement> referenceStatements= new ArrayList<>();

			Statement nextSibling= ASTNodes.getNextSibling(node);
			while (nextSibling != null && !ASTNodes.fallsThrough(nextSibling)) {
				referenceStatements.add(nextSibling);
				nextSibling= ASTNodes.getNextSibling(nextSibling);
			}

			if (nextSibling != null) {
				referenceStatements.add(nextSibling);
				ASTNodeFactory ast= cuRewrite.getASTBuilder();

				for (Statement redundantStatement : redundantStatements) {
					List<Statement> stmtsToCompare= ASTNodes.asList(redundantStatement);

					if (stmtsToCompare.size() > referenceStatements.size()) {
						stmtsToCompare= stmtsToCompare.subList(stmtsToCompare.size() - referenceStatements.size(),
								stmtsToCompare.size());
					}

					if (ASTNodes.match(referenceStatements, stmtsToCompare)) {
						ASTRewrite rewrite= cuRewrite.getASTRewrite();

						if (redundantStatement instanceof Block) {
							rewrite.remove(stmtsToCompare, null);
						} else {
							rewrite.replace(redundantStatement, ast.block(), null);
						}

						setResult(false);
						return false;
					}
				}
			}

			return true;
		}
	}
}
