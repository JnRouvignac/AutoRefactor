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
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class OutsideCodeRatherThanFallingThroughBlocksCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_OutsideCodeRatherThanFallingThroughBlocksCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_OutsideCodeRatherThanFallingThroughBlocksCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_OutsideCodeRatherThanFallingThroughBlocksCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		BlocksAndFollowingCodeVisitor blocksAndFollowingCodeVisitor= new BlocksAndFollowingCodeVisitor();
		blocksAndFollowingCodeVisitor.visitNode(node);
		return blocksAndFollowingCodeVisitor.result;
	}

	private final class BlocksAndFollowingCodeVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final TryStatement node) {
			return visitStatement(node);
		}

		@Override
		public boolean visit(final IfStatement node) {
			return visitStatement(node);
		}

		private boolean visitStatement(final Statement node) {
			if (result) {
				List<Statement> redundantStatements= new ArrayList<>();
				collectStatements(node, redundantStatements);
				return maybeRemoveRedundantCode(node, redundantStatements);
			}

			return true;
		}

		private void collectStatements(final Statement node, final List<Statement> redundantStatements) {
			if (node != null) {
				TryStatement ts= ASTNodes.as(node, TryStatement.class);
				IfStatement is= ASTNodes.as(node, IfStatement.class);

				if (ts != null && ts.getFinally() == null) {
					@SuppressWarnings("unchecked")
					List<CatchClause> catchClauses= ts.catchClauses();

					for (CatchClause catchClause : catchClauses) {
						doCollectStatements(catchClause.getBody(), redundantStatements);
					}
				} else if (is != null) {
					doCollectStatements(is.getThenStatement(), redundantStatements);
					doCollectStatements(is.getElseStatement(), redundantStatements);
				}
			}
		}

		private void doCollectStatements(final Statement node, final List<Statement> redundantStatements) {
			if (node != null) {
				redundantStatements.add(node);
				List<Statement> statements= ASTNodes.asList(node);

				if (!Utils.isEmpty(statements)) {
					collectStatements(statements.get(statements.size() - 1), redundantStatements);
				}
			}
		}

		private boolean maybeRemoveRedundantCode(final Statement node, final List<Statement> redundantStatements) {
			List<Statement> referenceStatements= ASTNodes.getNextSiblings(node);

			if (redundantStatements.isEmpty() || Utils.isEmpty(referenceStatements)) {
				return true;
			}

			for (Statement redundantStatement : redundantStatements) {
				List<Statement> statements= ASTNodes.asList(redundantStatement);

				if (Utils.isEmpty(statements) || !ASTNodes.fallsThrough(statements.get(statements.size() - 1))) {
					continue;
				}

				Statement lastStatement= null;

				List<Statement> stmtsToCompare;
				if (statements.size() > referenceStatements.size()) {
					stmtsToCompare= statements.subList(statements.size() - referenceStatements.size(),
							statements.size());
				} else {
					stmtsToCompare= new ArrayList<>(statements);
				}

				ASTSemanticMatcher matcher= new ASTSemanticMatcher() {
					@Override
					public boolean match(final SimpleName node, final Object other) {
						return other instanceof SimpleName
								&& node.resolveBinding() != null
								&& ((SimpleName) other).resolveBinding() != null
								&& ((node.resolveBinding().getKind() != IBinding.VARIABLE && ((SimpleName) other).resolveBinding().getKind() != IBinding.VARIABLE) || ASTNodes.isSameVariable(node, (SimpleName) other))
								&& super.match(node, other);
					}
				};
				boolean match= ASTNodes.match(matcher, referenceStatements, stmtsToCompare);

				if (!match) {
					lastStatement= statements.get(statements.size() - 1);
					ReturnStatement returnStatement= ASTNodes.as(lastStatement, ReturnStatement.class);
					ContinueStatement continueStatement= ASTNodes.as(lastStatement, ContinueStatement.class);

					if ((isIn(node, MethodDeclaration.class)
							&& returnStatement != null
							&& returnStatement.getExpression() == null)
							|| (isIn(node, EnhancedForStatement.class, ForStatement.class, WhileStatement.class, DoStatement.class)
									&& continueStatement != null
									&& continueStatement.getLabel() == null)) {
						if (statements.size() > referenceStatements.size() + 1) {
							stmtsToCompare= statements.subList(statements.size() - referenceStatements.size() - 1,
									statements.size() - 1);
						} else {
							stmtsToCompare= statements.subList(0, statements.size() - 1);
						}

						match= ASTNodes.match(matcher, referenceStatements, stmtsToCompare);
					}
				}

				if (match) {
					ASTRewrite rewrite= cuRewrite.getASTRewrite();
					ASTNodeFactory ast= cuRewrite.getASTBuilder();
					TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_OutsideCodeRatherThanFallingThroughBlocksCleanUp_name);

					if (redundantStatement instanceof Block) {
						rewrite.remove(stmtsToCompare, group);

						if (lastStatement != null) {
							rewrite.remove(lastStatement, group);
						}
					} else {
						rewrite.replace(redundantStatement, ast.block(), group);
					}

					this.result= false;
				}
			}

			return result;
		}

		private boolean isIn(final Statement node, final Class<?>... domClasses) {
			for (Class<?> domClass : domClasses) {
				if (node.getParent().getClass().isAssignableFrom(domClass)
						|| (node.getParent() instanceof Block && node.getParent().getParent().getClass().isAssignableFrom(domClass))) {
					return true;
				}
			}

			return false;
		}
	}
}
