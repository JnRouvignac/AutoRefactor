/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Jean-Noël Rouvignac - initial API and implementation
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

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class TryWithResourceCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_TryWithResourceCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_TryWithResourceCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_TryWithResourceCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public boolean visit(final Block node) {
		DeclarationAndTryVisitor returnStatementVisitor= new DeclarationAndTryVisitor(cuRewrite, node);
		node.accept(returnStatementVisitor);
		return returnStatementVisitor.getResult();
	}

	private static final class DeclarationAndTryVisitor extends BlockSubVisitor {
		public DeclarationAndTryVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
			super(cuRewrite, startNode);
		}

		@Override
		public boolean visit(final TryStatement node) {
			if (getResult()) {
				List<Statement> tryStatements= ASTNodes.asList(node.getBody());

				if (!tryStatements.isEmpty() && tryStatements.get(0).getNodeType() == ASTNode.TRY_STATEMENT) {
					TryStatement innerTryStatement= ASTNodes.as(tryStatements.get(0), TryStatement.class);

					if (innerTryStatement != null && !innerTryStatement.resources().isEmpty() && innerTryStatement.catchClauses().isEmpty()) {
						return collapseTryStatements(node, innerTryStatement);
					}
				}

				VariableDeclarationStatement previousDeclStatement= ASTNodes.as(ASTNodes.getPreviousStatement(node),
						VariableDeclarationStatement.class);

				if (previousDeclStatement == null) {
					return true;
				}

				VariableDeclarationFragment previousDeclFragment= ASTNodes.getUniqueFragment(previousDeclStatement);
				List<Statement> finallyStatements= ASTNodes.asList(node.getFinally());

				if (previousDeclFragment != null && !finallyStatements.isEmpty()) {
					List<ASTNode> nodesToRemove= new ArrayList<>();
					nodesToRemove.add(previousDeclStatement);

					Statement finallyStatement= finallyStatements.get(0);
					nodesToRemove.add(finallyStatements.size() == 1 ? node.getFinally() : finallyStatement);

					ExpressionStatement finallyEs= ASTNodes.as(finallyStatement, ExpressionStatement.class);
					IfStatement finallyIs= ASTNodes.as(finallyStatement, IfStatement.class);

					if (finallyEs != null) {
						MethodInvocation mi= ASTNodes.as(finallyEs.getExpression(), MethodInvocation.class);

						if (methodClosesCloseables(mi) && ASTNodes.areSameVariables(previousDeclFragment, mi.getExpression())) {
							return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
									nodesToRemove);
						}
					} else if (finallyIs != null && ASTNodes.asList(finallyIs.getThenStatement()).size() == 1
							&& ASTNodes.asList(finallyIs.getElseStatement()).isEmpty()) {
						Expression nullCheckedExpression= ASTNodes.getNullCheckedExpression(finallyIs.getExpression());

						Statement thenStatement= ASTNodes.asList(finallyIs.getThenStatement()).get(0);
						MethodInvocation mi= ASTNodes.asExpression(thenStatement, MethodInvocation.class);

						if (methodClosesCloseables(mi)
								&& ASTNodes.areSameVariables(previousDeclFragment, nullCheckedExpression, mi.getExpression())) {
							return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
									nodesToRemove);
						}
					}
				}
			}

			return true;
		}

		@SuppressWarnings("deprecation")
		private boolean maybeRefactorToTryWithResources(final TryStatement node, final List<Statement> tryStatements,
				final VariableDeclarationStatement previousDeclStatement,
				final VariableDeclarationFragment previousDeclFragment, final List<ASTNode> nodesToRemove) {
			VariableDeclarationExpression newResource= newResource(tryStatements, previousDeclStatement,
					previousDeclFragment, nodesToRemove);

			if (newResource == null) {
				return true;
			}

			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			rewrite.insertFirst(node, TryStatement.RESOURCES_PROPERTY, newResource, null);
			rewrite.remove(nodesToRemove, null);
			setResult(false);
			return false;
		}

		private boolean methodClosesCloseables(final MethodInvocation mi) {
			return ASTNodes.usesGivenSignature(mi, Closeable.class.getCanonicalName(), "close"); //$NON-NLS-1$
		}

		private VariableDeclarationExpression newResource(final List<Statement> tryStatements,
				final VariableDeclarationStatement previousDeclStatement, final VariableDeclarationFragment previousDeclFragment,
				final List<ASTNode> nodesToRemove) {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			VariableDeclarationFragment fragment= newFragment(tryStatements, previousDeclFragment, nodesToRemove);
			return fragment != null ? ast.declareExpression(rewrite.createMoveTarget(previousDeclStatement.getType()), fragment) : null;
		}

		private VariableDeclarationFragment newFragment(final List<Statement> tryStatements,
				final VariableDeclarationFragment existingFragment, final List<ASTNode> nodesToRemove) {
			VarDefinitionsUsesVisitor visitor= new VarDefinitionsUsesVisitor(existingFragment).find();
			List<SimpleName> definitions= visitor.getWrites();

			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			if (!tryStatements.isEmpty()) {
				Statement tryStatement= tryStatements.get(0);
				Assignment assignResource= ASTNodes.asExpression(tryStatement, Assignment.class);

				if (assignResource != null && ASTNodes.isSameVariable(existingFragment, assignResource.getLeftHandSide())) {
					nodesToRemove.add(tryStatement);

					if (containsOnly(definitions, assignResource.getLeftHandSide(), existingFragment.getName())) {
						return ast.declareFragment(rewrite.createMoveTarget(existingFragment.getName()),
								rewrite.createMoveTarget(assignResource.getRightHandSide()));
					}

					return null;
				}
			}

			return containsOnly(definitions, existingFragment.getName()) ? rewrite.createMoveTarget(existingFragment) : null;
		}

		private boolean containsOnly(final Collection<SimpleName> definitions, final Expression... simpleNames) {
			if (definitions.size() != simpleNames.length) {
				return false;
			}

			for (Expression simpleName : simpleNames) {
				if (!definitions.contains(simpleName)) {
					return false;
				}
			}

			return true;
		}

		@SuppressWarnings("deprecation")
		private boolean collapseTryStatements(final TryStatement outerTryStatement, final TryStatement innerTryStatement) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			rewrite.insertLast(outerTryStatement, TryStatement.RESOURCES_PROPERTY, ast.copyRange(ASTNodes.resources(innerTryStatement)), null);
			rewrite.replace(innerTryStatement, rewrite.createMoveTarget(innerTryStatement.getBody()), null);
			setResult(false);
			return false;
		}
	}
}
