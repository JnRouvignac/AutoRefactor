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
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class TryWithResourceCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.TryWithResourceCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.TryWithResourceCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.TryWithResourceCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public boolean visit(final Block node) {
		DeclarationAndTryVisitor returnStatementVisitor= new DeclarationAndTryVisitor();
		returnStatementVisitor.visitNode(node);
		return returnStatementVisitor.result;
	}

	private final class DeclarationAndTryVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final TryStatement node) {
			if (result) {
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
						MethodInvocation methodInvocation= ASTNodes.as(finallyEs.getExpression(), MethodInvocation.class);

						if (methodClosesCloseables(methodInvocation) && ASTNodes.areSameVariables(previousDeclFragment, methodInvocation.getExpression())) {
							return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
									nodesToRemove);
						}
					} else if (finallyIs != null && ASTNodes.asList(finallyIs.getThenStatement()).size() == 1
							&& ASTNodes.asList(finallyIs.getElseStatement()).isEmpty()) {
						Expression nullCheckedExpression= ASTNodes.getNullCheckedExpression(finallyIs.getExpression());

						Statement thenStatement= ASTNodes.asList(finallyIs.getThenStatement()).get(0);
						MethodInvocation methodInvocation= ASTNodes.asExpression(thenStatement, MethodInvocation.class);

						if (methodClosesCloseables(methodInvocation)
								&& ASTNodes.areSameVariables(previousDeclFragment, nullCheckedExpression, methodInvocation.getExpression())) {
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

			TextEditGroup group= new TextEditGroup(MultiFixMessages.TryWithResourceCleanUp_description);
			rewrite.insertFirst(node, TryStatement.RESOURCES_PROPERTY, newResource, group);
			rewrite.remove(nodesToRemove, group);
			this.result= false;
			return false;
		}

		private boolean methodClosesCloseables(final MethodInvocation methodInvocation) {
			return ASTNodes.usesGivenSignature(methodInvocation, Closeable.class.getCanonicalName(), "close"); //$NON-NLS-1$
		}

		private VariableDeclarationExpression newResource(final List<Statement> tryStatements,
				final VariableDeclarationStatement previousDeclStatement, final VariableDeclarationFragment previousDeclFragment,
				final List<ASTNode> nodesToRemove) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.TryWithResourceCleanUp_description);

			VariableDeclarationFragment fragment= newFragment(tryStatements, previousDeclFragment, nodesToRemove);
			return fragment != null ? ast.declareExpression(ASTNodes.createMoveTarget(rewrite, previousDeclStatement.getType()), fragment) : null;
		}

		private VariableDeclarationFragment newFragment(final List<Statement> tryStatements,
				final VariableDeclarationFragment existingFragment, final List<ASTNode> nodesToRemove) {
			VarDefinitionsUsesVisitor visitor= new VarDefinitionsUsesVisitor(existingFragment).find();
			List<SimpleName> definitions= visitor.getWrites();

			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.TryWithResourceCleanUp_description);

			if (!tryStatements.isEmpty()) {
				Statement tryStatement= tryStatements.get(0);
				Assignment assignResource= ASTNodes.asExpression(tryStatement, Assignment.class);

				if (assignResource != null && ASTNodes.isSameVariable(existingFragment, assignResource.getLeftHandSide())) {
					nodesToRemove.add(tryStatement);

					if (containsOnly(definitions, assignResource.getLeftHandSide(), existingFragment.getName())) {
						return ast.declareFragment(ASTNodes.createMoveTarget(rewrite, existingFragment.getName()),
								ASTNodes.createMoveTarget(rewrite, assignResource.getRightHandSide()));
					}

					return null;
				}
			}

			return containsOnly(definitions, existingFragment.getName()) ? ASTNodes.createMoveTarget(rewrite, existingFragment) : null;
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

		@SuppressWarnings({ "deprecation", "unchecked" })
		private boolean collapseTryStatements(final TryStatement outerTryStatement, final TryStatement innerTryStatement) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.TryWithResourceCleanUp_description);

			rewrite.insertLast(outerTryStatement, TryStatement.RESOURCES_PROPERTY, ast.copyRange((List<VariableDeclarationExpression>) innerTryStatement.resources()), group);
			rewrite.replace(innerTryStatement, ASTNodes.createMoveTarget(rewrite, innerTryStatement.getBody()), group);
			this.result= false;
			return false;
		}
	}
}
