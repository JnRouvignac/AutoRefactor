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
import java.util.Arrays;
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
public class ObsoleteTryWithResourceCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteTryWithResourceCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteTryWithResourceCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteTryWithResourceCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 7;
	}

	@Override
	public boolean visit(final Block node) {
		DeclarationAndTryVisitor declarationAndTryVisitor= new DeclarationAndTryVisitor();
		declarationAndTryVisitor.visitNode(node);
		return declarationAndTryVisitor.result;
	}

	private final class DeclarationAndTryVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final TryStatement node) {
			if (result) {
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

					MethodInvocation methodInvocation= ASTNodes.asExpression(finallyStatement, MethodInvocation.class);
					IfStatement finallyIs= ASTNodes.as(finallyStatement, IfStatement.class);
					List<Statement> tryStatements= ASTNodes.asList(node.getBody());

					if (methodInvocation != null) {
						if (methodClosesCloseables(methodInvocation) && ASTNodes.areSameVariables(previousDeclFragment, methodInvocation.getExpression())) {
							return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
									nodesToRemove);
						}
					} else if (finallyIs != null && ASTNodes.asList(finallyIs.getThenStatement()).size() == 1
							&& ASTNodes.asList(finallyIs.getElseStatement()).isEmpty()) {
						Expression nullCheckedExpression= ASTNodes.getNullCheckedExpression(finallyIs.getExpression());

						Statement thenStatement= ASTNodes.asList(finallyIs.getThenStatement()).get(0);
						MethodInvocation methodInvocation2= ASTNodes.asExpression(thenStatement, MethodInvocation.class);

						if (methodClosesCloseables(methodInvocation2)
								&& ASTNodes.areSameVariables(previousDeclFragment, nullCheckedExpression, methodInvocation2.getExpression())) {
							return maybeRefactorToTryWithResources(node, tryStatements, previousDeclStatement, previousDeclFragment,
									nodesToRemove);
						}
					}
				}
			}

			return true;
		}

		private boolean maybeRefactorToTryWithResources(final TryStatement node, final List<Statement> tryStatements,
				final VariableDeclarationStatement previousDeclStatement,
				final VariableDeclarationFragment previousDeclFragment, final List<ASTNode> nodesToRemove) {
			VarDefinitionsUsesVisitor visitor= new VarDefinitionsUsesVisitor(previousDeclFragment);
			List<SimpleName> definitions= visitor.getWrites();

			boolean hasAssignment= false;
			Statement tryStatement= null;
			Assignment assignResource= null;

			if (!tryStatements.isEmpty()) {
				tryStatement= tryStatements.get(0);
				assignResource= ASTNodes.asExpression(tryStatement, Assignment.class);

				hasAssignment= assignResource != null && ASTNodes.isSameVariable(previousDeclFragment, assignResource.getLeftHandSide());
			}

			if (hasAssignment) {
				if (containsOnly(definitions, assignResource.getLeftHandSide(), previousDeclFragment.getName())) {
					refactorFromAssignment(node, previousDeclStatement, previousDeclFragment, nodesToRemove,
							tryStatement, assignResource);

					result= false;
					return false;
				}
			} else if (containsOnly(definitions, previousDeclFragment.getName())) {
				refactorFromDeclaration(node, previousDeclStatement, previousDeclFragment, nodesToRemove);

				result= false;
				return false;
			}

			return true;
		}

		private boolean containsOnly(final Collection<SimpleName> definitions, final Expression... simpleNames) {
			return definitions.size() == simpleNames.length && definitions.containsAll(Arrays.asList(simpleNames));
		}

		private boolean methodClosesCloseables(final MethodInvocation methodInvocation) {
			return ASTNodes.usesGivenSignature(methodInvocation, Closeable.class.getCanonicalName(), "close"); //$NON-NLS-1$
		}

		private void refactorFromDeclaration(final TryStatement node,
				final VariableDeclarationStatement previousDeclStatement,
				final VariableDeclarationFragment previousDeclFragment, final List<ASTNode> nodesToRemove) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			VariableDeclarationFragment newFragment= ASTNodes.createMoveTarget(rewrite, previousDeclFragment);
			refactorToTryWithResources(node, previousDeclStatement, nodesToRemove, newFragment, rewrite, ast);
		}

		private void refactorFromAssignment(final TryStatement node,
				final VariableDeclarationStatement previousDeclStatement,
				final VariableDeclarationFragment previousDeclFragment, final List<ASTNode> nodesToRemove,
				final Statement tryStatement, final Assignment assignResource) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			nodesToRemove.add(tryStatement);

			VariableDeclarationFragment newFragment= ast.newVariableDeclarationFragment(ASTNodes.createMoveTarget(rewrite, previousDeclFragment.getName()),
					ASTNodes.createMoveTarget(rewrite, assignResource.getRightHandSide()));
			refactorToTryWithResources(node, previousDeclStatement, nodesToRemove, newFragment, rewrite, ast);
		}

		@SuppressWarnings("deprecation")
		private void refactorToTryWithResources(final TryStatement node,
				final VariableDeclarationStatement previousDeclStatement, final List<ASTNode> nodesToRemove,
				final VariableDeclarationFragment newFragment, final ASTRewrite rewrite, final ASTNodeFactory ast) {
			VariableDeclarationExpression newResource= ast.newVariableDeclarationExpression(ASTNodes.createMoveTarget(rewrite, previousDeclStatement.getType()), newFragment);

			TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteTryWithResourceCleanUp_description);
			rewrite.insertFirst(node, TryStatement.RESOURCES_PROPERTY, newResource, group);

			for (ASTNode nodeToRemove : nodesToRemove) {
				ASTNodes.removeButKeepComment(rewrite, nodeToRemove, group);
			}
		}
	}
}
