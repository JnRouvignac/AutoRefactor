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

import java.util.Collection;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class VariableInsideIfRatherThanAboveCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.VariableInsideIfRatherThanAboveCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.VariableInsideIfRatherThanAboveCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.VariableInsideIfRatherThanAboveCleanUp_reason;
	}

	@Override
	public boolean visit(final Block visited) {
		VariableAndIfVisitor newAndPutAllMethodVisitor= new VariableAndIfVisitor();
		newAndPutAllMethodVisitor.visitNode(visited);
		return newAndPutAllMethodVisitor.result;
	}

	private final class VariableAndIfVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final IfStatement visited) {
			if (result) {
				Statement variableAssignment= ASTNodes.getPreviousSibling(visited);
				VariableDeclarationFragment variable= getVariable(variableAssignment);

				if (variable == null || isVarUsed(variable, visited.getExpression())) {
					return true;
				}

				for (Statement statement : ASTNodes.getNextSiblings(visited)) {
					if (isVarUsed(variable, statement)) {
						return true;
					}
				}

				if (isVarUsed(variable, visited.getThenStatement())) {
					if (visited.getElseStatement() != null && isVarUsed(variable, visited.getElseStatement())) {
						return true;
					}

					return maybeMoveAssignment(variableAssignment, visited.getThenStatement());
				}

				if (visited.getElseStatement() != null) {
					return !isVarUsed(variable, visited.getElseStatement()) || maybeMoveAssignment(variableAssignment, visited.getElseStatement());
				}
			}

			return true;
		}

		private boolean isVarUsed(final VariableDeclarationFragment variable, final ASTNode astNode) {
			VarDefinitionsUsesVisitor varOccurrenceVisitor= new VarDefinitionsUsesVisitor(variable.resolveBinding(), astNode, true);
			return !varOccurrenceVisitor.getWrites().isEmpty() || !varOccurrenceVisitor.getReads().isEmpty();
		}

		private VariableDeclarationFragment getVariable(final Statement variableAssignment) {
			VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(variableAssignment);

			if (fragment != null && (fragment.getInitializer() == null || ASTNodes.isPassiveWithoutFallingThrough(fragment.getInitializer()))) {
				return fragment;
			}

			return null;
		}

		private boolean maybeMoveAssignment(final Statement variableAssignment, final Statement statement) {
			List<Statement> statements= ASTNodes.asList(statement);

			if (statements.isEmpty()) {
				return true;
			}

			moveAssignmentInsideIf(variableAssignment, statement, statements);
			result= false;
			return false;
		}

		private void moveAssignmentInsideIf(final Statement variableAssignment, final Statement statement,
				final List<Statement> statements) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.VariableInsideIfRatherThanAboveCleanUp_description);

			if (statement instanceof Block) {
				rewrite.insertBefore(ASTNodes.createMoveTarget(rewrite, variableAssignment), statements.get(0), group);
				rewrite.remove(variableAssignment, group);
			} else {
				List<Statement> copyOfThenStatements= ASTNodes.createMoveTarget(rewrite, statements);
				copyOfThenStatements.add(0, ASTNodes.createMoveTarget(rewrite, variableAssignment));
				Block newBlock= ast.newBlock();
				newBlock.statements().addAll(copyOfThenStatements);
				ASTNodes.replaceButKeepComment(rewrite, statement, newBlock, group);
			}
		}
	}
}
