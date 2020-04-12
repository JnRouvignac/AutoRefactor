/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice TIERCELIN - Avoid side effect
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
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class RemoveUnnecessaryLocalBeforeReturnCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryLocalBeforeReturnCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		ReturnStatementVisitor returnStatementVisitor= new ReturnStatementVisitor(cuRewrite, node);
		node.accept(returnStatementVisitor);
		return returnStatementVisitor.getResult();
	}

	private static final class ReturnStatementVisitor extends BlockSubVisitor {
		public ReturnStatementVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
			super(cuRewrite, startNode);
		}

		@Override
		public boolean visit(final ReturnStatement node) {
			if (getResult()) {
				Statement previousSibling= ASTNodes.getPreviousSibling(node);
				if (!cuRewrite.getASTRewrite().hasBeenRefactored(previousSibling)
						&& previousSibling instanceof VariableDeclarationStatement) {
					VariableDeclarationStatement vds= (VariableDeclarationStatement) previousSibling;
					VariableDeclarationFragment vdf= ASTNodes.getUniqueFragment(vds);

					if (vdf != null && ASTNodes.isSameLocalVariable(node.getExpression(), vdf.getName())) {
						Expression returnExpression= vdf.getInitializer();
						if (returnExpression instanceof ArrayInitializer) {
							if (!removeArrayVariable(node, vds, (ArrayInitializer) returnExpression)) {
								return true;
							}
						} else {
							replaceReturnStatement(node, vds, returnExpression);
						}
						setResult(false);
						return false;
					}
				} else {
					Assignment as= ASTNodes.asExpression(previousSibling, Assignment.class);
					if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN) && ASTNodes.isSameLocalVariable(node.getExpression(), as.getLeftHandSide())
							&& as.getLeftHandSide() instanceof Name
							&& !isUsedAfterReturn((IVariableBinding) ((Name) as.getLeftHandSide()).resolveBinding(),
									node)) {
						replaceReturnStatement(node, previousSibling, as.getRightHandSide());
						setResult(false);
						return false;
					}
				}
			}

			return true;
		}

		private boolean isUsedAfterReturn(final IVariableBinding varToSearch, final ASTNode scopeNode) {
			TryStatement tryStatement= ASTNodes.getAncestorOrNull(scopeNode, TryStatement.class);

			if (tryStatement == null) {
				return false;
			}

			if (tryStatement.getFinally() != null) {
				VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(
						varToSearch, tryStatement.getFinally(), true).find();
				if (!variableUseVisitor.getReads().isEmpty()) {
					return true;
				}
			}

			return isUsedAfterReturn(varToSearch, tryStatement);
		}

		private boolean removeArrayVariable(final ReturnStatement node, final VariableDeclarationStatement vds, final ArrayInitializer returnExpression) {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			Type varType= vds.getType();
			VariableDeclarationFragment varDeclFrag= (VariableDeclarationFragment) vds.fragments().get(0);

			if (varType instanceof ArrayType) {
				ArrayType arrayType= (ArrayType) varType;
				// Mixed c style/var style not supported yet. Abort instead of generating wrong code
				if (varDeclFrag.getExtraDimensions() > 0) {
					return false;
				}
				// Java style array "Type[] var"
				ReturnStatement newReturnStatement= ast
						.return0(ast.newArray(ast.createCopyTarget(arrayType), rewrite.createMoveTarget(returnExpression)));
				replaceReturnStatementForArray(node, vds, newReturnStatement);
			} else {
				// C style array "Type var[]"
				ArrayType arrayType= node.getAST().newArrayType(ast.createCopyTarget(vds.getType()), varDeclFrag.getExtraDimensions());
				ReturnStatement newReturnStatement= ast
						.return0(ast.newArray(arrayType, rewrite.createMoveTarget(returnExpression)));
				replaceReturnStatementForArray(node, vds, newReturnStatement);
			}

			return true;
		}

		private void replaceReturnStatementForArray(final ReturnStatement node, final Statement previousSibling,
				final ReturnStatement newReturnStatement) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			rewrite.remove(previousSibling, null);
			rewrite.replace(node, newReturnStatement, null);
		}

		private void replaceReturnStatement(final ReturnStatement node, final Statement previousSibling,
				final Expression returnExpression) {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			rewrite.remove(previousSibling, null);
			rewrite.replace(node, ast.return0(rewrite.createMoveTarget(returnExpression)), null);
		}
	}
}
