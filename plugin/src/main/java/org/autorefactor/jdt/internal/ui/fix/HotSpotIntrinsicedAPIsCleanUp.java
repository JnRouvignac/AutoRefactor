/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class HotSpotIntrinsicedAPIsCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_reason;
	}

	private static class SystemArrayCopyParams {
		private IVariableBinding indexVarBinding;
		private Expression indexStartPos;
		private Expression srcArrayExpression;
		private Expression srcPos;
		private Expression destArrayExpression;
		private Expression destPos;
		private Expression length;

		@Override
		public String toString() {
			return "System.arraycopy(" + srcArrayExpression + ", " + srcPos + ", " + destArrayExpression + ", " + destPos + ", " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
					+ length + ")"; //$NON-NLS-1$
		}
	}

	@Override
	public boolean visit(final ForStatement node) {
		SystemArrayCopyParams params= new SystemArrayCopyParams();
		collectUniqueIndex(node, params);
		IVariableBinding incrementedIdx= getUniqueIncrementedVariable(node);
		List<Statement> statements= ASTNodes.asList(node.getBody());

		if (Utils.equalNotNull(params.indexVarBinding, incrementedIdx) && statements.size() == 1) {
			collectLength(node.getExpression(), incrementedIdx, params);

			Assignment as= ASTNodes.asExpression(statements.get(0), Assignment.class);

			if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN)) {
				ArrayAccess aaLHS= ASTNodes.as(as.getLeftHandSide(), ArrayAccess.class);
				ArrayAccess aaRHS= ASTNodes.as(as.getRightHandSide(), ArrayAccess.class);

				if (aaLHS != null && aaRHS != null) {
					params.destArrayExpression= aaLHS.getArray();
					params.srcArrayExpression= aaRHS.getArray();

					if (ASTNodes.haveSameType(params.srcArrayExpression, params.destArrayExpression)) {
						params.destPos= calcIndex(aaLHS.getIndex(), params);
						params.srcPos= calcIndex(aaRHS.getIndex(), params);
						return maybeReplaceWithSystemArrayCopyCloneAll(node, params);
					}
				}
			}
		}

		return true;
	}

	private Expression calcIndex(final Expression index, final SystemArrayCopyParams params) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (index instanceof SimpleName) {
			IVariableBinding indexVar= getVariableBinding(index);

			if (Utils.equalNotNull(params.indexVarBinding, indexVar)) {
				return ast.createCopyTarget(params.indexStartPos);
			}
		} else if (index instanceof InfixExpression) {
			InfixExpression ie= (InfixExpression) index;

			if (!ie.hasExtendedOperands() && ASTNodes.hasOperator(ie, InfixExpression.Operator.PLUS)) {
				Expression leftOperand= ie.getLeftOperand();
				Expression rightOperand= ie.getRightOperand();

				if (leftOperand instanceof SimpleName) {
					IVariableBinding indexVar= getVariableBinding(leftOperand);

					if (Utils.equalNotNull(params.indexVarBinding, indexVar)) {
						return plus(rightOperand, params.indexStartPos);
					}
				}

				if (rightOperand instanceof SimpleName) {
					IVariableBinding indexVar= getVariableBinding(rightOperand);

					if (Utils.equalNotNull(params.indexVarBinding, indexVar)) {
						return plus(leftOperand, params.indexStartPos);
					}
				}
			}
		}

		return null;
	}

	private Expression plus(final Expression expression1, final Expression expression2) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		Long expr1Value= ASTNodes.integerLiteral(expression1);
		Long expr2Value= ASTNodes.integerLiteral(expression2);

		if (expr1Value != null && expr2Value != null) {
			return ast.int0((int) (expr1Value + expr2Value));
		}

		if (Long.valueOf(0).equals(expr1Value)) {
			return ast.createCopyTarget(expression2);
		}

		if (Long.valueOf(0).equals(expr2Value)) {
			return ast.createCopyTarget(expression1);
		}

		return ast.infixExpression(ast.createCopyTarget(expression1), InfixExpression.Operator.PLUS, ast.createCopyTarget(expression2));
	}

	private Expression minus(final Expression expression1, final Expression expression2) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		Long expr1Value= ASTNodes.integerLiteral(expression1);
		Long expr2Value= ASTNodes.integerLiteral(expression2);

		if (expr1Value != null && expr2Value != null) {
			return ast.int0((int) (expr1Value - expr2Value));
		}

		if (Long.valueOf(0).equals(expr1Value)) {
			throw new NotImplementedException(expression2, "Code is not implemented for negating expr2: " + expression2); //$NON-NLS-1$
		}

		if (Long.valueOf(0).equals(expr2Value)) {
			return ast.createCopyTarget(expression1);
		}

		return ast.infixExpression(ast.createCopyTarget(expression1), InfixExpression.Operator.MINUS, ast.createCopyTarget(expression2));
	}

	private Expression minusPlusOne(final Expression expression1, final Expression expression2) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		Long expr1Value= ASTNodes.integerLiteral(expression1);
		Long expr2Value= ASTNodes.integerLiteral(expression2);

		if (expr1Value != null && expr2Value != null) {
			return ast.int0((int) (expr1Value - expr2Value + 1));
		}

		if (Long.valueOf(0).equals(expr1Value)) {
			throw new NotImplementedException(expression2, "Code is not implemented for negating expr2: " + expression2); //$NON-NLS-1$
		}

		if (Long.valueOf(0).equals(expr2Value)) {
			return ast.infixExpression(ast.createCopyTarget(expression1), InfixExpression.Operator.PLUS, cuRewrite.getAST().newNumberLiteral("1")); //$NON-NLS-1$
		}

		return ast.infixExpression(ast.infixExpression(ast.createCopyTarget(expression1), InfixExpression.Operator.MINUS, ast.createCopyTarget(expression2)), InfixExpression.Operator.PLUS, cuRewrite.getAST().newNumberLiteral("1")); //$NON-NLS-1$
	}

	private void collectLength(final Expression condition, final IVariableBinding incrementedIdx,
			final SystemArrayCopyParams params) {
		InfixExpression infixExpression= ASTNodes.as(condition, InfixExpression.class);

		if (infixExpression != null) {
			if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.LESS, InfixExpression.Operator.LESS_EQUALS)) {
				collectLength(incrementedIdx, params, infixExpression, infixExpression.getLeftOperand(), infixExpression.getRightOperand());
			} else if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.GREATER, InfixExpression.Operator.GREATER_EQUALS)) {
				collectLength(incrementedIdx, params, infixExpression, infixExpression.getRightOperand(), infixExpression.getLeftOperand());
			}
		}
	}

	private void collectLength(final IVariableBinding incrementedIdx, final SystemArrayCopyParams params,
			final InfixExpression ie, final Expression variable, final Expression boundary) {
		IVariableBinding conditionIdx= getVariableBinding(variable);

		if (Utils.equalNotNull(incrementedIdx, conditionIdx)) {
			if (ASTNodes.hasOperator(ie, InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.GREATER_EQUALS)) {
				params.length= minusPlusOne(boundary, params.indexStartPos);
			} else {
				params.length= minus(boundary, params.indexStartPos);
			}
		}
	}

	private boolean maybeReplaceWithSystemArrayCopyCloneAll(final ForStatement node, final SystemArrayCopyParams params) {
		if (params.srcArrayExpression == null || params.srcPos == null || params.destArrayExpression == null
				|| params.destPos == null || params.length == null) {
			return true;
		}

		replaceWithSystemArrayCopy(node, params);
		return false;
	}

	private void replaceWithSystemArrayCopy(final ForStatement node, final SystemArrayCopyParams params) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		TryStatement tryStatement= ast.try0(
				ast.block(ast
						.toStatement(ast.newMethodInvocation(System.class.getSimpleName(), "arraycopy", rewrite.createMoveTarget(params.srcArrayExpression), params.srcPos, rewrite.createMoveTarget(params.destArrayExpression), params.destPos, params.length))), //$NON-NLS-1$
				ast.catch0(IndexOutOfBoundsException.class.getSimpleName(), "e", //$NON-NLS-1$
						ast.throw0(ast.new0(ArrayIndexOutOfBoundsException.class.getSimpleName(), ast.newMethodInvocation("e", "getMessage"))))); //$NON-NLS-1$ //$NON-NLS-2$

		rewrite.replace(node, tryStatement, null);
	}

	private void collectUniqueIndex(final ForStatement node, final SystemArrayCopyParams params) {
		if (ASTNodes.initializers(node).size() != 1) {
			return;
		}

		Expression initializer= ASTNodes.initializers(node).get(0);

		if (initializer instanceof VariableDeclarationExpression) {
			VariableDeclarationExpression vde= (VariableDeclarationExpression) initializer;

			if (ASTNodes.isPrimitive(vde, int.class.getSimpleName()) && ASTNodes.fragments(vde).size() == 1) {
				// This must be the array index
				VariableDeclarationFragment vdf= ASTNodes.fragments(vde).get(0);
				if (vdf.getExtraDimensions() == 0) {
					params.indexStartPos= vdf.getInitializer();
					params.indexVarBinding= vdf.resolveBinding();
				}
			}
		} else if (initializer instanceof Assignment) {
			Assignment assignment= (Assignment) initializer;

			if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN) && ASTNodes.isPrimitive(assignment.resolveTypeBinding(), int.class.getSimpleName())) {
				// This must be the array index
				params.indexStartPos= assignment.getRightHandSide();
				SimpleName lhs= ASTNodes.as(assignment.getLeftHandSide(), SimpleName.class);

				if (lhs != null) {
					IBinding binding= lhs.resolveBinding();

					if (binding instanceof IVariableBinding) {
						params.indexVarBinding= (IVariableBinding) binding;
					}
				}
			}
		}
	}

	private IVariableBinding getUniqueIncrementedVariable(final ForStatement node) {
		if (ASTNodes.updaters(node).size() != 1) {
			return null;
		}

		Expression updater= ASTNodes.updaters(node).get(0);

		if (updater instanceof PostfixExpression) {
			PostfixExpression postfixExpression= (PostfixExpression) updater;

			if (ASTNodes.hasOperator(postfixExpression, PostfixExpression.Operator.INCREMENT)) {
				return getVariableBinding(postfixExpression.getOperand());
			}
		} else if (updater instanceof PrefixExpression) {
			PrefixExpression prefixExpression= (PrefixExpression) updater;

			if (ASTNodes.hasOperator(prefixExpression, PrefixExpression.Operator.INCREMENT)) {
				return getVariableBinding(prefixExpression.getOperand());
			}
		}

		return null;
	}

	private IVariableBinding getVariableBinding(final Expression expression) {
		SimpleName simpleName= ASTNodes.as(expression, SimpleName.class);

		if (simpleName != null) {
			IBinding binding= simpleName.resolveBinding();

			if (binding instanceof IVariableBinding) { // This is a local variable or a field
				return (IVariableBinding) binding;
			}
		}

		return null;
	}
}
