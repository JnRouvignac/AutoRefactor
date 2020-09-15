/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
 * Copyright (C) 2019 Fabrice Tiercelin - Change the parsing of condition
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
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class NoAssignmentInIfConditionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.NoAssignmentInIfConditionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.NoAssignmentInIfConditionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.NoAssignmentInIfConditionCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		IfWithAssignmentVisitor ifWithAssignmentVisitor= new IfWithAssignmentVisitor();
		ifWithAssignmentVisitor.visitNode(node);
		return ifWithAssignmentVisitor.result;
	}

	private final class IfWithAssignmentVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final IfStatement node) {
			return !result || moveAssignmentBeforeIfStatementIfPossible(node, node.getExpression(), new ArrayList<Expression>());
		}

		private boolean moveAssignmentBeforeIfStatementIfPossible(final IfStatement node, final Expression expression, final List<Expression> evaluatedExpression) {
			Assignment assignment= ASTNodes.as(expression, Assignment.class);

			if (assignment != null) {
				return moveAssignmentBeforeIfStatement(node, assignment, evaluatedExpression);
			}

			PrefixExpression prefixExpression= ASTNodes.as(expression, PrefixExpression.class);

			if (prefixExpression != null && ASTNodes.hasOperator(prefixExpression,
					PrefixExpression.Operator.NOT,
					PrefixExpression.Operator.COMPLEMENT,
					PrefixExpression.Operator.MINUS,
					PrefixExpression.Operator.PLUS)) {
				return moveAssignmentBeforeIfStatementIfPossible(node, prefixExpression.getOperand(), evaluatedExpression);
			}

			InfixExpression infixExpression= ASTNodes.as(expression, InfixExpression.class);

			if (infixExpression != null) {
				List<Expression> operands= ASTNodes.allOperands(infixExpression);
				boolean isAllOperandsEvaluated= ASTNodes.hasOperator(infixExpression,
						InfixExpression.Operator.EQUALS,
						InfixExpression.Operator.NOT_EQUALS,
						InfixExpression.Operator.PLUS,
						InfixExpression.Operator.MINUS,
						InfixExpression.Operator.DIVIDE,
						InfixExpression.Operator.TIMES,
						InfixExpression.Operator.XOR,
						InfixExpression.Operator.GREATER,
						InfixExpression.Operator.GREATER_EQUALS,
						InfixExpression.Operator.LEFT_SHIFT,
						InfixExpression.Operator.LESS,
						InfixExpression.Operator.LESS_EQUALS,
						InfixExpression.Operator.REMAINDER,
						InfixExpression.Operator.RIGHT_SHIFT_SIGNED,
						InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED,
						InfixExpression.Operator.AND,
						InfixExpression.Operator.OR);

				for (Expression operand : operands) {
					if (!moveAssignmentBeforeIfStatementIfPossible(node, operand, evaluatedExpression)) {
						return false;
					}

					if (!isAllOperandsEvaluated || !ASTNodes.isPassive(operand)) {
						break;
					}

					evaluatedExpression.add(operand);
				}
			}

			ConditionalExpression ce= ASTNodes.as(expression, ConditionalExpression.class);

			return ce == null || moveAssignmentBeforeIfStatementIfPossible(node, ce.getExpression(), evaluatedExpression);
		}

		private boolean moveAssignmentBeforeIfStatement(final IfStatement node, final Assignment assignment, final List<Expression> evaluatedExpression) {
			Expression lhs= ASTNodes.getUnparenthesedExpression(assignment.getLeftHandSide());

			if (!evaluatedExpression.isEmpty()) {
				Name mame= ASTNodes.as(lhs, Name.class);
				FieldAccess fieldAccess= ASTNodes.as(lhs, FieldAccess.class);
				SuperFieldAccess superFieldAccess= ASTNodes.as(lhs, SuperFieldAccess.class);
				IVariableBinding variableBinding;

				if (mame != null) {
					IBinding binding= mame.resolveBinding();

					if (!(binding instanceof IVariableBinding)) {
						return true;
					}

					variableBinding= (IVariableBinding) binding;
				} else if (fieldAccess != null) {
					variableBinding= fieldAccess.resolveFieldBinding();
				} else if (superFieldAccess != null) {
					variableBinding= superFieldAccess.resolveFieldBinding();
				} else {
					return true;
				}

				for (Expression expression : evaluatedExpression) {
					VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(variableBinding,
							expression, true).find();

					if (!variableUseVisitor.getReads().isEmpty()) {
						return true;
					}
				}
			}

			VariableDeclarationStatement vds= ASTNodes.as(ASTNodes.getPreviousSibling(node), VariableDeclarationStatement.class);
			VariableDeclarationFragment fragment= findVariableDeclarationFragment(vds, lhs);

			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.NoAssignmentInIfConditionCleanUp_description);

			TextEditGroup editGroup= new TextEditGroup(NoAssignmentInIfConditionCleanUp.class.getCanonicalName());

			if (fragment != null && (fragment.getInitializer() == null || ASTNodes.isPassive(fragment.getInitializer()))) {
				rewrite.set(fragment, VariableDeclarationFragment.INITIALIZER_PROPERTY, assignment.getRightHandSide(), group);
				rewrite.replace(ASTNodes.getParent(assignment, ParenthesizedExpression.class), ast.createCopyTarget(lhs), group);
				this.result= false;
				return false;
			}

			if (!ASTNodes.isInElse(node)) {
				rewrite.replace(ASTNodes.getParent(assignment, ParenthesizedExpression.class), ast.createCopyTarget(lhs), group);
				Statement newAssignment= ast.toStatement(ASTNodes.createMoveTarget(rewrite, assignment));

				if (ASTNodes.canHaveSiblings(node)) {
					rewrite.insertBefore(newAssignment, node, group);
				} else {
					Block newBlock= ast.block(newAssignment, ASTNodes.createMoveTarget(rewrite, node));
					rewrite.replace(node, newBlock, group);
				}

				this.result= false;
				return false;
			}

			return true;
		}

		@SuppressWarnings("unchecked")
		private VariableDeclarationFragment findVariableDeclarationFragment(final VariableDeclarationStatement vds,
				final Expression expression) {
			if (vds != null && expression instanceof SimpleName) {
				for (VariableDeclarationFragment fragment : (List<VariableDeclarationFragment>) vds.fragments()) {
					if (ASTNodes.isSameVariable(expression, fragment)) {
						return fragment;
					}
				}
			}

			return null;
		}
	}
}
