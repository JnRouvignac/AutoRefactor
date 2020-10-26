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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class IncrementStatementRatherThanIncrementExpressionCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.IncrementStatementRatherThanIncrementExpressionCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.IncrementStatementRatherThanIncrementExpressionCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.IncrementStatementRatherThanIncrementExpressionCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		NewAndPutAllMethodVisitor newAndPutAllMethodVisitor= new NewAndPutAllMethodVisitor();
		newAndPutAllMethodVisitor.visitNode(node);
		return newAndPutAllMethodVisitor.result;
	}

	private final class NewAndPutAllMethodVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final PrefixExpression node) {
			if (ASTNodes.hasOperator(node, PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.DECREMENT)) {
				return visitExpression(node, node.getOperand());
			}

			return true;
		}

		@Override
		public boolean visit(final PostfixExpression node) {
			if (ASTNodes.hasOperator(node, PostfixExpression.Operator.INCREMENT, PostfixExpression.Operator.DECREMENT)) {
				return visitExpression(node, node.getOperand());
			}

			return true;
		}

		public boolean visitExpression(final Expression node, final Expression variable) {
			SimpleName variableName= ASTNodes.as(variable, SimpleName.class);

			if (result
					&& !(node.getParent() instanceof ExpressionStatement)
					&& variableName != null
					&& variableName.resolveBinding() != null
					&& variableName.resolveBinding().getKind() == IBinding.VARIABLE
					&& ASTNodes.isLocalVariable(variableName.resolveBinding())) {
				return visitParent(node, variable, node);
			}

			return true;
		}

		public boolean visitParent(final Expression node, final Expression variable, final ASTNode parent) {
			ASTNode ancestor= parent.getParent();

			if (ancestor != null) {
				switch (ancestor.getNodeType()) {
				case ASTNode.IF_STATEMENT:
					IfStatement statement= (IfStatement) ancestor;

					if (node instanceof PrefixExpression
							&& parent.getLocationInParent() == IfStatement.EXPRESSION_PROPERTY
							&& !ASTNodes.isInElse(statement)) {
						return maybeExtractIncrement(node, variable, statement);
					}

					return true;

				case ASTNode.LABELED_STATEMENT:
				case ASTNode.VARIABLE_DECLARATION_STATEMENT:
				case ASTNode.EXPRESSION_STATEMENT:
					return maybeExtractIncrement(node, variable, (Statement) ancestor);

				case ASTNode.THROW_STATEMENT:
				case ASTNode.RETURN_STATEMENT:
					if (node instanceof PrefixExpression) {
						return maybeExtractIncrement(node, variable, (Statement) ancestor);
					}

					return true;

				case ASTNode.CONSTRUCTOR_INVOCATION:
				case ASTNode.SUPER_CONSTRUCTOR_INVOCATION:
					if (node instanceof PostfixExpression) {
						return maybeExtractIncrement(node, variable, (Statement) ancestor);
					}

					return true;

				case ASTNode.QUALIFIED_NAME:
				case ASTNode.SIMPLE_NAME:
				case ASTNode.FIELD_ACCESS:
				case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
				case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
				case ASTNode.ASSIGNMENT:
				case ASTNode.INSTANCEOF_EXPRESSION:
				case ASTNode.CLASS_INSTANCE_CREATION:
				case ASTNode.METHOD_INVOCATION:
				case ASTNode.SUPER_METHOD_INVOCATION:
				case ASTNode.CAST_EXPRESSION:
				case ASTNode.PARENTHESIZED_EXPRESSION:
				case ASTNode.POSTFIX_EXPRESSION:
				case ASTNode.PREFIX_EXPRESSION:
				case ASTNode.ARRAY_ACCESS:
				case ASTNode.ARRAY_CREATION:
				case ASTNode.ARRAY_INITIALIZER:
					return visitParent(node, variable, ancestor);

				case ASTNode.INFIX_EXPRESSION:
					if (parent.getLocationInParent() == InfixExpression.LEFT_OPERAND_PROPERTY
						|| ASTNodes.hasOperator((InfixExpression) ancestor,
							InfixExpression.Operator.AND,
							InfixExpression.Operator.DIVIDE,
							InfixExpression.Operator.EQUALS,
							InfixExpression.Operator.GREATER,
							InfixExpression.Operator.GREATER_EQUALS,
							InfixExpression.Operator.LEFT_SHIFT,
							InfixExpression.Operator.LESS,
							InfixExpression.Operator.LESS_EQUALS,
							InfixExpression.Operator.MINUS,
							InfixExpression.Operator.NOT_EQUALS,
							InfixExpression.Operator.OR,
							InfixExpression.Operator.PLUS,
							InfixExpression.Operator.REMAINDER,
							InfixExpression.Operator.RIGHT_SHIFT_SIGNED,
							InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED,
							InfixExpression.Operator.TIMES,
							InfixExpression.Operator.XOR)) {
						return visitParent(node, variable, ancestor);
					}

					return true;

				case ASTNode.CONDITIONAL_EXPRESSION:
					if (parent.getLocationInParent() == ConditionalExpression.EXPRESSION_PROPERTY) {
						return visitParent(node, variable, ancestor);
					}

					return true;

				default:
				}
			}

			return true;
		}

		private boolean maybeExtractIncrement(final Expression node, final Expression variable, final Statement statement) {
			SimpleName variableName= ASTNodes.as(variable, SimpleName.class);
			VarDefinitionsUsesVisitor varDefinitionsUsesVisitor= new VarDefinitionsUsesVisitor((IVariableBinding) variableName.resolveBinding(), statement, true).find();

			if (varDefinitionsUsesVisitor.getWrites().isEmpty()
					&& varDefinitionsUsesVisitor.getReads().size() == 1
					&& (node instanceof PrefixExpression || !ASTNodes.fallsThrough(statement))) {
				extractIncrement(node, variable, statement);

				result= false;
				return false;
			}

			return true;
		}

		private void extractIncrement(final Expression node, final Expression variable,
				final Statement statement) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.IncrementStatementRatherThanIncrementExpressionCleanUp_description);

			ASTNodes.replaceButKeepComment(rewrite, ASTNodes.getMatchingParent(node, ParenthesizedExpression.class), ast.createCopyTarget(variable), group);

			if (node instanceof PostfixExpression) {
				Statement newAssignment= ast.newExpressionStatement(ASTNodes.createMoveTarget(rewrite, node));

				if (ASTNodes.canHaveSiblings(statement)) {
					rewrite.insertAfter(newAssignment, statement, group);
				} else {
					Block newBlock1= ast.newBlock();
					newBlock1.statements().add(ASTNodes.createMoveTarget(rewrite, statement));
					newBlock1.statements().add(newAssignment);
					Block newBlock= newBlock1;

					ASTNodes.replaceButKeepComment(rewrite, statement, newBlock, group);
				}
			} else {
				Statement newAssignment;
				if (ASTNodes.hasOperator((PrefixExpression) node, PrefixExpression.Operator.INCREMENT)) {
					newAssignment= ast.newExpressionStatement(ast.newPostfixExpression(ASTNodes.createMoveTarget(rewrite, variable), PostfixExpression.Operator.INCREMENT));
				} else {
					newAssignment= ast.newExpressionStatement(ast.newPostfixExpression(ASTNodes.createMoveTarget(rewrite, variable), PostfixExpression.Operator.DECREMENT));
				}

				if (ASTNodes.canHaveSiblings(statement)) {
					rewrite.insertBefore(newAssignment, statement, group);
				} else {
					Block newBlock1= ast.newBlock();
					newBlock1.statements().add(newAssignment);
					newBlock1.statements().add(ASTNodes.createMoveTarget(rewrite, statement));
					Block newBlock= newBlock1;

					ASTNodes.replaceButKeepComment(rewrite, statement, newBlock, group);
				}
			}

		}
	}
}
