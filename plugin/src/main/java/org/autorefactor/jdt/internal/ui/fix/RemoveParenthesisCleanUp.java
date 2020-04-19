/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class RemoveParenthesisCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveParenthesisCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveParenthesisCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveParenthesisCleanUp_reason;
	}

	/**
	 * A mapping of child operation to parent operation that mandates using
	 * parentheses.
	 */
	private static final Map<InfixExpression.Operator, List<InfixExpression.Operator>> SHOULD_HAVE_PARENTHESES= new HashMap<InfixExpression.Operator, List<InfixExpression.Operator>>() {
		private static final long serialVersionUID= -8949107654517355855L;

		{
			put(InfixExpression.Operator.CONDITIONAL_AND, Arrays.asList(InfixExpression.Operator.CONDITIONAL_OR));
			put(InfixExpression.Operator.AND, Arrays.asList(InfixExpression.Operator.XOR, InfixExpression.Operator.OR));
			put(InfixExpression.Operator.XOR, Arrays.asList(InfixExpression.Operator.OR));
			put(InfixExpression.Operator.LEFT_SHIFT, Arrays.asList(InfixExpression.Operator.OR, InfixExpression.Operator.AND));
			put(InfixExpression.Operator.RIGHT_SHIFT_SIGNED, Arrays.asList(InfixExpression.Operator.OR, InfixExpression.Operator.AND));
			put(InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED, Arrays.asList(InfixExpression.Operator.OR, InfixExpression.Operator.AND));
		}
	};

	@Override
	public boolean visit(final ParenthesizedExpression node) {
		Expression expressionWithoutParentheses= getExpressionWithoutParentheses(node);

		if (expressionWithoutParentheses != null) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			rewrite.replace(node, rewrite.createMoveTarget(expressionWithoutParentheses), null);
			return false;
		}

		return true;
	}

	@Override
	public boolean visit(final InfixExpression node) {
		if (shouldHaveParentheses(node)) {
			addParentheses(node);
			return false;
		}

		return true;
	}

	private Expression getExpressionWithoutParentheses(final ParenthesizedExpression parenthesis) {
		ASTNode parent= parenthesis.getParent();
		Expression child= parenthesis.getExpression();

		if (isParenthesesUselessForParent(parent, parenthesis)
				|| isParenthesesUselessForChild(child)) {
			return child;
		}

		if (parent instanceof InfixExpression) {
			InfixExpression parentInfixExpression = (InfixExpression) parent;

			if (child instanceof InfixExpression) {
				InfixExpression.Operator innerOp = ((InfixExpression) child).getOperator();

				if (innerOp == parentInfixExpression.getOperator()
						&& OperatorEnum.isAssociative(innerOp)
						// Leave String concatenations with mixed type
						// to other if statements in this method.
						&& Utils.equalNotNull(child.resolveTypeBinding(), parentInfixExpression.resolveTypeBinding())) {
					return child;
				}
			}
		}

		// Infix, prefix or postfix without parenthesis is not readable
		if (isInnerExprHardToRead(child, parent)) {
			return null;
		}

		if (parent instanceof InfixExpression
				&& ASTNodes.hasOperator((InfixExpression) parent, InfixExpression.Operator.PLUS, InfixExpression.Operator.MINUS)
				|| parent instanceof PrefixExpression
						&& ASTNodes.hasOperator((PrefixExpression) parent, PrefixExpression.Operator.PLUS, PrefixExpression.Operator.MINUS)) {
			if (child instanceof PrefixExpression
					&& ASTNodes.hasOperator((PrefixExpression) child, PrefixExpression.Operator.DECREMENT, PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.PLUS, PrefixExpression.Operator.MINUS)) {
				return null;
			}

			if (child instanceof PostfixExpression
					&& ASTNodes.hasOperator((PostfixExpression) child, PostfixExpression.Operator.DECREMENT, PostfixExpression.Operator.INCREMENT)) {
				return null;
			}

			if (child instanceof NumberLiteral
					&& (((NumberLiteral) child).getToken().startsWith("+") || ((NumberLiteral) child).getToken().startsWith("-"))) { //$NON-NLS-1$ //$NON-NLS-2$
				return null;
			}
		}

		int compareTo= OperatorEnum.compareTo(child, parent);

		if (compareTo < 0) {
			return null;
		}

		if (compareTo > 0) {
			return child;
		}

		if (
				// TODO JNR can we revert the condition in the InfixExpression?
				// parentheses are sometimes needed to explicit code,
				// some like it like that
				child instanceof InfixExpression
				|| child instanceof CastExpression
				// Infix and prefix or postfix without parenthesis is not readable
				|| (parent instanceof InfixExpression
						|| parent instanceof PrefixExpression
						|| parent instanceof PostfixExpression)
						&& (child instanceof PrefixExpression
								|| child instanceof PostfixExpression)) {
			return null;
		}

		return child;
	}

	private boolean isParenthesesUselessForChild(final Expression child) {
		return Arrays.asList(
				ASTNode.PARENTHESIZED_EXPRESSION,
				ASTNode.SIMPLE_NAME,
				ASTNode.QUALIFIED_NAME,
				ASTNode.THIS_EXPRESSION,
				ASTNode.ARRAY_ACCESS,
				ASTNode.FIELD_ACCESS,
				ASTNode.SUPER_FIELD_ACCESS,
				ASTNode.METHOD_INVOCATION,
				ASTNode.SUPER_METHOD_INVOCATION,
				ASTNode.CLASS_INSTANCE_CREATION,
				ASTNode.CONSTRUCTOR_INVOCATION,
				ASTNode.SUPER_CONSTRUCTOR_INVOCATION,
				ASTNode.SUPER_METHOD_INVOCATION,
				ASTNode.BOOLEAN_LITERAL,
				ASTNode.CHARACTER_LITERAL,
				ASTNode.NULL_LITERAL,
				ASTNode.NUMBER_LITERAL,
				ASTNode.STRING_LITERAL
				).contains(child.getNodeType());
	}

	/**
	 * Returns whether the supplied expression is complex enough to read.
	 *
	 * @param innerExpression the inner expression to test for ease of read
	 * @param parent    the parent node to test for ease of read
	 * @return true if the expressions is hard to read, false otherwise
	 */
	private boolean isInnerExprHardToRead(final Expression innerExpression, final ASTNode parent) {
		if (parent instanceof ConditionalExpression) {
			return innerExpression instanceof ConditionalExpression || innerExpression instanceof Assignment
					|| innerExpression instanceof InstanceofExpression || innerExpression instanceof InfixExpression;
		}

		if (parent instanceof InfixExpression && innerExpression instanceof InfixExpression) {
			InfixExpression innerInfixExpression= (InfixExpression) innerExpression;
			InfixExpression.Operator innerOp= innerInfixExpression.getOperator();
			InfixExpression.Operator parentOp= ((InfixExpression) parent).getOperator();
			return ASTNodes.hasOperator((InfixExpression) parent, InfixExpression.Operator.EQUALS) || shouldHaveParentheses(innerOp, parentOp)
					|| ASTNodes.is(innerInfixExpression.getLeftOperand(), Assignment.class)
					|| ASTNodes.is(innerInfixExpression.getRightOperand(), Assignment.class);
		}

		return false;
	}

	private boolean isParenthesesUselessForParent(final ASTNode parent, final ParenthesizedExpression node) {
		switch (parent.getNodeType()) {
		case ASTNode.ASSIGNMENT:
			Assignment assignment= (Assignment) parent;
			return node.equals(assignment.getRightHandSide());

		case ASTNode.METHOD_INVOCATION:
			MethodInvocation methodInvocation= (MethodInvocation) parent;
			return ASTNodes.arguments(methodInvocation).contains(node) || canRemoveParenthesesAroundExpression(methodInvocation, node);

		case ASTNode.SUPER_METHOD_INVOCATION:
			SuperMethodInvocation superMethodInvocation= (SuperMethodInvocation) parent;
			return ASTNodes.arguments(superMethodInvocation).contains(node);

		case ASTNode.IF_STATEMENT:
			IfStatement ifStatement= (IfStatement) parent;
			return node.equals(ifStatement.getExpression());

		case ASTNode.WHILE_STATEMENT:
			WhileStatement whileStatement= (WhileStatement) parent;
			return node.equals(whileStatement.getExpression());

		case ASTNode.DO_STATEMENT:
			DoStatement doStatement= (DoStatement) parent;
			return node.equals(doStatement.getExpression());

		case ASTNode.RETURN_STATEMENT:
			ReturnStatement returnStatement= (ReturnStatement) parent;
			return node.equals(returnStatement.getExpression());

		case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
			VariableDeclarationFragment variableDeclarationFragment= (VariableDeclarationFragment) parent;
			return node.equals(variableDeclarationFragment.getInitializer());

		case ASTNode.ARRAY_ACCESS:
			return node.getLocationInParent() == ArrayAccess.INDEX_PROPERTY;

		case ASTNode.ARRAY_INITIALIZER:
			return true;

		default:
			return false;
		}
	}

	private boolean canRemoveParenthesesAroundExpression(final MethodInvocation mi, final ParenthesizedExpression node) {
		final Expression callingExpression= mi.getExpression();
		if (node.equals(callingExpression)) {
			switch (node.getExpression().getNodeType()) {
			case ASTNode.ASSIGNMENT:
			case ASTNode.CAST_EXPRESSION:
			case ASTNode.CONDITIONAL_EXPRESSION:
			case ASTNode.INFIX_EXPRESSION:
				return false;

			default:
				return true;
			}
		}

		return false;
	}

	private boolean shouldHaveParentheses(final InfixExpression node) {
		InfixExpression.Operator childOp= node.getOperator();

		if (node.getParent() instanceof InfixExpression) {
			InfixExpression ie= (InfixExpression) node.getParent();
			return shouldHaveParentheses(childOp, ie.getOperator());
		}

		return false;
	}

	private boolean shouldHaveParentheses(final InfixExpression.Operator actualChildOp, final InfixExpression.Operator actualParentOp) {
		List<InfixExpression.Operator> parentOps= SHOULD_HAVE_PARENTHESES.get(actualChildOp);
		return parentOps != null && parentOps.contains(actualParentOp);
	}

	private void addParentheses(final Expression expression) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		rewrite.replace(expression, ast.parenthesize(rewrite.createMoveTarget(expression)), null);
	}
}
