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
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
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

	// TODO Very few parenthesized expressions are actually needed. They are:
	// 1) inside InfixExpressions with logical operators (&&, ||, etc.)
	// Sometimes needed to explicit code, some like it like that too
	// 2) Inside String concatenations if they hold an InfixExpression that does
	// not resolve to String (what about PrefixExpression and
	// PostFixExpression?)
	// 3) Around CastExpression
	// Any others?

	// TODO JNR String s = "some " + " string " + "" + ( "fhj" + "prout" );

	@Override
	public boolean visit(final ParenthesizedExpression node) {
		Expression innerExpression= getExpressionWithoutParentheses(node, node.getParent());

		if (innerExpression != node) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			rewrite.replace(node, rewrite.createMoveTarget(innerExpression), null);
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

	private Expression getExpressionWithoutParentheses(final ParenthesizedExpression node, final ASTNode parent) {
		Expression innerExpression= node.getExpression();

		if (innerExpression instanceof ParenthesizedExpression) {
			return getExpressionWithoutParentheses((ParenthesizedExpression) innerExpression, parent);
		}

		if (parent instanceof InfixExpression) {
			InfixExpression parentInfixExpression = (InfixExpression) parent;

			if (innerExpression instanceof InfixExpression) {
				InfixExpression.Operator innerOp = ((InfixExpression) innerExpression).getOperator();

				if (innerOp == parentInfixExpression.getOperator()
						&& OperatorEnum.isAssociative(innerOp)
						// Leave String concatenations with mixed type
						// to other if statements in this method.
						&& Utils.equalNotNull(innerExpression.resolveTypeBinding(), parentInfixExpression.resolveTypeBinding())) {
					return innerExpression;
				}
			}
		}

		// Infix, prefix or postfix without parenthesis is not readable
		if ((parent instanceof InfixExpression
				&& ASTNodes.hasOperator((InfixExpression) parent, InfixExpression.Operator.PLUS, InfixExpression.Operator.MINUS)
				|| parent instanceof PrefixExpression
						&& ASTNodes.hasOperator((PrefixExpression) parent, PrefixExpression.Operator.PLUS, PrefixExpression.Operator.MINUS)) && (innerExpression instanceof PrefixExpression
				&& ASTNodes.hasOperator((PrefixExpression) innerExpression, PrefixExpression.Operator.DECREMENT, PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.PLUS, PrefixExpression.Operator.MINUS) || innerExpression instanceof PostfixExpression
				&& ASTNodes.hasOperator((PostfixExpression) innerExpression, PostfixExpression.Operator.DECREMENT, PostfixExpression.Operator.INCREMENT)) || isInnerExprHardToRead(innerExpression, parent)) {
			return node;
		}

		if (isUselessParenthesesInStatement(parent, node)) {
			return innerExpression;
		}

		int compareTo= OperatorEnum.compareTo(innerExpression, parent);

		if (compareTo < 0) {
			return node;
		}

		if (compareTo > 0) {
			return innerExpression;
		}

		if (
				// TODO JNR can we revert the condition in the InfixExpression?
				// parentheses are sometimes needed to explicit code,
				// some like it like that
				innerExpression instanceof InfixExpression
				// TODO JNR add additional code to check if the cast is really required
				// or if it can be removed.
				|| innerExpression instanceof CastExpression
				// Infix and prefix or postfix without parenthesis is not readable
				|| (parent instanceof InfixExpression
						|| parent instanceof PrefixExpression
						|| parent instanceof PostfixExpression)
						&& (innerExpression instanceof PrefixExpression
								|| innerExpression instanceof PostfixExpression)) {
			return node;
		}

		return innerExpression;
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
			InfixExpression innerIe= (InfixExpression) innerExpression;
			InfixExpression.Operator innerOp= innerIe.getOperator();
			InfixExpression.Operator parentOp= ((InfixExpression) parent).getOperator();
			return ASTNodes.hasOperator((InfixExpression) parent, InfixExpression.Operator.EQUALS) || shouldHaveParentheses(innerOp, parentOp)
					|| ASTNodes.is(innerIe.getLeftOperand(), Assignment.class)
					|| ASTNodes.is(innerIe.getRightOperand(), Assignment.class);
		}

		return false;
	}

	private boolean isUselessParenthesesInStatement(final ASTNode parent, final ParenthesizedExpression node) {
		switch (parent.getNodeType()) {
		case ASTNode.ASSIGNMENT:
			Assignment a= (Assignment) parent;
			return node.equals(a.getRightHandSide());

		case ASTNode.METHOD_INVOCATION:
			MethodInvocation mi= (MethodInvocation) parent;
			return ASTNodes.arguments(mi).contains(node) || canRemoveParenthesesAroundExpression(mi, node);

		case ASTNode.IF_STATEMENT:
			IfStatement is= (IfStatement) parent;
			return node.equals(is.getExpression());

		case ASTNode.WHILE_STATEMENT:
			WhileStatement ws= (WhileStatement) parent;
			return node.equals(ws.getExpression());

		case ASTNode.DO_STATEMENT:
			DoStatement ds= (DoStatement) parent;
			return node.equals(ds.getExpression());

		case ASTNode.RETURN_STATEMENT:
			ReturnStatement rs= (ReturnStatement) parent;
			return node.equals(rs.getExpression());

		case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
			VariableDeclarationFragment vdf= (VariableDeclarationFragment) parent;
			return node.equals(vdf.getInitializer());

		default:
			return false;
		}
	}

	private boolean canRemoveParenthesesAroundExpression(final MethodInvocation mi, final ParenthesizedExpression node) {
		if (node.equals(mi.getExpression())) {
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
