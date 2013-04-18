/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Simplify Java expressions:
 * <ul>
 * <li>Remove redundant null checks or useless RHS or LHS operands</li>
 * <li>Fixing compareTo() usage</li>
 * <li>Removed useless parentheses</li>
 * <li>Removed "this." from method calls</li>
 * <li>Directly check boolean values instead of comparing against true / false</li>
 * </ul>
 */
public class SimplifyExpressionRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private final Refactorings refactorings = new Refactorings();
	private AST ast;
	private Release javaSERelease;
	private int javaMinorVersion;

	public SimplifyExpressionRefactoring() {
		super();
	}

	public void setAST(final AST ast) {
		this.ast = ast;
	}

	public void setJavaSERelease(Release javaSERelease) {
		this.javaSERelease = javaSERelease;
		this.javaMinorVersion = this.javaSERelease.getMinorVersion();
	}

	// TODO JNR remove avoidable boxing / unboxing

	// TODO Very few parenthesized expressions are actually needed. They are:
	// 1) inside InfixExpressions with logical operators (&&, ||, etc.)
	// Sometimes needed to explicit code, some like it like that too
	// 2) Inside String concatenations if they hold an InfixExpression that does
	// not resolve to String (what about PrefixExpression and
	// PostFixExpression?)
	// 3) Around CastExpression
	// Any others?

	// TODO JNR !true => false and !false => true

	// TODO JNR String s = "some " + " string " + "" + ( "fhj" + "prout" );

	@Override
	public boolean visit(ParenthesizedExpression node) {
		final Expression innerExpr = getExpressionWithoutParentheses(node);
		if (innerExpr != node) {
			final Expression innerExprCopy = ASTHelper.copySubtree(this.ast,
					innerExpr);
			this.refactorings.replace(node, innerExprCopy);
			innerExprCopy.accept(this);
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private Expression getExpressionWithoutParentheses(
			ParenthesizedExpression node) {
		final ASTNode parent = node.getParent();
		final Expression innerExpr = node.getExpression();
		if (innerExpr instanceof ParenthesizedExpression) {
			return getExpressionWithoutParentheses((ParenthesizedExpression) innerExpr);
		}
		if (innerExpr instanceof InfixExpression
				&& parent instanceof InfixExpression) {
			final InfixExpression innerInfixExpr = (InfixExpression) innerExpr;
			final InfixExpression parentInfixExpr = (InfixExpression) parent;
			final Operator innerOp = innerInfixExpr.getOperator();
			final Operator outerOp = parentInfixExpr.getOperator();
			if (innerOp == parentInfixExpr.getOperator()
					&& isAssociativeOperator(innerOp)
					// Leave String concatenations with mixed type
					// to other if statements in this method.
					&& innerExpr.resolveTypeBinding().equals(
							parentInfixExpr.resolveTypeBinding())) {
				return innerExpr;
			} else if (isReadable(innerOp, outerOp)) {
				return innerExpr;
			}
		}
		if (parent instanceof InfixExpression) {
			final InfixExpression parentInfixExpr = (InfixExpression) parent;
			if (innerExpr instanceof InstanceofExpression
					&& !ASTHelper.hasType(innerExpr, "java.lang.String")
					&& ASTHelper.hasType(parentInfixExpr, "java.lang.String")) {
				// The parentheses hold an InfixExpression that does not resolve
				// to String but is inside a String concatenation
				return node;
			}
			if (innerExpr instanceof ConditionalExpression
					&& ASTHelper.hasType(parentInfixExpr, "java.lang.String")) {
				// The parentheses hold a ConditionalExpression that is inside a String
				// concatenation
				return node;
			}
		}
		if (parent instanceof PrefixExpression
				&& innerExpr instanceof InstanceofExpression) {
			// Cannot remove parentheses around InstanceofExpression if it is negated
			return node;
		}
		if (isUselessParenthesesInStatement(parent, node)) {
			return innerExpr;
		}
		if (
		// TODO JNR can we revert the condition in the InfixExpression?
		// parentheses are sometimes needed to explicit code,
		// some like it like that
		innerExpr instanceof InfixExpression
		// TODO JNR add additional code to check if the cast i really required
		// or if it can be removed.
				|| innerExpr instanceof CastExpression) {
			return node;
		}
		return innerExpr;
	}

	/**
	 * Returns whether a condition using the specified inner operation within the
	 * specified outer operation remains easily readable.
	 *
	 * @param innerOp
	 *          the inner operation
	 * @param outerOp
	 *          the outer operation
	 * @return true if the made up condition would still be readable, false
	 *         otherwise
	 */
	private boolean isReadable(Operator innerOp, Operator outerOp)
	{
		if (Operator.CONDITIONAL_AND.equals(outerOp)
				|| Operator.CONDITIONAL_OR.equals(outerOp)) {
			if (Operator.EQUALS.equals(innerOp)
					|| Operator.NOT_EQUALS.equals(innerOp)
					|| Operator.GREATER.equals(innerOp)
					|| Operator.GREATER_EQUALS.equals(innerOp)
					|| Operator.LESS.equals(innerOp)
					|| Operator.LESS_EQUALS.equals(innerOp)
					|| Operator.CONDITIONAL_AND.equals(innerOp)
					|| Operator.CONDITIONAL_OR.equals(innerOp)) {
				return true;
			}
		}
		return false;
	}

	private boolean isUselessParenthesesInStatement(final ASTNode parent,
			ParenthesizedExpression node) {
		if (parent instanceof Expression) {
			if (parent instanceof Assignment) {
				Assignment a = (Assignment) parent;
				return node.equals(a.getRightHandSide());
			} else if (parent instanceof MethodInvocation) {
				MethodInvocation mi = (MethodInvocation) parent;
				return mi.arguments().contains(node);
			}
		} else if (parent instanceof Statement) {
			if (parent instanceof IfStatement) {
				IfStatement is = (IfStatement) parent;
				return node.equals(is.getExpression());
			} else if (parent instanceof WhileStatement) {
				WhileStatement ws = (WhileStatement) parent;
				return node.equals(ws.getExpression());
			} else if (parent instanceof DoStatement) {
				DoStatement ds = (DoStatement) parent;
				return node.equals(ds.getExpression());
			} else if (parent instanceof ReturnStatement) {
				ReturnStatement rs = (ReturnStatement) parent;
				return node.equals(rs.getExpression());
			}
		} else if (parent instanceof VariableDeclarationFragment) {
			VariableDeclarationFragment vdf = (VariableDeclarationFragment) parent;
			return node.equals(vdf.getInitializer());
		}
		return false;
	}

	private boolean isAssociativeOperator(final Operator innerOp) {
		return innerOp == Operator.CONDITIONAL_AND
				|| innerOp == Operator.CONDITIONAL_OR
				|| innerOp == Operator.PLUS || innerOp == Operator.TIMES
				|| innerOp == Operator.AND || innerOp == Operator.OR
				|| innerOp == Operator.XOR;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null) {
			// TODO JNR handle same class calls and sub classes
			return ASTHelper.VISIT_SUBTREE;
		}
		final ITypeBinding typeBinding = node.getExpression()
				.resolveTypeBinding();
		if ("compareTo".equals(node.getName().getIdentifier())) {
			if (ASTHelper.instanceOf(typeBinding, "java.lang.Comparable")
					&& node.arguments().size() == 1) {
				replaceInfixExpressionIfNeeded(node.getParent());
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			} else if (ASTHelper
					.instanceOf(typeBinding, "java.lang.Comparator")
					&& node.arguments().size() == 2) {
				replaceInfixExpressionIfNeeded(node.getParent());
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		} else if (javaMinorVersion >= 2
				&& "compareToIgnoreCase".equals(node.getName().getIdentifier())
				&& ASTHelper.instanceOf(typeBinding, "java.lang.String")
				&& node.arguments().size() == 1) {
			replaceInfixExpressionIfNeeded(node.getParent());
			return ASTHelper.DO_NOT_VISIT_SUBTREE;
		} else {
			ThisExpression te = ASTHelper.as(node.getExpression(),
					ThisExpression.class);
			if (te != null
					&& ASTHelper.thisExpressionRefersToCurrentType(
							te.getQualifier(), node)) {
				// remove useless thisExpressions
				this.refactorings.remove(node.getExpression());
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private void replaceInfixExpressionIfNeeded(ASTNode expr) {
		if (expr instanceof ParenthesizedExpression) {
			replaceInfixExpressionIfNeeded(expr.getParent());
		} else if (expr instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) expr;
			final Object value = ie.getRightOperand()
					.resolveConstantExpressionValue();
			if (value instanceof Number) {
				final Number nb = (Integer) value;
				if (nb.doubleValue() == 0) {
					return;
				}
				if (InfixExpression.Operator.EQUALS.equals(ie.getOperator())) {
					if (nb.doubleValue() < 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(InfixExpression.Operator.LESS);
						this.refactorings.replace(ie, newIe);
					} else if (nb.doubleValue() > 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(InfixExpression.Operator.GREATER);
						this.refactorings.replace(ie, newIe);
					}
				} else if (InfixExpression.Operator.NOT_EQUALS.equals(ie
						.getOperator())) {
					if (nb.doubleValue() < 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(InfixExpression.Operator.GREATER_EQUALS);
						this.refactorings.replace(ie, newIe);
					} else if (nb.doubleValue() > 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(InfixExpression.Operator.LESS_EQUALS);
						this.refactorings.replace(ie, newIe);
					}
				}
			}
		}
	}

	private InfixExpression getNewInfixExpression(final InfixExpression ie) {
		final InfixExpression newIe = this.ast.newInfixExpression();
		newIe.setLeftOperand(ASTHelper.copySubtree(this.ast,
				ie.getLeftOperand()));
		newIe.setRightOperand(this.ast.newNumberLiteral("0"));
		return newIe;
	}

	@Override
	public boolean visit(InfixExpression node) {
		final Expression lhs = node.getLeftOperand();
		final Expression rhs = node.getRightOperand();
		final Operator operator = node.getOperator();
		final Object lhsConstantValue = lhs.resolveConstantExpressionValue();
		if (Operator.CONDITIONAL_OR.equals(operator)) {
			if (Boolean.TRUE.equals(lhsConstantValue)) {
				replaceByCopy(node, lhs);
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			} else if (Boolean.FALSE.equals(lhsConstantValue)) {
				replaceByCopy(node, rhs);
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		} else if (Operator.CONDITIONAL_AND.equals(operator)) {
			if (Boolean.TRUE.equals(lhsConstantValue)) {
				replaceByCopy(node, rhs);
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			} else if (Boolean.FALSE.equals(lhsConstantValue)) {
				replaceByCopy(node, lhs);
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			} else {
				Expression nullCheckedExpression = getNullCheckedExpression(lhs);
				if (nullCheckedExpression != null) {
					if (isNullCheckRedundant(rhs, nullCheckedExpression)) {
						replaceByCopy(node, rhs);
						return ASTHelper.DO_NOT_VISIT_SUBTREE;
					}
				} else {
					nullCheckedExpression = getNullCheckedExpression(rhs);
					if (isNullCheckRedundant(lhs, nullCheckedExpression)) {
						replaceByCopy(node, lhs);
						return ASTHelper.DO_NOT_VISIT_SUBTREE;
					}
				}
			}
		} else if (Operator.EQUALS.equals(operator)) {
			final Boolean blo = ASTHelper.getBooleanLiteral(node
					.getLeftOperand());
			final Boolean bro = ASTHelper.getBooleanLiteral(node
					.getRightOperand());
			if (blo != null) {
				replace(node, blo.booleanValue(), node.getRightOperand());
			} else if (bro != null) {
				replace(node, bro.booleanValue(), node.getLeftOperand());
			}
		} else if (Operator.NOT_EQUALS.equals(operator)) {
			final Boolean blo = ASTHelper.getBooleanLiteral(node
					.getLeftOperand());
			final Boolean bro = ASTHelper.getBooleanLiteral(node
					.getRightOperand());
			if (blo != null) {
				replace(node, !blo.booleanValue(), node.getRightOperand());
			} else if (bro != null) {
				replace(node, !bro.booleanValue(), node.getLeftOperand());
			}
		}

		return ASTHelper.VISIT_SUBTREE;
	}

	private boolean isPrimitiveBool(Expression expr) {
		ITypeBinding typeBinding = expr.resolveTypeBinding();
		return typeBinding.isPrimitive()
				&& "boolean".equals(typeBinding.getQualifiedName());
	}

	private void replace(InfixExpression node, boolean negate,
			Expression exprToCopy) {
		if (!isPrimitiveBool(node.getLeftOperand())
				&& !isPrimitiveBool(node.getRightOperand())) {
			return;
		}
		// Either:
		// - Two boolean primitives: no possible NPE
		// - One boolean primitive and one Boolean object, this code already run
		// the risk of an NPE, so we can replace the infix expression without
		// fearing we would introduce a previously non existing NPE.
		Expression operand;
		if (negate) {
			operand = ASTHelper.copySubtree(this.ast, exprToCopy);
		} else {
			operand = negate(exprToCopy);
		}
		this.refactorings.replace(node, operand);
	}

	private Expression negate(Expression expr) {
		if (expr instanceof PrefixExpression) {
			final PrefixExpression pe = (PrefixExpression) expr;
			if (PrefixExpression.Operator.NOT.equals(pe.getOperator())) {
				return ASTHelper.copySubtree(this.ast, pe.getOperand());
			}
		}
		final PrefixExpression pe = this.ast.newPrefixExpression();
		pe.setOperator(PrefixExpression.Operator.NOT);
		pe.setOperand(ASTHelper.copySubtree(this.ast, expr));
		return pe;
	}

	private void replaceByCopy(InfixExpression node, final Expression lhs) {
		this.refactorings.replace(node, ASTHelper.copySubtree(this.ast, lhs));
	}

	/**
	 * The previous null check is redundant if:
	 * <ul>
	 * <li>the null checked expression is reused in an instanceof expression</li>
	 * <li>the null checked expression is reused in an expression checking for
	 * object equality against an expression that resolves to a non null
	 * constant</li>
	 * </ul>
	 *
	 * @param e
	 * @param nullCheckedExpression
	 * @return
	 */
	private boolean isNullCheckRedundant(Expression e,
			Expression nullCheckedExpression) {
		if (nullCheckedExpression == null) {
			return false;
		} else if (e instanceof InstanceofExpression) {
			return ((InstanceofExpression) e).getLeftOperand().subtreeMatch(
					new ASTMatcher(), nullCheckedExpression);
		} else if (e instanceof MethodInvocation) {
			final MethodInvocation expr = (MethodInvocation) e;
			if (expr.getExpression() != null
					&& expr.getExpression().resolveConstantExpressionValue() != null
					&& expr.arguments().size() == 1
					&& ((Expression) expr.arguments().get(0)).subtreeMatch(
							new ASTMatcher(), nullCheckedExpression)) {
				// Did we invoke java.lang.Object.equals()?
				// Did we invoke java.lang.String.equalsIgnoreCase()?
				return "equals".equals(expr.getName().getIdentifier())
						|| (ASTHelper.hasType(expr.getExpression(),
								"java.lang.String") && "equalsIgnoreCase"
								.equals(expr.getName().getIdentifier()));
			}
		}
		return false;
	}

	private Expression getNullCheckedExpression(Expression e) {
		if (e instanceof InfixExpression) {
			final InfixExpression expr = (InfixExpression) e;
			if (Operator.NOT_EQUALS.equals(expr.getOperator())) {
				if (expr.getLeftOperand() instanceof NullLiteral) {
					return expr.getRightOperand();
				} else if (expr.getRightOperand() instanceof NullLiteral) {
					return expr.getLeftOperand();
				}
			}
		}
		return null;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.refactorings;
	}
}
