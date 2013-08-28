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
import org.autorefactor.util.NotImplementedException;
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
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.WhileStatement;

import static org.autorefactor.refactoring.ASTHelper.*;

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

	private RefactoringContext ctx;
	private int javaMinorVersion;
	private final boolean removeThisForNonStaticMethodAccess;

	public SimplifyExpressionRefactoring(boolean removeThisForNonStaticMethodAccess) {
		this.removeThisForNonStaticMethodAccess = removeThisForNonStaticMethodAccess;
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
		this.javaMinorVersion = this.ctx.getJavaSERelease().getMinorVersion();
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
			final Expression innerExprCopy = ASTHelper.copySubtree(this.ctx.getAST(),
					innerExpr);
			this.ctx.getRefactorings().replace(node, innerExprCopy);
			innerExprCopy.accept(this);
			return DO_NOT_VISIT_SUBTREE;
		}
		return VISIT_SUBTREE;
	}

	private Expression getExpressionWithoutParentheses(ParenthesizedExpression node) {
		final ASTNode parent = node.getParent();
		final Expression innerExpr = node.getExpression();
		if (innerExpr instanceof ParenthesizedExpression) {
			return getExpressionWithoutParentheses((ParenthesizedExpression) innerExpr);
		}
		if (parent instanceof InfixExpression) {
			final InfixExpression parentInfixExpr = (InfixExpression) parent;
			if (innerExpr instanceof InfixExpression) {
				final Operator innerOp = ((InfixExpression) innerExpr).getOperator();
				if (innerOp == parentInfixExpr.getOperator()
						&& OperatorEnum.isAssociative(innerOp)
						// Leave String concatenations with mixed type
						// to other if statements in this method.
						&& innerExpr.resolveTypeBinding() != null
						&& innerExpr.resolveTypeBinding().equals(
								parentInfixExpr.resolveTypeBinding())) {
					return innerExpr;
				}
			}
		}
		if (isHardToRead(innerExpr, parent)) {
			return node;
		}
		if (isUselessParenthesesInStatement(parent, node)) {
			return innerExpr;
		}
		final int compareTo = OperatorEnum.compareTo(innerExpr, parent);
		if (compareTo < 0) {
			return node;
		} else if (compareTo > 0) {
			return innerExpr;
		}
		if (
		// TODO JNR can we revert the condition in the InfixExpression?
		// parentheses are sometimes needed to explicit code,
		// some like it like that
		innerExpr instanceof InfixExpression
		// TODO JNR add additional code to check if the cast is really required
		// or if it can be removed.
				|| innerExpr instanceof CastExpression) {
			return node;
		}
		return innerExpr;
	}

	/**
	 * Returns whether the supplied expression is complex enough to read.
	 *
	 * @param innerExpr
	 *          the inner expression to test for ease of read
	 * @param parent
	 *          the parent node to test for ease of read
	 * @return true if the expressions is hard to read, false otherwise
	 */
	private boolean isHardToRead(final Expression innerExpr, final ASTNode parent) {
		if (parent instanceof InfixExpression) {
			if (innerExpr instanceof InfixExpression) {
				final Operator innerOp = ((InfixExpression) innerExpr).getOperator();
				final Operator parentOp = ((InfixExpression) parent).getOperator();
				return Operator.EQUALS.equals(parentOp)
					|| (Operator.CONDITIONAL_AND.equals(innerOp) && Operator.CONDITIONAL_OR.equals(parentOp))
					|| (Operator.AND.equals(innerOp) && Operator.OR.equals(parentOp))
					|| (Operator.AND.equals(innerOp) && Operator.XOR.equals(parentOp))
					|| (Operator.XOR.equals(innerOp) && Operator.OR.equals(parentOp));
			}
		} else if (parent instanceof ConditionalExpression) {
			return innerExpr instanceof ConditionalExpression
				|| innerExpr instanceof Assignment
				|| innerExpr instanceof InstanceofExpression;
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

	@Override
	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null) {
			// TODO JNR handle same class calls and sub classes
			return VISIT_SUBTREE;
		}
		final ITypeBinding typeBinding = node.getExpression()
				.resolveTypeBinding();
		if ("compareTo".equals(node.getName().getIdentifier())) {
			if (ASTHelper.instanceOf(typeBinding, "java.lang.Comparable")
					&& node.arguments().size() == 1) {
				replaceInfixExpressionIfNeeded(node.getParent());
				return DO_NOT_VISIT_SUBTREE;
			} else if (ASTHelper.instanceOf(typeBinding, "java.lang.Comparator")
					&& node.arguments().size() == 2) {
				replaceInfixExpressionIfNeeded(node.getParent());
				return DO_NOT_VISIT_SUBTREE;
			}
		} else if (javaMinorVersion >= 2
				&& "compareToIgnoreCase".equals(node.getName().getIdentifier())
				&& ASTHelper.instanceOf(typeBinding, "java.lang.String")
				&& node.arguments().size() == 1) {
			replaceInfixExpressionIfNeeded(node.getParent());
			return DO_NOT_VISIT_SUBTREE;
		} else if (this.removeThisForNonStaticMethodAccess) {
			ThisExpression te = ASTHelper.as(node.getExpression(),
					ThisExpression.class);
			if (te != null
					&& ASTHelper.thisExpressionRefersToCurrentType(
							te.getQualifier(), node)) {
				// remove useless thisExpressions
				this.ctx.getRefactorings().remove(node.getExpression());
				return DO_NOT_VISIT_SUBTREE;
			}
		}
		return VISIT_SUBTREE;
	}

	private void replaceInfixExpressionIfNeeded(ASTNode expr) {
		if (expr instanceof ParenthesizedExpression) {
			replaceInfixExpressionIfNeeded(expr.getParent());
		} else if (expr instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) expr;
			checkNoExtendedOperands(ie);
			final Object value = ie.getRightOperand()
					.resolveConstantExpressionValue();
			if (value instanceof Number) {
				final Number nb = (Integer) value;
				if (nb.doubleValue() == 0) {
					return;
				}
				if (Operator.EQUALS.equals(ie.getOperator())) {
					if (nb.doubleValue() < 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(Operator.LESS);
						this.ctx.getRefactorings().replace(ie, newIe);
					} else if (nb.doubleValue() > 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(Operator.GREATER);
						this.ctx.getRefactorings().replace(ie, newIe);
					}
				} else if (Operator.NOT_EQUALS.equals(ie.getOperator())) {
					if (nb.doubleValue() < 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(Operator.GREATER_EQUALS);
						this.ctx.getRefactorings().replace(ie, newIe);
					} else if (nb.doubleValue() > 0) {
						final InfixExpression newIe = getNewInfixExpression(ie);
						newIe.setOperator(Operator.LESS_EQUALS);
						this.ctx.getRefactorings().replace(ie, newIe);
					}
				}
			}
		}
	}

	private InfixExpression getNewInfixExpression(final InfixExpression ie) {
		final AST ast = this.ctx.getAST();
		final InfixExpression newIe = ast.newInfixExpression();
		newIe.setLeftOperand(ASTHelper.copySubtree(ast, ie.getLeftOperand()));
		newIe.setRightOperand(ast.newNumberLiteral("0"));
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
				return DO_NOT_VISIT_SUBTREE;
			} else if (Boolean.FALSE.equals(lhsConstantValue)) {
				checkNoExtendedOperands(node);
				replaceByCopy(node, rhs);
				return DO_NOT_VISIT_SUBTREE;
			}
		} else if (Operator.CONDITIONAL_AND.equals(operator)) {
			if (Boolean.TRUE.equals(lhsConstantValue)) {
				checkNoExtendedOperands(node);
				replaceByCopy(node, rhs);
				return DO_NOT_VISIT_SUBTREE;
			} else if (Boolean.FALSE.equals(lhsConstantValue)) {
				replaceByCopy(node, lhs);
				return DO_NOT_VISIT_SUBTREE;
			} else {
				Expression nullCheckedExpression = getNullCheckedExpression(lhs);
				if (nullCheckedExpression != null) {
					if (isNullCheckRedundant(rhs, nullCheckedExpression)) {
						checkNoExtendedOperands(node);
						replaceByCopy(node, rhs);
						return DO_NOT_VISIT_SUBTREE;
					}
				} else {
					nullCheckedExpression = getNullCheckedExpression(rhs);
					if (isNullCheckRedundant(lhs, nullCheckedExpression)) {
						replaceByCopy(node, lhs);
						return DO_NOT_VISIT_SUBTREE;
					}
				}
			}
		} else if (Operator.EQUALS.equals(operator)) {
			boolean result = VISIT_SUBTREE;
			final Boolean blo = ASTHelper.getBooleanLiteral(lhs);
			final Boolean bro = ASTHelper.getBooleanLiteral(rhs);
			if (blo != null) {
				checkNoExtendedOperands(node);
				result = replace(node, blo.booleanValue(), rhs);
			} else if (bro != null) {
				result = replace(node, bro.booleanValue(), lhs);
			}
			if (result == DO_NOT_VISIT_SUBTREE) {
				return DO_NOT_VISIT_SUBTREE;
			}
		} else if (Operator.NOT_EQUALS.equals(operator)) {
			boolean continueVisit = VISIT_SUBTREE;
			final Boolean blo = ASTHelper.getBooleanLiteral(lhs);
			final Boolean bro = ASTHelper.getBooleanLiteral(rhs);
			if (blo != null) {
				checkNoExtendedOperands(node);
				continueVisit = replace(node, !blo.booleanValue(), rhs);
			} else if (bro != null) {
				continueVisit = replace(node, !bro.booleanValue(), lhs);
			}
			if (!continueVisit) {
				return DO_NOT_VISIT_SUBTREE;
			}
		}

		if (shouldAddParentheses(node, Operator.CONDITIONAL_AND, Operator.CONDITIONAL_OR)
				|| shouldAddParentheses(node, Operator.AND, Operator.XOR)
				|| shouldAddParentheses(node, Operator.AND, Operator.OR)
				|| shouldAddParentheses(node, Operator.XOR, Operator.OR)) {
			addParentheses(node);
			return DO_NOT_VISIT_SUBTREE;
		}
		return VISIT_SUBTREE;
	}

	private boolean shouldAddParentheses(InfixExpression node, Operator opNode, Operator opParent) {
		final Operator operator = node.getOperator();
		if (opNode.equals(operator)) {
			if (node.getParent() instanceof InfixExpression) {
				InfixExpression ie = (InfixExpression) node.getParent();
				return opParent.equals(ie.getOperator());
			}
		}
		return false;
	}

	private void addParentheses(Expression e) {
		final AST ast = this.ctx.getAST();
		final ParenthesizedExpression pe = ast.newParenthesizedExpression();
		pe.setExpression(ASTHelper.copySubtree(ast, e));
		this.ctx.getRefactorings().replace(e, pe);
	}

	/**
	 * Extended operands are used for deeply nested expressions, mostly string concatenation expressions.
	 * <p>
	 * This will be implemented only if somebody comes up with code where the runtime exception is thrown.
	 * </p>
	 *
	 * @see <a href="http://publib.boulder.ibm.com/infocenter/iadthelp/v6r0/topic/org.eclipse.jdt.doc.isv/reference/api/org/eclipse/jdt/core/dom/InfixExpression.html#extendedOperands()">
	 */
	private void checkNoExtendedOperands(InfixExpression node) {
		if (!hasType(node, "java.lang.String") && !node.extendedOperands().isEmpty()) {
			throw new NotImplementedException("for extended operands");
		}
	}

	private boolean isPrimitiveBool(Expression expr) {
		ITypeBinding typeBinding = expr.resolveTypeBinding();
		return typeBinding.isPrimitive()
				&& "boolean".equals(typeBinding.getQualifiedName());
	}

	private boolean replace(InfixExpression node, boolean negate,
			Expression exprToCopy) {
		checkNoExtendedOperands(node);
		if (!isPrimitiveBool(node.getLeftOperand())
				&& !isPrimitiveBool(node.getRightOperand())) {
			return VISIT_SUBTREE;
		}
		// Either:
		// - Two boolean primitives: no possible NPE
		// - One boolean primitive and one Boolean object, this code already run
		// the risk of an NPE, so we can replace the infix expression without
		// fearing we would introduce a previously non existing NPE.
		Expression operand;
		if (negate) {
			operand = ASTHelper.copySubtree(this.ctx.getAST(), exprToCopy);
		} else {
			operand = ASTHelper.negate(this.ctx.getAST(), exprToCopy, true);
		}
		this.ctx.getRefactorings().replace(node, operand);
		return DO_NOT_VISIT_SUBTREE;
	}

	private void replaceByCopy(InfixExpression node, final Expression expr) {
		this.ctx.getRefactorings().replace(node, ASTHelper.copySubtree(this.ctx.getAST(), expr));
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
	private boolean isNullCheckRedundant(Expression e, Expression nullCheckedExpression) {
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
						|| (hasType(expr.getExpression(), "java.lang.String")
							&& "equalsIgnoreCase".equals(expr.getName().getIdentifier()));
			}
		}
		return false;
	}

	private Expression getNullCheckedExpression(Expression e) {
		if (e instanceof InfixExpression) {
			final InfixExpression expr = (InfixExpression) e;
			checkNoExtendedOperands(expr);
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
		return this.ctx.getRefactorings();
	}
}
