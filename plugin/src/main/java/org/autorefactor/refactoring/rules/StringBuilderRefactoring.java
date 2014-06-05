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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.*;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * StringBuilder related refactorings:
 * <ul>
 * <li>StringBuffer to StringBuilder conversions</li>
 * <li>Remove String appends using operator '+' as parameters of
 * StringBuffer/StringBuilder.append()</li>
 * <li>Replace calls to StringBuffer/StringBuilder constructor + calls to
 * append() + calls toString() with straight String concatenation with operator
 * '+'</li>
 * </ul>
 */
public class StringBuilderRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private static final class BooleanHolder {

		private boolean value;

		public BooleanHolder(boolean defaultValue) {
			this.value = defaultValue;
		}
	}

	private RefactoringContext ctx;
	private int javaMinorVersion;

	public StringBuilderRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
		this.javaMinorVersion = this.ctx.getJavaSERelease().getMinorVersion();
	}

	@Override
	public boolean visit(ClassInstanceCreation node) {
		final ITypeBinding typeBinding = node.getType().resolveBinding();
		if (this.javaMinorVersion >= 5 && typeBinding != null) {
			if ("java.lang.StringBuffer".equals(typeBinding.getQualifiedName())) {
				// TODO JNR replace with StringBuilder
				// check that the current method return type is not StringBuffer
				// do we need the CFG + live variable analysis first?
			}
		}
		return VISIT_SUBTREE;
	}

	@Override
	public boolean visit(InfixExpression node) {
		// TODO JNR also remove valueOf() methods in these cases, etc.:
		// String s = "" + String.valueOf(1);
		// String s = "" + Integer.toString(1);
		// String s = "" + Long.toString(1);
		if (Operator.PLUS.equals(node.getOperator())
				&& hasType(node, "java.lang.String")) {
			final LinkedList<Expression> allOperands = new LinkedList<Expression>();
			addAllSubExpressions(node, allOperands, null);
			boolean replaceNeeded = filterOutEmptyStringsFromStringConcat(allOperands);
			if (replaceNeeded) {
				this.ctx.getRefactorings().replace(node, createStringConcats(allOperands));
				return DO_NOT_VISIT_SUBTREE;
			}
			// FIXME In theory commented code down below should work better than current code above
			// (preserving comments, etc.), but in practice it does not work at all.
			// for (Expression operand : allOperands) {
			// if ("".equals(operand.resolveConstantExpressionValue())) {
			// this.ctx.getRefactorings().remove(operand);
			// }
			// }
		}
		return VISIT_SUBTREE;
	}

	private boolean filterOutEmptyStringsFromStringConcat(List<Expression> allOperands) {
		boolean replaceNeeded = false;
		boolean canRemoveEmptyStrings = false;
		for (int i = 0; i < allOperands.size(); i++) {
			Expression expr = allOperands.get(i);
			boolean canNowRemoveEmptyStrings = canRemoveEmptyStrings || hasType(expr, "java.lang.String");
			if (isEmptyString(expr)) {
				boolean removeExpr = false;
				if (canRemoveEmptyStrings) {
					removeExpr = true;
				} else if (canNowRemoveEmptyStrings
						&& i + 1 < allOperands.size()
						&& hasType(allOperands.get(i + 1), "java.lang.String")) {
					removeExpr = true;
				}

				if (removeExpr) {
					allOperands.remove(i);
					replaceNeeded = true;
				}
			}
			canRemoveEmptyStrings = canNowRemoveEmptyStrings;
		}
		return replaceNeeded;
	}

	private boolean isEmptyString(Expression expr) {
		return "".equals(expr.resolveConstantExpressionValue())
				// Due to a bug with ASTNode.resolveConstantExpressionValue()
				// in Eclipse 3.7.2 and 3.8.0, this second check is necessary
				|| (expr instanceof StringLiteral
						&& "".equals(((StringLiteral) expr).getLiteralValue()));
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null) {
			return VISIT_SUBTREE;
		}
		final ITypeBinding typeBinding = node.getExpression().resolveTypeBinding();
		if ("append".equals(node.getName().getIdentifier())
				&& node.arguments().size() == 1
				// most expensive check comes last
				&& instanceOf(typeBinding, "java.lang.Appendable")) {
			final LinkedList<Expression> allAppendedStrings = new LinkedList<Expression>();
			final BooleanHolder foundInfixExpr = new BooleanHolder(false);
			final Expression lastExpr = collectAllAppendedStrings(node,
					allAppendedStrings, foundInfixExpr);
			if (lastExpr instanceof Name || lastExpr instanceof FieldAccess) {
				boolean rewriteNeeded = filterOutEmptyStrings(allAppendedStrings);
				if (rewriteNeeded || foundInfixExpr.value) {
					// rewrite the successive calls to append() on an Appendable
					this.ctx.getRefactorings().replace(node,
							createStringAppends(lastExpr, allAppendedStrings));
					return DO_NOT_VISIT_SUBTREE;
				}
			}
		} else if (isMethod(node, "java.lang.StringBuilder", "toString")
				|| isMethod(node, "java.lang.StringBuffer", "toString")) {
			final LinkedList<Expression> allAppendedStrings = new LinkedList<Expression>();
			final Expression lastExpr = collectAllAppendedStrings(
					node.getExpression(), allAppendedStrings, null);
			// TODO new StringBuffer().append(" bla").append("bla").toString();
			// outputs " blabla"
			if (lastExpr instanceof ClassInstanceCreation) {
				// replace with String concatenation
				this.ctx.getRefactorings().replace(node,
						createStringConcats(allAppendedStrings));
				return DO_NOT_VISIT_SUBTREE;
			}
		}
		return VISIT_SUBTREE;
	}

	private boolean filterOutEmptyStrings(List<Expression> allExprs) {
		boolean result = false;
		for (Iterator<Expression> iter = allExprs.iterator(); iter.hasNext();) {
			Expression expr = iter.next();
			if (isEmptyString(expr)) {
				iter.remove();
				result = true;
			}
		}
		return result;
	}

	private ASTNode createStringAppends(Expression lastExpr, List<Expression> appendedStrings) {
		Expression result = lastExpr;
		for (Expression expr : appendedStrings) {
			if (result == null) {
				result = expr;
			} else {
				final MethodInvocation mi = this.ctx.getAST().newMethodInvocation();
				mi.setExpression(result);
				mi.setName(this.ctx.getAST().newSimpleName("append"));
				mi.arguments().add(copy(expr));
				result = mi;
			}
		}
		return result;
	}

	private Expression createStringConcats(List<Expression> appendedStrings) {
		if (appendedStrings.size() == 0) {
			throw new NotImplementedException("when there are no appended strings");
		} else if (appendedStrings.size() == 1) {
			return appendedStrings.get(0);
		}

		final Iterator<Expression> it = appendedStrings.iterator();
		final InfixExpression ie = this.ctx.getAST().newInfixExpression();
		ie.setLeftOperand(copy(it.next()));
		ie.setOperator(Operator.PLUS);
		ie.setRightOperand(copy(it.next()));
		while (it.hasNext()) {
			ie.extendedOperands().add(copy(it.next()));
		}
		return ie;
	}

	private Expression collectAllAppendedStrings(Expression expr,
			final LinkedList<Expression> allOperands, BooleanHolder foundInfixExpr) {
		if (instanceOf(expr, "java.lang.Appendable")) {
			if (expr instanceof MethodInvocation) {
				final MethodInvocation mi = (MethodInvocation) expr;
				if ("append".equals(mi.getName().getIdentifier())
						&& mi.arguments().size() == 1) {
					final Expression arg = (Expression) mi.arguments().get(0);
					addAllSubExpressions(arg, allOperands, foundInfixExpr);
					return collectAllAppendedStrings(mi.getExpression(), allOperands,
							foundInfixExpr);
				}
			} else if (expr instanceof ClassInstanceCreation) {
				final ClassInstanceCreation cic = (ClassInstanceCreation) expr;
				if (cic.arguments().size() == 1) {
					final Expression arg = (Expression) cic.arguments().get(0);
					if (hasType(arg, "java.lang.String")
							|| instanceOf(arg, "java.lang.CharSequence")) {
						allOperands.addFirst(copy(arg));
					}
				}
				return copy(cic);
			} else if (expr instanceof Name || expr instanceof FieldAccess) {
				return copy(expr);
			}
		}
		return null;
	}

	private <T> T copy(T node) {
		return copySubtree(this.ctx.getAST(), node);
	}

	private void addAllSubExpressions(final Expression arg, final LinkedList<Expression> results,
			final BooleanHolder foundInfixExpr) {
		if (arg instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) arg;
			if (InfixExpression.Operator.PLUS.equals(ie.getOperator())) {
				if (ie.hasExtendedOperands()) {
					final List<Expression> reversed =
							new ArrayList<Expression>(ie.extendedOperands());
					Collections.reverse(reversed);
					for (Expression op : reversed) {
						addAllSubExpressions(op, results, foundInfixExpr);
					}
				}
				addAllSubExpressions(ie.getRightOperand(), results, foundInfixExpr);
				addAllSubExpressions(ie.getLeftOperand(), results, foundInfixExpr);
				if (foundInfixExpr != null) {
					foundInfixExpr.value = true;
				}
				return;
			}
		}
		results.addFirst(arg);
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
