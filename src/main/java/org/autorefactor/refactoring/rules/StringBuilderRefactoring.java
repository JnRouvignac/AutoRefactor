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

import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.StringLiteral;

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
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(InfixExpression node) {
		if (Operator.PLUS.equals(node.getOperator())
				&& "".equals(node.getRightOperand().resolveConstantExpressionValue())
				&& ASTHelper.hasType(node.getLeftOperand(), "java.lang.String")) {
			Expression newE = ASTHelper.copySubtree(this.ctx.getAST(), node.getLeftOperand());
			this.ctx.getRefactorings().replace(node, newE);
			return ASTHelper.DO_NOT_VISIT_SUBTREE;
		}
		return super.visit(node);
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null) {
			return ASTHelper.VISIT_SUBTREE;
		}
		final ITypeBinding typeBinding = node.getExpression()
				.resolveTypeBinding();
		if ("append".equals(node.getName().getIdentifier())
				&& node.arguments().size() == 1
				// most expensive check comes last
				&& ASTHelper.instanceOf(typeBinding, "java.lang.Appendable")) {
			final LinkedList<Expression> allAppendedStrings = new LinkedList<Expression>();
			final Expression lastExpr = collectAllAppendedStrings(node,
					allAppendedStrings);
			if (lastExpr instanceof Name || lastExpr instanceof FieldAccess) {
				// TODO do EfficientAppendableUse
				// TODO if content is equal to 0 + "", then drop ""
				this.ctx.getRefactorings().replace(node,
						createStringAppends(lastExpr, allAppendedStrings));
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		} else if ("toString".equals(node.getName().getIdentifier())
				&& node.arguments().isEmpty()
				&& ASTHelper.hasType(node.getExpression(),
						"java.lang.StringBuilder", "java.lang.StringBuffer")) {
			final LinkedList<Expression> allAppendedStrings = new LinkedList<Expression>();
			final Expression lastExpr = collectAllAppendedStrings(
					node.getExpression(), allAppendedStrings);
			// TODO new StringBuffer().append(" bla").append("bla").toString();
			// outputs " blabla"
			if (lastExpr instanceof ClassInstanceCreation) {
				this.ctx.getRefactorings().replace(node,
						createStringAdds(allAppendedStrings));
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private ASTNode createStringAppends(Expression lastExpr,
			List<Expression> appendedStrings) {
		Expression result = lastExpr;
		for (Expression expr : appendedStrings) {
			if (result == null) {
				result = expr;
			} else {
				final MethodInvocation mi = this.ctx.getAST().newMethodInvocation();
				mi.setExpression(result);
				mi.setName(this.ctx.getAST().newSimpleName("append"));
				mi.arguments().add(expr);
				result = mi;
			}
		}
		return result;
	}

	private Expression createStringAdds(List<Expression> appendedStrings) {
		Expression result = null;
		for (Expression expr : appendedStrings) {
			if (result == null) {
				result = expr;
			} else {
				final InfixExpression ie = this.ctx.getAST().newInfixExpression();
				ie.setLeftOperand(result);
				ie.setOperator(Operator.PLUS);
				ie.setRightOperand(expr);
				result = ie;
			}
		}
		return result;
	}

	private Expression collectAllAppendedStrings(Expression expr,
			final LinkedList<Expression> results) {
		if (ASTHelper.hasType(expr, "java.lang.StringBuilder",
				"java.lang.StringBuffer")) {
			if (expr instanceof MethodInvocation) {
				final MethodInvocation mi = (MethodInvocation) expr;
				if ("append".equals(mi.getName().getIdentifier())
						&& mi.arguments().size() == 1) {
					final Expression arg = (Expression) mi.arguments().get(0);
					addAllAppendedString(arg, results);
					return collectAllAppendedStrings(mi.getExpression(),
							results);
				}
			} else if (expr instanceof ClassInstanceCreation) {
				final ClassInstanceCreation cic = (ClassInstanceCreation) expr;
				if (cic.arguments().size() == 1) {
					final Expression arg = (Expression) cic.arguments().get(0);
					if (ASTHelper.hasType(arg, "java.lang.String")
							|| ASTHelper.instanceOf(arg.resolveTypeBinding(),
									"java.lang.CharSequence")) {
						results.addFirst(ASTHelper.copySubtree(this.ctx.getAST(), arg));
					}
				}
				return ASTHelper.copySubtree(this.ctx.getAST(), cic);
			} else if (expr instanceof Name || expr instanceof FieldAccess) {
				return ASTHelper.copySubtree(this.ctx.getAST(), expr);
			}
		}
		return null;
	}

	private void addAllAppendedString(final Expression arg,
			final LinkedList<Expression> results) {
		if (arg instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) arg;
			if (InfixExpression.Operator.PLUS.equals(ie.getOperator())) {
				addAllAppendedString(ie.getRightOperand(), results);
				addAllAppendedString(ie.getLeftOperand(), results);
				return;
			}
		}
		if (arg instanceof StringLiteral) {
			final StringLiteral sl = (StringLiteral) arg;
			if ("".equals(sl.getLiteralValue())) {
				return;
			}
		}
		results.addFirst(ASTHelper.copySubtree(this.ctx.getAST(), arg));
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
