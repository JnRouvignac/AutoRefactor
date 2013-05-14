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

import java.math.BigDecimal;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.StringLiteral;

/**
 * Refactors to a proper use of BigDecimals:
 * <ul>
 * <li>Use Strings with floating point values</li>
 * <li>Use integer constructors with integers</li>
 * <li>Replace calls to {@link BigDecimal#equals(Object)} with calls to
 * {@link BigDecimal#compareTo(BigDecimal)}</li>
 * </ul>
 */
public class BigDecimalRefactorings extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;
	private int javaMinorVersion;

	public BigDecimalRefactorings() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
		this.javaMinorVersion = this.ctx.getJavaSERelease().getMinorVersion();
	}

	@Override
	public boolean visit(ClassInstanceCreation node) {
		final ITypeBinding typeBinding = node.getType().resolveBinding();
		if (typeBinding != null
				&& "java.math.BigDecimal"
						.equals(typeBinding.getQualifiedName())
				&& node.arguments().size() == 1) {
			final Expression arg = (Expression) node.arguments().get(0);
			if (arg instanceof NumberLiteral) {
				final NumberLiteral nb = (NumberLiteral) arg;
				if (nb.getToken().contains(".")) {
					// Only instantiation from double, not from integer
					this.ctx.getRefactorings().replace(nb,
							getStringLiteral(nb.getToken()));
				} else {
					if (javaMinorVersion < 5) {
						return ASTHelper.VISIT_SUBTREE;
					}
					if ("0".equals(nb.getToken())) {
						replaceWithQualifiedName(node, typeBinding.getName(),
								"ZERO");
					} else if ("1".equals(nb.getToken())) {
						replaceWithQualifiedName(node, typeBinding.getName(),
								"ONE");
					} else if ("10".equals(nb.getToken())) {
						replaceWithQualifiedName(node, typeBinding.getName(),
								"TEN");
					} else {
						this.ctx.getRefactorings().replace(node,
								getValueOf(typeBinding.getName(), nb));
					}
				}
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			} else if (arg instanceof StringLiteral) {
				if (javaMinorVersion < 5) {
					return ASTHelper.VISIT_SUBTREE;
				}
				final String literalValue = ((StringLiteral) arg)
						.getLiteralValue();
				if ("0".equals(literalValue)) {
					replaceWithQualifiedName(node, typeBinding.getName(),
							"ZERO");
				} else if ("1".equals(literalValue)) {
					replaceWithQualifiedName(node, typeBinding.getName(), "ONE");
				} else if ("10".equals(literalValue)) {
					replaceWithQualifiedName(node, typeBinding.getName(), "TEN");
				} else if (literalValue.matches("\\d+")) {
					final NumberLiteral nb = this.ctx.getAST()
							.newNumberLiteral(literalValue);
					this.ctx.getRefactorings().replace(node,
							getValueOf(typeBinding.getName(), nb));
				}
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private void replaceWithQualifiedName(ASTNode node, String className,
			String field) {
		this.ctx.getRefactorings().replace(node,
				this.ctx.getAST().newName(new String[] { className, field }));
	}

	private ASTNode getValueOf(String name, NumberLiteral nb) {
		final AST ast = this.ctx.getAST();
		final MethodInvocation mi = ast.newMethodInvocation();
		mi.setExpression(ast.newName(name));
		mi.setName(ast.newSimpleName("valueOf"));
		mi.arguments().add(ASTHelper.copySubtree(ast, nb));
		return mi;
	}

	private StringLiteral getStringLiteral(String numberLiteral) {
		final StringLiteral sl = this.ctx.getAST().newStringLiteral();
		sl.setLiteralValue(numberLiteral);
		return sl;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null) {
			return ASTHelper.VISIT_SUBTREE;
		}
		if (ASTHelper.hasType(node.getExpression(), "java.math.BigDecimal")) {
			if (javaMinorVersion >= 5
					&& "valueOf".equals(node.getName().getIdentifier())
					&& node.arguments().size() == 1) {
				final ITypeBinding typeBinding = node.getExpression()
						.resolveTypeBinding();
				final Expression arg = (Expression) node.arguments().get(0);
				if (arg instanceof NumberLiteral) {
					final NumberLiteral nb = (NumberLiteral) arg;
					if (nb.getToken().contains(".")) {
						this.ctx.getRefactorings().replace(
								node,
								getClassInstanceCreatorNode(
										(Name) node.getExpression(),
										nb.getToken()));
					} else if ("0".equals(nb.getToken())) {
						replaceWithQualifiedName(node, typeBinding.getName(),
								"ZERO");
					} else if ("1".equals(nb.getToken())) {
						replaceWithQualifiedName(node, typeBinding.getName(),
								"ONE");
					} else if ("10".equals(nb.getToken())) {
						replaceWithQualifiedName(node, typeBinding.getName(),
								"TEN");
					}
					return ASTHelper.DO_NOT_VISIT_SUBTREE;
				}
			} else if ("equals".equals(node.getName().getIdentifier())
					&& node.arguments().size() == 1) {
				final Expression arg = (Expression) node.arguments().get(0);
				if (ASTHelper.hasType(arg, "java.math.BigDecimal")) {
					if (isInStringAppend(node.getParent())) {
						this.ctx.getRefactorings()
								.replace(
										node,
										getParenthesizedExpression(getCompareToNode(node)));
						return ASTHelper.DO_NOT_VISIT_SUBTREE;
					} else {
						this.ctx.getRefactorings().replace(node, getCompareToNode(node));
						return ASTHelper.DO_NOT_VISIT_SUBTREE;
					}
				}
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private ParenthesizedExpression getParenthesizedExpression(
			Expression compareToNode) {
		final ParenthesizedExpression pe = this.ctx.getAST()
				.newParenthesizedExpression();
		pe.setExpression(compareToNode);
		return pe;
	}

	private boolean isInStringAppend(ASTNode node) {
		if (node instanceof InfixExpression) {
			final InfixExpression expr = (InfixExpression) node;
			if (Operator.PLUS.equals(expr.getOperator())
					|| (ASTHelper.hasType(expr.getLeftOperand(),
							"java.lang.String") || ASTHelper.hasType(
							expr.getRightOperand(), "java.lang.String"))) {
				return true;
			}
		}
		return false;
	}

	private ASTNode getClassInstanceCreatorNode(Name className,
			String numberListeral) {
		final ClassInstanceCreation cic = this.ctx.getAST().newClassInstanceCreation();
		cic.setType(this.ctx.getAST().newSimpleType(ASTHelper.copySubtree(this.ctx.getAST(),
				className)));
		cic.arguments().add(getStringLiteral(numberListeral));
		return cic;
	}

	private InfixExpression getCompareToNode(MethodInvocation node) {
		final AST ast = this.ctx.getAST();
		final MethodInvocation mi = ast.newMethodInvocation();
		mi.setName(ast.newSimpleName("compareTo"));
		mi.setExpression(ASTHelper.copySubtree(ast, node.getExpression()));
		mi.arguments().add(ASTHelper.copySubtree(ast, node.arguments().get(0)));

		final NumberLiteral nl = ast.newNumberLiteral();
		nl.setToken("0");

		final InfixExpression ie = ast.newInfixExpression();
		ie.setLeftOperand(mi);
		ie.setOperator(Operator.EQUALS);
		ie.setRightOperand(nl);

		return ie;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
