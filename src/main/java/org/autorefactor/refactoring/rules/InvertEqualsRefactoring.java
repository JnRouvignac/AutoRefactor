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
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/**
 * Inverts calls to {@link Object#equals(Object)} and
 * {@link String#equalsIgnoreCase(String)} when it is known that the second
 * operand is not null and the first can be null.
 * <p>
 * TODO JNR use CFG and expression analysis to find extra information about
 * expression nullness.
 * </p>
 */
public class InvertEqualsRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private final Refactorings refactorings = new Refactorings();
	private AST ast;
	private Release javaSERelease;

	public InvertEqualsRefactoring() {
		super();
	}

	public void setAST(final AST ast) {
		this.ast = ast;
	}

	public void setJavaSERelease(Release javaSERelease) {
		this.javaSERelease = javaSERelease;
	}

	@Override
	public boolean visit(MethodInvocation node) {
		if (node.getExpression() == null || node.arguments().size() != 1) {
			return ASTHelper.VISIT_SUBTREE;
		}
		final boolean isEquals = "equals"
				.equals(node.getName().getIdentifier());
		final boolean isStringEqualsIgnoreCase = ASTHelper.hasType(
				node.getExpression(), "java.lang.String")
				&& "equalsIgnoreCase".equals(node.getName().getIdentifier());
		if (isEquals || isStringEqualsIgnoreCase) {
			final Expression expr = node.getExpression();
			final Object exprConstantValue = expr
					.resolveConstantExpressionValue();
			final Expression arg = (Expression) node.arguments().get(0);
			final Object argConstantValue = arg
					.resolveConstantExpressionValue();
			if (exprConstantValue == null && argConstantValue != null) {
				this.refactorings.replace(node,
						invertEqualsInvocation(expr, arg));
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private ASTNode invertEqualsInvocation(Expression lhs, Expression rhs) {
		final MethodInvocation mi = this.ast.newMethodInvocation();
		mi.setExpression(ASTHelper.copySubtree(this.ast, rhs));
		mi.setName(this.ast.newSimpleName("equals"));
		mi.arguments().add(ASTHelper.copySubtree(this.ast, lhs));
		return mi;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.refactorings;
	}
}
