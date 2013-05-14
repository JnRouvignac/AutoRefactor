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
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;

/**
 * Collapses two consecutive if statements into just one.
 */
public class CollapseIfStatementRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;

	public CollapseIfStatementRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	@Override
	public boolean visit(IfStatement node) {
		if (node.getElseStatement() == null) {
			final IfStatement is = ASTHelper.as(node.getThenStatement(),
					IfStatement.class);
			if (is != null) {
				replaceIfNoElseStatement(node, is);
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private boolean replaceIfNoElseStatement(IfStatement outerIf,
			IfStatement innerIf) {
		if (innerIf.getElseStatement() != null) {
			return ASTHelper.VISIT_SUBTREE;
		}

		final AST ast = this.ctx.getAST();
		final Expression leftOperand = ASTHelper.copySubtree(ast,
				outerIf.getExpression());
		final Expression rightOperand = ASTHelper.copySubtree(ast,
				innerIf.getExpression());

		final InfixExpression ie = ast.newInfixExpression();
		ie.setLeftOperand(parenthesizeInfixExpr(leftOperand));
		ie.setOperator(Operator.CONDITIONAL_AND);
		ie.setRightOperand(parenthesizeInfixExpr(rightOperand));

		final IfStatement is = ast.newIfStatement();
		is.setExpression(ie);
		is.setThenStatement(ASTHelper.copySubtree(ast, innerIf.getThenStatement()));
		this.ctx.getRefactorings().replace(outerIf, is);
		return ASTHelper.DO_NOT_VISIT_SUBTREE;
	}

	private Expression parenthesizeInfixExpr(Expression expr) {
		if (expr instanceof InfixExpression) {
			final InfixExpression ie = (InfixExpression) expr;
			if (InfixExpression.Operator.CONDITIONAL_OR
					.equals(ie.getOperator())) {
				final ParenthesizedExpression pe = this.ctx.getAST()
						.newParenthesizedExpression();
				pe.setExpression(ie);
				return pe;
			}
		}
		return expr;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
