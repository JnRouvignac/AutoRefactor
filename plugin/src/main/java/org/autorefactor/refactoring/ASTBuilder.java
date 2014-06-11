/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring;

import org.eclipse.jdt.core.dom.*;

/**
 * Helper class for building AST note in a somewhat fluent API.
 * Method names which are also java keywords are postfixed with a "0".
 */
public class ASTBuilder {

	private final AST ast;

	public ASTBuilder(final AST ast) {
		this.ast = ast;
	}

	public Block body(final Statement... stmts) {
		final Block tryBody = ast.newBlock();
		for (Statement stmt : stmts) {
			tryBody.statements().add(stmt);
		}
		return tryBody;
	}

	public CatchClause catch0(String exceptionType, String exceptionName, Statement... stmts) {
		final CatchClause cc = ast.newCatchClause();
		final SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
		svd.setType(ast.newSimpleType(ast.newSimpleName(exceptionType)));
		svd.setName(ast.newSimpleName(exceptionName));
		cc.setException(svd);

		final Block block = ast.newBlock();
		for (Statement stmt : stmts) {
			block.statements().add(stmt);
		}
		cc.setBody(block);
		return cc;
	}

	public MethodInvocation invoke(String expression, String methodName, Expression... arguments) {
		final MethodInvocation mi = ast.newMethodInvocation();
		mi.setExpression(ast.newSimpleName(expression));
		mi.setName(ast.newSimpleName(methodName));
		for (Expression argument : arguments) {
			mi.arguments().add(argument);
		}
		return mi;
	}

	public ClassInstanceCreation new0(String className, Expression... arguments) {
		final ClassInstanceCreation cic = ast.newClassInstanceCreation();
		cic.setType(ast.newSimpleType(ast.newSimpleName(className)));
		for (Expression argument : arguments) {
			cic.arguments().add(argument);
		}
		return cic;
	}

	public ThrowStatement throw0(final Expression expression) {
		final ThrowStatement throwS = ast.newThrowStatement();
		throwS.setExpression(expression);
		return throwS;
	}

	public ExpressionStatement toStmt(final MethodInvocation sysCopy) {
		return ast.newExpressionStatement(sysCopy);
	}

	public TryStatement try0(final Block body, CatchClause... catchClauses) {
		final TryStatement tryS = ast.newTryStatement();
		tryS.setBody(body);
		for (CatchClause catchCause : catchClauses) {
			tryS.catchClauses().add(catchCause);
		}
		return tryS;
	}

}
