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

import java.util.List;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;

import static org.autorefactor.refactoring.ASTHelper.*;

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
        addAll(statements(tryBody), stmts);
        return tryBody;
    }

    public CatchClause catch0(String exceptionType, String exceptionName, Statement... stmts) {
        final CatchClause cc = ast.newCatchClause();
        final SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
        svd.setType(newSimpleType(exceptionType));
        svd.setName(ast.newSimpleName(exceptionName));
        cc.setException(svd);

        final Block block = ast.newBlock();
        addAll(statements(block), stmts);
        cc.setBody(block);
        return cc;
    }

    public <T extends Expression> T copyExpr(T node) {
        return ASTHelper.copySubtree(ast, node);
    }

    public <T extends Statement> T copyStmt(T node) {
        return ASTHelper.copySubtree(ast, node);
    }

    public IfStatement if0(Expression condition, Statement thenStatement) {
        return if0(condition, thenStatement, null);
    }

    public IfStatement if0(Expression condition, Statement thenStatement, Statement elseStatement) {
        final IfStatement is = ast.newIfStatement();
        is.setExpression(condition);
        is.setThenStatement(thenStatement);
        is.setElseStatement(elseStatement);
        return is;
    }

    public InfixExpression infixExpr(Expression leftOperand, InfixExpression.Operator operator, Expression rightOperand) {
        final InfixExpression ie = ast.newInfixExpression();
        ie.setLeftOperand(leftOperand);
        ie.setOperator(operator);
        ie.setRightOperand(rightOperand);
        return ie;
    }

    public NumberLiteral int0(int i) {
        return ast.newNumberLiteral(Integer.toString(i));
    }

    public MethodInvocation invoke(String expression, String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(ast.newSimpleName(expression));
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    public MethodInvocation invoke(Expression expression, String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(expression);
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    public Name name(String... names) {
        if (names.length == 0) {
            throw new IllegalArgumentException("Expected at least one name, but was given 0 names");
        }
        if (names.length == 1) {
            return ast.newSimpleName(names[0]);
        }
        return ast.newName(names);
    }

    public ClassInstanceCreation new0(String className, Expression... arguments) {
        final ClassInstanceCreation cic = ast.newClassInstanceCreation();
        cic.setType(newSimpleType(className));
        addAll(arguments(cic), arguments);
        return cic;
    }

    public ClassInstanceCreation new0(ITypeBinding binding, Expression... arguments) {
        final String className = binding.getName();
        final int ltIdx = className.indexOf('<');
        if (ltIdx == -1) {
            final ClassInstanceCreation cic = ast.newClassInstanceCreation();
            cic.setType(newSimpleType(className));
            addAll(arguments(cic), arguments);
            return cic;
        }
        final String erasedClassName = className.substring(0, ltIdx);
        final int gtIdx = className.indexOf('>', ltIdx);
        final String typeParam = className.substring(ltIdx + 1, gtIdx);

        final ClassInstanceCreation cic = ast.newClassInstanceCreation();
        final ParameterizedType type = ast.newParameterizedType(
                newSimpleType(erasedClassName));
        typeArguments(type).add(newSimpleType(typeParam));
        cic.setType(type);
        addAll(arguments(cic), arguments);
        return cic;
    }

    private <T extends ASTNode> void addAll(List<T> whereToAdd,
            @SuppressWarnings("unchecked") T... toAdd) {
        for (T e : toAdd) {
            whereToAdd.add(e);
        }
    }

    private SimpleType newSimpleType(final String erasedClassName) {
        return ast.newSimpleType(ast.newName(erasedClassName));
    }

    public NumberLiteral number(String s) {
        return ast.newNumberLiteral(s);
    }

    public ParenthesizedExpression parenthesize(Expression expression) {
        final ParenthesizedExpression pe = ast.newParenthesizedExpression();
        pe.setExpression(expression);
        return pe;
    }

    public ReturnStatement return0(Expression expression) {
        final ReturnStatement rs = ast.newReturnStatement();
        rs.setExpression(expression);
        return rs;
    }

    public StringLiteral string(String s) {
        final StringLiteral sl = ast.newStringLiteral();
        sl.setLiteralValue(s);
        return sl;
    }

    public ThrowStatement throw0(final Expression expression) {
        final ThrowStatement throwS = ast.newThrowStatement();
        throwS.setExpression(expression);
        return throwS;
    }

    public ExpressionStatement toStmt(final Expression expression) {
        return ast.newExpressionStatement(expression);
    }

    public TryStatement try0(final Block body, CatchClause... catchClauses) {
        final TryStatement tryS = ast.newTryStatement();
        tryS.setBody(body);
        addAll(catchClauses(tryS), catchClauses);
        return tryS;
    }

}
