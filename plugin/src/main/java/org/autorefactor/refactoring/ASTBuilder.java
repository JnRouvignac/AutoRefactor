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

import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
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
    private final Refactorings refactorings;

    /**
     * Class constructor.
     *
     * @param refactorings the refactorings
     */
    public ASTBuilder(final Refactorings refactorings) {
        this.refactorings = refactorings;
        this.ast = refactorings.getAST();
    }

    /**
     * Builds a new {@link Assignment} instance.
     *
     * @param lhs the left hand side expression
     * @param operator the assignment operator
     * @param rhs the right hand side expression
     * @return a new Block
     */
    public Assignment assign(final Expression lhs, final Assignment.Operator operator, final Expression rhs) {
        final Assignment assign = ast.newAssignment();
        assign.setLeftHandSide(lhs);
        assign.setOperator(operator);
        assign.setRightHandSide(rhs);
        return assign;
    }

    /**
     * Builds a new {@link Block} instance.
     *
     * @param stmts the statements to add to the block
     * @return a new Block
     */
    public Block body(final Statement... stmts) {
        final Block tryBody = ast.newBlock();
        addAll(statements(tryBody), stmts);
        return tryBody;
    }

    /**
     * Builds a new {@link CatchClause} instance.
     *
     * @param exceptionTypeName the exception type name
     * @param caughtExceptionName the local name for the caught exception
     * @param stmts the statements to add to the catch clause
     * @return a new catch clause
     */
    public CatchClause catch0(String exceptionTypeName, String caughtExceptionName, Statement... stmts) {
        final CatchClause cc = ast.newCatchClause();
        final SingleVariableDeclaration svd = ast.newSingleVariableDeclaration();
        svd.setType(newSimpleType(exceptionTypeName));
        svd.setName(ast.newSimpleName(caughtExceptionName));
        cc.setException(svd);

        final Block block = ast.newBlock();
        addAll(statements(block), stmts);
        cc.setBody(block);
        return cc;
    }

    /**
     * Returns a copy of the provided {@link Expression}.
     *
     * @param <T> the actual expression type
     * @param exprToCopy the expression to copy
     * @return a copy of the expression
     * @deprecated use {@link #copy(ASTNode)}
     */
    public <T extends Expression> T copyExpr(T exprToCopy) {
        return copySubtree(ast, exprToCopy);
    }

    /**
     * Returns a copy of the provided {@link ASTNode}.
     *
     * @param <T> the actual node type
     * @param nodeToCopy the node to copy
     * @return a copy of the node
     */
    public <T extends ASTNode> T copy(T nodeToCopy) {
        if (isValidInCurrentAST(nodeToCopy)) {
            return refactorings.createCopyTarget(nodeToCopy);
        }
        return copySubtree(ast, nodeToCopy);
    }

    /**
     * Returns a copy of the provided {@link Expression} list.
     *
     * @param <E> the actual expression's type
     * @param expressions the expression list to copy
     * @return a copy of the expression list
     */
    public <E extends Expression> List<E> copyAll(List<E> expressions) {
        return copySubtrees(ast, expressions);
    }

    private boolean isValidInCurrentAST(ASTNode node) {
        return node.getAST() == ast
                && node.getStartPosition() != -1;
    }

    /**
     * Builds a new {@link IfStatement} instance.
     *
     * @param condition the if condition
     * @param thenStatement the then statement
     * @return a new if statement
     */
    public IfStatement if0(Expression condition, Statement thenStatement) {
        return if0(condition, thenStatement, null);
    }

    /**
     * Builds a new {@link IfStatement} instance.
     *
     * @param condition the if condition
     * @param thenStatement the statement of the then clause
     * @param elseStatement the statement of the else clause
     * @return a new if statement
     */
    public IfStatement if0(Expression condition, Statement thenStatement, Statement elseStatement) {
        final IfStatement is = ast.newIfStatement();
        is.setExpression(condition);
        is.setThenStatement(thenStatement);
        is.setElseStatement(elseStatement);
        return is;
    }

    /**
     * Builds a new {@link InfixExpression} instance.
     *
     * @param leftOperand the left operand
     * @param operator the infix operator
     * @param rightOperand the right operand
     * @return a new infix expression
     */
    public InfixExpression infixExpr(Expression leftOperand, InfixExpression.Operator operator,
            Expression rightOperand) {
        final InfixExpression ie = ast.newInfixExpression();
        ie.setLeftOperand(leftOperand);
        ie.setOperator(operator);
        ie.setRightOperand(rightOperand);
        return ie;
    }

    /**
     * Builds a new {@link NumberLiteral} instance.
     *
     * @param i the number literal value
     * @return a new number literal
     */
    public NumberLiteral int0(int i) {
        return ast.newNumberLiteral(Integer.toString(i));
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param expression the method invocation expression
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public MethodInvocation invoke(String expression, String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(ast.newSimpleName(expression));
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param expression the method invocation expression
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public MethodInvocation invoke(Expression expression, String methodName, Expression... arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(expression);
        mi.setName(ast.newSimpleName(methodName));
        addAll(arguments(mi), arguments);
        return mi;
    }

    /**
     * Builds a new {@link MethodInvocation} instance.
     *
     * @param <E> the arguments type
     * @param expression the method invocation expression
     * @param methodName the name of the invoked method
     * @param arguments the arguments for the method invocation
     * @return a new method invocation
     */
    public <E extends Expression> MethodInvocation invoke(Expression expression, String methodName, List<E> arguments) {
        final MethodInvocation mi = ast.newMethodInvocation();
        mi.setExpression(expression);
        mi.setName(ast.newSimpleName(methodName));
        arguments(mi).addAll(arguments);
        return mi;
    }

    /**
     * Builds a new {@link Name} instance. If only a single name is provided then a {@link SimpleName} is returned,
     * if several names are provided then a {@link QualifiedName} is built.
     *
     * @param names the qualified or simple name
     * @return a new name
     * @throws IllegalStateException if no names are provided
     */
    public Name name(String... names) {
        if (names.length == 0) {
            throw new IllegalArgumentException("Expected at least one name, but was given 0 names");
        }
        if (names.length == 1) {
            return ast.newSimpleName(names[0]);
        }
        return ast.newName(names);
    }

    /**
     * Builds a new {@link ClassInstanceCreation} instance.
     *
     * @param typeName the instantiated type name
     * @param arguments the constructor invocation arguments
     * @return a new class instance creation
     */
    public ClassInstanceCreation new0(String typeName, Expression... arguments) {
        final ClassInstanceCreation cic = ast.newClassInstanceCreation();
        cic.setType(newSimpleType(typeName));
        addAll(arguments(cic), arguments);
        return cic;
    }

    /**
     * Builds a new {@link ClassInstanceCreation} instance.
     *
     * @param typeBinding the type binding of the instantiated type
     * @param arguments the constructor invocation arguments
     * @return a new class instance creation
     */
    public ClassInstanceCreation new0(ITypeBinding typeBinding, Expression... arguments) {
        final String className = typeBinding.getName();
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

    /**
     * Builds a new {@link ArrayCreation} instance.
     *
     * @param <E> the type of the expressions
     * @param typeBinding the type binding of the instantiated type
     * @param arrayInitializers the expressions forming the array initializer
     * @return a new array creation instance
     */
    public <E extends Expression> ArrayCreation newArray(ITypeBinding typeBinding, List<E> arrayInitializers) {
        final ArrayInitializer ai = ast.newArrayInitializer();
        expressions(ai).addAll(arrayInitializers);

        final ArrayCreation ac = ast.newArrayCreation();
        ac.setType((ArrayType) toType(ast, typeBinding));
        ac.setInitializer(ai);
        return ac;
    }

    private SimpleType newSimpleType(final String typeName) {
        return ast.newSimpleType(ast.newName(typeName));
    }

    /**
     * Builds a new {@link NumberLiteral} instance.
     *
     * @param s the number literal value
     * @return a new number literal
     */
    public NumberLiteral number(String s) {
        return ast.newNumberLiteral(s);
    }

    /**
     * Builds a new {@link ParenthesizedExpression} instance.
     *
     * @param expression the expression to wrap with parentheses
     * @return a new parenthesized expression
     */
    public ParenthesizedExpression parenthesize(Expression expression) {
        final ParenthesizedExpression pe = ast.newParenthesizedExpression();
        pe.setExpression(expression);
        return pe;
    }

    /**
     * Builds a new {@link ReturnStatement} instance.
     *
     * @param expression the expression to return
     * @return a new return statement
     */
    public ReturnStatement return0(Expression expression) {
        final ReturnStatement rs = ast.newReturnStatement();
        rs.setExpression(expression);
        return rs;
    }

    /**
     * Builds a new {@link StringLiteral} instance.
     *
     * @param s the string literal value
     * @return a new string literal
     */
    public StringLiteral string(String s) {
        final StringLiteral sl = ast.newStringLiteral();
        sl.setLiteralValue(s);
        return sl;
    }

    /**
     * Builds a new {@link ThrowStatement} instance.
     *
     * @param expression the expression to throw
     * @return a new throw statement
     */
    public ThrowStatement throw0(final Expression expression) {
        final ThrowStatement throwS = ast.newThrowStatement();
        throwS.setExpression(expression);
        return throwS;
    }

    /**
     * Builds a new {@link ExpressionStatement} instance.
     *
     * @param expression the expression to transform into a statement
     * @return a new expression statement
     */
    public ExpressionStatement toStmt(final Expression expression) {
        return ast.newExpressionStatement(expression);
    }

    /**
     * Builds a new {@link TryStatement} instance.
     *
     * @param body the try body
     * @param catchClauses the catch clauses for the try
     * @return a new try statement
     */
    public TryStatement try0(final Block body, CatchClause... catchClauses) {
        final TryStatement tryS = ast.newTryStatement();
        tryS.setBody(body);
        addAll(catchClauses(tryS), catchClauses);
        return tryS;
    }

}
