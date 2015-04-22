/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/**
 * Refactors the use of TestNG assertions.
 * <p>
 * FIXME: Assert.assertNotEquals() exists only since TestNG 6.1.
 * This refactoring should be made conditional on TestNG version.
 * </p>
 */
public class TestNGAssertRefactoring extends AbstractRefactoringRule {

    private static final String OBJECT = "java.lang.Object";

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        if (isMethod(node, "org.testng.Assert", "assertTrue", "boolean")
                || isMethod(node, "org.testng.Assert", "assertTrue", "boolean", "java.lang.String")) {
            return maybeRefactorAssertTrue(node, true);
        } else if (isMethod(node, "org.testng.Assert", "assertFalse", "boolean")
                || isMethod(node, "org.testng.Assert", "assertFalse", "boolean", "java.lang.String")) {
            return maybeRefactorAssertTrue(node, false);
        } else if (isMethod(node, "org.testng.Assert", "assertEquals", OBJECT, OBJECT)
                || isMethod(node, "org.testng.Assert", "assertEquals", OBJECT, OBJECT, "java.lang.String")) {
            return maybeRefactorAssertEquals(node, true);
        } else if (isMethod(node, "org.testng.Assert", "assertNotEquals", OBJECT, OBJECT)
                || isMethod(node, "org.testng.Assert", "assertNotEquals", OBJECT, OBJECT, "java.lang.String")) {
            return maybeRefactorAssertEquals(node, false);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorAssertTrue(MethodInvocation node, boolean isAssertTrue) {
        final List<Expression> args = arguments(node);
        final Expression arg0 = args.get(0);
        final InfixExpression arg0Ie = as(arg0, InfixExpression.class);
        final MethodInvocation arg0mi = as(arg0, MethodInvocation.class);
        final PrefixExpression arg0pe = as(arg0, PrefixExpression.class);
        final Refactorings r = this.ctx.getRefactorings();
        if (arg0Ie != null) {
            if (Operator.EQUALS.equals(arg0Ie.getOperator())) {
                return invokeAssert(node, arg0Ie, !isAssertTrue);
            } else if (Operator.NOT_EQUALS.equals(arg0Ie.getOperator())) {
                return invokeAssert(node, arg0Ie, isAssertTrue);
            }
        } else if (isMethod(arg0mi, OBJECT, "equals", OBJECT)) {
            r.replace(node,
                    invokeAssertEquals(node, arg0mi, !isAssertTrue));
            return DO_NOT_VISIT_SUBTREE;
        } else if (arg0pe != null && PrefixExpression.Operator.NOT.equals(arg0pe.getOperator())) {
            final MethodInvocation negatedMi = as(arg0pe.getOperand(), MethodInvocation.class);
            if (isMethod(negatedMi, OBJECT, "equals", OBJECT)) {
                r.replace(node,
                        invokeAssertEquals(node, negatedMi, isAssertTrue));
                return DO_NOT_VISIT_SUBTREE;
            } else {
                r.replace(node,
                        invertAssert(node, isAssertTrue, arg0pe.getOperand()));
                return DO_NOT_VISIT_SUBTREE;
            }
        } else {
            Object constantValue = arg0.resolveConstantExpressionValue();
            if (Boolean.TRUE.equals(constantValue)) {
                return replaceOrRemove(node, !isAssertTrue);
            } else if (Boolean.FALSE.equals(constantValue)) {
                return replaceOrRemove(node, isAssertTrue);
            }
        }
        return VISIT_SUBTREE;
    }

    private MethodInvocation invertAssert(MethodInvocation node, boolean isAssertTrue, Expression arg) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(node);
        final String methodName = isAssertTrue ? "assertFalse" : "assertTrue";
        final Expression msgArg = getMessageArg(node, 1);
        if (msgArg == null) {
            return b.invoke(copyOfExpr, methodName, b.copy(arg));
        } else  {
            return b.invoke(copyOfExpr, methodName, b.copy(arg), b.copy(msgArg));
        }
    }

    private boolean replaceOrRemove(MethodInvocation node, boolean replace) {
        final Refactorings r = this.ctx.getRefactorings();
        if (replace) {
            r.replace(node, invokeFail(node));
            return DO_NOT_VISIT_SUBTREE;
        } else if (node.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
            r.remove(node.getParent());
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorAssertEquals(MethodInvocation node, boolean isAssertEquals) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final List<Expression> args = arguments(node);
        final Expression arg0 = args.get(0);
        final Expression arg1 = args.get(1);
        if (isNullLiteral(arg0)) {
            r.replace(node,
                    invokeAssertNull(node, !isAssertEquals, arg1, getMessageArg(node, 2)));
            return DO_NOT_VISIT_SUBTREE;
        } else if (isNullLiteral(arg1)) {
            r.replace(node,
                    invokeAssertNull(node, !isAssertEquals, arg0, getMessageArg(node, 2)));
            return DO_NOT_VISIT_SUBTREE;
        } else if (isConstant(arg0) && !isConstant(arg1)) {
            r.replace(arg0, b.copy(arg1));
            r.replace(arg1, b.copy(arg0));
            return DO_NOT_VISIT_SUBTREE;
        } else if (isVariableNamedExpected(arg0) && !isVariableNamedExpected(arg1)) {
            r.replace(arg0, b.copy(arg1));
            r.replace(arg1, b.copy(arg0));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean isVariableNamedExpected(Expression expr) {
        switch (expr.getNodeType()) {
        case SIMPLE_NAME:
            final SimpleName sn = (SimpleName) expr;
            return levenshteinDistance(sn.getIdentifier().toLowerCase(), "expected") <= 3;

        case QUALIFIED_NAME:
            final QualifiedName qn = (QualifiedName) expr;
            return isVariableNamedExpected(qn.getName());

        default:
            return false;
        }
    }

    /**
     * Returns the levenstein distance between the two provided strings.
     * <p>
     * Note: Implementation comes from wikipedia.
     *
     * @param s1 the first string to compare
     * @param s2 the second string to compare
     * @return the levenshtein distance between the two strings
     * @see <a href="https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_full_matrix">
     * Iterative implementation with full matrix</a>
     */
    private static int levenshteinDistance(String s1, String s2) {
        int s1Length = s1.length() + 1;
        int s2Length = s2.length() + 1;

        int[][] d = new int[s1Length][s2Length];
        for (int i = 0; i < s1Length; i++) {
            d[i][0] = i;
        }
        for (int j = 0; j < s2Length; j++) {
            d[0][j] = j;
        }

        for (int i = 1; i < s1Length; i++) {
            for (int j = 1; j < s2Length; j++) {
                int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;

                int deleteCost = d[i - 1][j] + 1;
                int insertCost = d[i][j - 1] + 1;
                int substitutionCost = d[i - 1][j - 1] + cost;
                d[i][j] = Math.min(Math.min(deleteCost, insertCost), substitutionCost);
            }
        }

        return d[s1Length - 1][s2Length - 1];
    }

    private String getAssertName(boolean isNot, String assertType) {
        return "assert" + (isNot ? "Not" : "") + assertType;
    }

    private boolean invokeAssert(MethodInvocation node, final InfixExpression ie, boolean isNot) {
        final Refactorings r = this.ctx.getRefactorings();
        if (isComparingObjects(ie)) {
            if (isNullLiteral(ie.getLeftOperand())) {
                r.replace(node,
                        invokeAssertNull(node, isNot, ie.getRightOperand(), getMessageArg(node, 1)));
            } else if (isNullLiteral(ie.getRightOperand())) {
                r.replace(node,
                        invokeAssertNull(node, isNot, ie.getLeftOperand(), getMessageArg(node, 1)));
            } else {
                r.replace(node,
                        invokeAssert(node, getAssertName(isNot, "Same"), ie.getLeftOperand(), ie.getRightOperand()));
            }
        } else {
            r.replace(node,
                    invokeAssert(node, getAssertName(isNot, "Equals"), ie.getLeftOperand(), ie.getRightOperand()));
        }
        return DO_NOT_VISIT_SUBTREE;
    }

    private Expression getMessageArg(MethodInvocation node, int index) {
        final List<Expression> args = arguments(node);
        if (index < args.size()) {
            return args.get(index);
        }
        return null;
    }

    private MethodInvocation invokeAssertNull(
            MethodInvocation node, boolean isNot, Expression expr, Expression msgArg) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(node);
        final String methodName = getAssertName(isNot, "Null");
        final Expression copyOfActual = b.copy(expr);
        if (msgArg == null) {
            return b.invoke(copyOfExpr, methodName, copyOfActual);
        } else {
            return b.invoke(copyOfExpr, methodName, copyOfActual, b.copy(msgArg));
        }
    }

    private boolean isComparingObjects(final InfixExpression ie) {
        return !isPrimitive(ie.getLeftOperand()) || !isPrimitive(ie.getRightOperand());
    }

    private MethodInvocation invokeAssert(MethodInvocation node, String methodName,
            Expression actual, Expression expected) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return invokeAssert0(node, methodName, b.copy(actual), b.copy(expected), getMessageArg(node, 1));
    }

    private MethodInvocation invokeAssertEquals(MethodInvocation node, final MethodInvocation arg0mi, boolean isNot) {
        return invokeAssert(node, getAssertName(isNot, "Equals"), arg0mi.getExpression(), arguments(arg0mi));
    }

    private MethodInvocation invokeAssert(MethodInvocation node, String methodName,
            Expression actual, List<Expression> expected) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return invokeAssert0(node, methodName, b.copy(actual), b.copyRange(expected), getMessageArg(node, 1));
    }

    private MethodInvocation invokeAssert0(MethodInvocation node, String methodName,
            Expression copyOfActual, Expression copyOfExpected, Expression msgArg) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(node);
        if (msgArg == null) {
            return b.invoke(copyOfExpr, methodName, copyOfActual, copyOfExpected);
        } else {
            return b.invoke(copyOfExpr, methodName, copyOfActual, copyOfExpected, b.copy(msgArg));
        }
    }

    private MethodInvocation invokeFail(MethodInvocation node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final List<Expression> args = arguments(node);
        final Expression copyOfExpr = b.copyExpression(node);
        if (args.size() == 1) {
            return b.invoke(copyOfExpr, "fail");
        } else if (args.size() == 2) {
            return b.invoke(copyOfExpr, "fail", b.copy(args.get(1)));
        } else {
            throw new NotImplementedException(node);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(IfStatement node) {
        final List<Statement> stmts = asList(node.getThenStatement());
        if (stmts.size() == 1) {
            final MethodInvocation mi = asExpression(stmts.get(0), MethodInvocation.class);
            if (isMethod(mi, "org.testng.Assert", "fail")
                    || isMethod(mi, "org.testng.Assert", "fail", "java.lang.String")) {
                final InfixExpression conditionIe = as(node.getExpression(), InfixExpression.class);
                final MethodInvocation conditionMi = as(node.getExpression(), MethodInvocation.class);
                final PrefixExpression conditionPe = as(node.getExpression(), PrefixExpression.class);
                final Refactorings r = this.ctx.getRefactorings();
                if (conditionIe != null) {
                    if (Operator.EQUALS.equals(conditionIe.getOperator())) {
                        return invokeAssertForFail(node, mi, conditionIe, true);
                    } else if (Operator.NOT_EQUALS.equals(conditionIe.getOperator())) {
                        return invokeAssertForFail(node, mi, conditionIe, false);
                    }
                } else if (isMethod(conditionMi, OBJECT, "equals", OBJECT)) {
                    r.replace(node,
                            invokeAssertForFail(mi, "assertNotEquals",
                                    conditionMi.getExpression(), arguments(conditionMi).get(0)));
                    return DO_NOT_VISIT_SUBTREE;
                } else if (conditionPe != null && PrefixExpression.Operator.NOT.equals(conditionPe.getOperator())) {
                    final MethodInvocation negatedMi = as(conditionPe.getOperand(), MethodInvocation.class);
                    if (isMethod(negatedMi, OBJECT, "equals", OBJECT)) {
                        r.replace(node,
                                invokeAssertForFail(mi, "assertEquals",
                                        negatedMi.getExpression(), arguments(negatedMi).get(0)));
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean invokeAssertForFail(IfStatement toReplace, MethodInvocation mi, InfixExpression ie, boolean isNot) {
        final Refactorings r = this.ctx.getRefactorings();
        if (isNullLiteral(ie.getLeftOperand())) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            r.replace(toReplace,
                    b.toStmt(invokeAssertNull(mi, isNot, ie.getRightOperand(), getMessageArg(mi, 0))));
        } else if (isNullLiteral(ie.getRightOperand())) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            r.replace(toReplace,
                    b.toStmt(invokeAssertNull(mi, isNot, ie.getLeftOperand(), getMessageArg(mi, 0))));
        } else if (isComparingObjects(ie)) {
            r.replace(toReplace,
                    invokeAssertForFail(mi, getAssertName(isNot, "Same"), ie.getLeftOperand(), ie.getRightOperand()));
        } else {
            r.replace(toReplace,
                    invokeAssertForFail(mi, getAssertName(isNot, "Equals"), ie.getLeftOperand(), ie.getRightOperand()));
        }
        return DO_NOT_VISIT_SUBTREE;
    }

    private ExpressionStatement invokeAssertForFail(MethodInvocation mi, String methodName,
            Expression actual, Expression expected) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.toStmt(
                invokeAssert0(mi, methodName, b.copy(actual), b.copy(expected), getMessageArg(mi, 0)));
    }
}
