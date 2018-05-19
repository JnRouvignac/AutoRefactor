/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Adapt for JUnit
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.*;

/**
 * See {@link #getDescription()} method.
 */
public abstract class AbstractUnitTestRefactoring extends AbstractRefactoringRule {

    /**
     * The OBJECT constant.
     */
    protected static final String OBJECT = "java.lang.Object";

    /**
     * The OBJECT constant.
     */
    private final Set<String> staticImports = new HashSet<String>();

    /**
     * Return true if assertNotEquals can be used.
     *
     * @return True if assertNotEquals can be used.
     */
    protected abstract boolean canUseAssertNotEquals();

    /**
     * Get the actual value and then the expected value.
     *
     * @param leftValue
     *            The left value
     * @param rightValue
     *            The right value
     * @return The actual and the expected.
     */
    protected abstract Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
            final Expression rightValue);

    /**
     * Invoke the method with full qualified name if needed.
     *
     * @param b
     *            The builder.
     * @param copyOfMethod
     *            The copy of the original method.
     * @param methodName
     *            methodName.
     * @param copyOfActual
     *            The copy of the actual value or null.
     * @param copyOfExpected
     *            The copy of the expected value or null.
     * @param failureMessage
     *            The original failure message or null.
     * @return The method invocation object.
     */
    protected abstract MethodInvocation invokeQualifiedMethod(final ASTBuilder b, final Expression copyOfMethod,
            final String methodName, final Expression copyOfActual, final Expression copyOfExpected,
            final Expression failureMessage);

    @Override
    public abstract boolean visit(MethodInvocation node);

    @Override
    public abstract boolean visit(IfStatement node);

    @Override
    public boolean visit(CompilationUnit node) {
        staticImports.clear();
        return super.visit(node);
    }

    @Override
    public boolean visit(final ImportDeclaration node) {
        if (node.isStatic()) {
            if (node.isOnDemand()) {
                staticImports.add(node.getName().getFullyQualifiedName() + ".*");
            } else {
                staticImports.add(node.getName().getFullyQualifiedName());
            }
        }
        return VISIT_SUBTREE;
    }

    /**
     * Maybe refactor the statement.
     *
     * @param nodeToReplace
     *            The node
     * @param originalMethod
     *            The method invocation
     * @param isAssertTrue
     *            True if assertTrue is used, False if assertFalse is used.
     * @param condition
     *            The condition on which the assert is based.
     * @param failureMessage
     *            The failure message or null.
     * @param isRewriteNeeded
     *            True if is the rewriting is needed.
     * @return True if refactored
     */
    protected boolean maybeRefactorStatement(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean isAssertTrue, final Expression condition, final Expression failureMessage,
            final boolean isRewriteNeeded) {
        Expression localCondition = condition;
        boolean localIsAssertTrue = isAssertTrue;
        boolean localIsRewriteNeeded = isRewriteNeeded;
        PrefixExpression localConditionPe = as(localCondition, PrefixExpression.class);

        while (hasOperator(localConditionPe, NOT)) {
            localIsRewriteNeeded = true;

            localIsAssertTrue = !localIsAssertTrue;
            localCondition = as(localConditionPe.getOperand(), Expression.class);
            localConditionPe = as(localCondition, PrefixExpression.class);
        }

        final InfixExpression conditionIe = as(localCondition, InfixExpression.class);
        final MethodInvocation conditionMi = as(localCondition, MethodInvocation.class);
        final Object constantValue = localCondition.resolveConstantExpressionValue();

        return maybeRefactorAssertTrueOrFalse(nodeToReplace, originalMethod, localIsAssertTrue, localCondition,
                conditionIe, conditionMi, constantValue, failureMessage, localIsRewriteNeeded);
    }

    private boolean maybeRefactorAssertTrueOrFalse(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean isAssertTrue, final Expression condition, final InfixExpression conditionIe,
            final MethodInvocation conditionMi, final Object constantValue, final Expression failureMessage,
            final boolean isRewriteNeeded) {
        if (conditionIe != null) {
            if (hasOperator(conditionIe, EQUALS)) {
                return maybeRefactorComparison(nodeToReplace, originalMethod, conditionIe, isAssertTrue, failureMessage,
                        isRewriteNeeded);
            } else if (hasOperator(conditionIe, NOT_EQUALS)) {
                return maybeRefactorComparison(nodeToReplace, originalMethod, conditionIe, !isAssertTrue,
                        failureMessage, isRewriteNeeded);
            }
        } else if (isMethod(conditionMi, OBJECT, "equals", OBJECT)) {
            if (canUseAssertNotEquals() || isAssertTrue) {
                final Pair<Expression, Expression> actualAndExpected = getActualAndExpected(conditionMi.getExpression(),
                        arg0(conditionMi));
                return maybeRefactorToAssertEquals(nodeToReplace, originalMethod, isAssertTrue,
                        actualAndExpected.getFirst(), actualAndExpected.getSecond(), failureMessage, true);
            }
        } else if (constantValue instanceof Boolean) {
            return maybeReplaceOrRemove(nodeToReplace, originalMethod, isAssertTrue ^ (Boolean) constantValue,
                    failureMessage);
        } else if (isRewriteNeeded) {
            return refactorToAssertTrueOrFalse(nodeToReplace, originalMethod, failureMessage, condition, isAssertTrue);
        }
        return VISIT_SUBTREE;
    }

    private boolean refactorToAssertTrueOrFalse(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final Expression failureMessage, final Expression condition, final boolean isAssertTrue) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        final String methodName = isAssertTrue ? "assertTrue" : "assertFalse";

        r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b,
                invokeMethod(b, originalMethod, methodName, b.copy(condition), null, failureMessage)));
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean maybeReplaceOrRemove(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean replace, final Expression failureMessage) {
        final Refactorings r = this.ctx.getRefactorings();
        if (replace) {
            r.replace(nodeToReplace, invokeFail(nodeToReplace, originalMethod, failureMessage));
            return DO_NOT_VISIT_SUBTREE;
        } else if (nodeToReplace.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
            r.remove(nodeToReplace.getParent());
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private MethodInvocation invokeFail(final ASTNode node, final MethodInvocation originalMethod,
            final Expression failureMessage) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final List<Expression> args = arguments(originalMethod);
        if ((args.size() == 1) || (args.size() == 2)) {
            return invokeMethod(b, originalMethod, "fail", null, null, failureMessage);
        } else {
            throw new NotImplementedException(node);
        }
    }

    private boolean maybeRefactorComparison(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final InfixExpression ie, final boolean isAssertEquals, final Expression failureMessage,
            final boolean isRewriteNeeded) {
        final Pair<Expression, Expression> actualAndExpected = getActualAndExpected(ie.getLeftOperand(),
                ie.getRightOperand());

        if (isComparingObjects(ie) && !isNullLiteral(ie.getLeftOperand()) && !isNullLiteral(ie.getRightOperand())) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            final Refactorings r = this.ctx.getRefactorings();

            final MethodInvocation newAssert = invokeMethod(b, originalMethod, getAssertName(isAssertEquals, "Same"),
                    b.copy(actualAndExpected.getFirst()), b.copy(actualAndExpected.getSecond()), failureMessage);
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b, newAssert));
            return DO_NOT_VISIT_SUBTREE;
        } else {
            return maybeRefactorToAssertEquals(nodeToReplace, originalMethod, isAssertEquals,
                    actualAndExpected.getFirst(), actualAndExpected.getSecond(), failureMessage, true);
        }
    }

    /**
     * Maybe refactor the assert equals.
     *
     * @param nodeToReplace
     *            The node to replace
     * @param originalMethod
     *            The node
     * @param isAssertEquals
     *            The is assert equals
     * @param actualValue
     *            The actual value
     * @param expectedValue
     *            The expected value
     * @param failureMessage
     *            The failure message
     * @param isRewriteNeeded
     *            True if is the rewriting is needed.
     * @return The return
     */
    protected boolean maybeRefactorToAssertEquals(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean isAssertEquals, final Expression actualValue, final Expression expectedValue,
            final Expression failureMessage, final boolean isRewriteNeeded) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();

        if (isNullLiteral(actualValue)) {
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b,
                    invokeAssertNull(originalMethod, isAssertEquals, expectedValue, failureMessage)));
            return DO_NOT_VISIT_SUBTREE;
        } else if (isNullLiteral(expectedValue)) {
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b,
                    invokeAssertNull(originalMethod, isAssertEquals, actualValue, failureMessage)));
            return DO_NOT_VISIT_SUBTREE;
        } else if ((isConstant(actualValue) || isVariableNamedExpected(actualValue))
                && !isConstant(expectedValue) && !isVariableNamedExpected(expectedValue)) {
            final MethodInvocation newAssert = invokeMethod(b, originalMethod, getAssertName(isAssertEquals, "Equals"),
                    b.copy(expectedValue), b.copy(actualValue), failureMessage);
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b, newAssert));
            return DO_NOT_VISIT_SUBTREE;
        } else if (isRewriteNeeded) {
            final MethodInvocation newAssert = invokeMethod(b, originalMethod, getAssertName(isAssertEquals, "Equals"),
                    b.copy(actualValue), b.copy(expectedValue), failureMessage);
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b, newAssert));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean isVariableNamedExpected(final Expression expr) {
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

    private String getAssertName(final boolean isPositive, final String assertType) {
        return "assert" + (isPositive ? "" : "Not") + assertType;
    }

    private boolean isComparingObjects(final InfixExpression ie) {
        return !isPrimitive(ie.getLeftOperand()) || !isPrimitive(ie.getRightOperand());
    }

    private ASTNode invokeMethodOrStatement(final ASTNode nodeToReplace, final ASTBuilder b,
            final MethodInvocation newMethod) {
        if (nodeToReplace instanceof Statement) {
            // The new node should be also a statement
            return b.toStmt(newMethod);
        } else {
            return newMethod;
        }
    }

    private MethodInvocation invokeAssertNull(final MethodInvocation originalMethod, final boolean isPositive,
            final Expression actual, final Expression failureMessage) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final String methodName = getAssertName(isPositive, "Null");
        final Expression copyOfActual = b.copy(actual);
        return invokeMethod(b, originalMethod, methodName, copyOfActual, null, failureMessage);
    }

    /**
     * Invoke the method.
     *
     * @param b
     *            The builder.
     * @param originalMethod
     *            The copy of the original method.
     * @param methodName
     *            methodName.
     * @param copyOfActual
     *            The copy of the actual value or null.
     * @param copyOfExpected
     *            The copy of the expected value or null.
     * @param failureMessage
     *            The original failure message or null.
     * @return The method invocation object.
     */
    private MethodInvocation invokeMethod(final ASTBuilder b, final MethodInvocation originalMethod,
            final String methodName, final Expression copyOfActual, final Expression copyOfExpected,
            final Expression failureMessage) {
        final String qualifiedMethodName =
                originalMethod.resolveMethodBinding().getDeclaringClass().getQualifiedName();

        Expression qualifiedMethod;
        if (originalMethod.getExpression() == null && !staticImports.contains(qualifiedMethodName + "." + methodName)
                && !staticImports.contains(qualifiedMethodName + ".*")) {
            qualifiedMethod = b.name(qualifiedMethodName.split("\\."));
        } else {
            qualifiedMethod = b.copyExpression(originalMethod);
        }
        return invokeQualifiedMethod(b, qualifiedMethod,
                methodName, copyOfActual, copyOfExpected,
                failureMessage);
    }

    /**
     * Returns the levenshtein distance between the two provided strings.
     * <p>
     * Note: Implementation comes from wikipedia.
     *
     * @param s1
     *            the first string to compare
     * @param s2
     *            the second string to compare
     * @return the levenshtein distance between the two strings
     * @see <a href=
     *      "https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_full_matrix">
     *      Iterative implementation with full matrix</a>
     */
    private static int levenshteinDistance(final String s1, final String s2) {
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
}
