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
package org.autorefactor.jdt.internal.ui.fix;

import static org.eclipse.jdt.core.dom.ASTNode.QUALIFIED_NAME;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_NAME;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;

/**
 * See {@link #getDescription()} method.
 */
public abstract class AbstractUnitTestCleanUp extends AbstractCleanUpRule {
    /**
     * The scan of the class imports.
     */
    private final Set<String> staticImports= new HashSet<>();

    /**
     * Return true if assertNotEquals can be used.
     *
     * @return True if assertNotEquals can be used.
     */
    protected abstract boolean canUseAssertNotEquals();

    /**
     * Get the actual value and then the expected value.
     *
     * @param leftValue  The left value
     * @param rightValue The right value
     * @return The actual and the expected.
     */
    protected abstract Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
            final Expression rightValue);

    /**
     * Invoke the method with full qualified name if needed.
     *
     * @param b              The builder.
     * @param copyOfMethod   The copy of the original method.
     * @param methodName     methodName.
     * @param copyOfActual   The copy of the actual value or null.
     * @param copyOfExpected The copy of the expected value or null.
     * @param delta          The delta or null
     * @param failureMessage The original failure message or null.
     * @return The method invocation object.
     */
    protected abstract MethodInvocation invokeQualifiedMethod(final ASTNodeFactory b, final Expression copyOfMethod,
            final String methodName, final Expression copyOfActual, final Expression copyOfExpected,
            Expression delta, final Expression failureMessage);

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
                staticImports.add(node.getName().getFullyQualifiedName() + ".*"); //$NON-NLS-1$
            } else {
                staticImports.add(node.getName().getFullyQualifiedName());
            }
        }
        return true;
    }

    /**
     * Maybe refactor the statement.
     *
     * @param nodeToReplace   The node
     * @param originalMethod  The method invocation
     * @param isAssertTrue    True if assertTrue is used, False if assertFalse is
     *                        used.
     * @param condition       The condition on which the assert is based.
     * @param failureMessage  The failure message or null.
     * @param isRewriteNeeded True if is the rewriting is needed.
     * @return True if refactored
     */
    protected boolean maybeRefactorStatement(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean isAssertTrue, final Expression condition, final Expression failureMessage,
            final boolean isRewriteNeeded) {
        Expression localCondition= condition;
        boolean localIsAssertTrue= isAssertTrue;
        boolean localIsRewriteNeeded= isRewriteNeeded;
        PrefixExpression localConditionPe= ASTNodes.as(localCondition, PrefixExpression.class);

        while (ASTNodes.hasOperator(localConditionPe, PrefixExpression.Operator.NOT)) {
            localIsRewriteNeeded= true;

            localIsAssertTrue= !localIsAssertTrue;
            localCondition= ASTNodes.as(localConditionPe.getOperand(), Expression.class);
            localConditionPe= ASTNodes.as(localCondition, PrefixExpression.class);
        }

        final InfixExpression conditionIe= ASTNodes.as(localCondition, InfixExpression.class);
        final MethodInvocation conditionMi= ASTNodes.as(localCondition, MethodInvocation.class);
        final Object constantValue= localCondition.resolveConstantExpressionValue();

        return maybeRefactorAssertTrueOrFalse(nodeToReplace, originalMethod, localIsAssertTrue, localCondition,
                conditionIe, conditionMi, constantValue, failureMessage, localIsRewriteNeeded);
    }

    private boolean maybeRefactorAssertTrueOrFalse(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean isAssertTrue, final Expression condition, final InfixExpression conditionIe,
            final MethodInvocation conditionMi, final Object constantValue, final Expression failureMessage,
            final boolean isRewriteNeeded) {
        if (conditionIe != null) {
            if (ASTNodes.hasOperator(conditionIe, InfixExpression.Operator.EQUALS)) {
                return maybeRefactorComparison(nodeToReplace, originalMethod, conditionIe, isAssertTrue, failureMessage,
                        isRewriteNeeded);
            }
            if (ASTNodes.hasOperator(conditionIe, InfixExpression.Operator.NOT_EQUALS)) {
                return maybeRefactorComparison(nodeToReplace, originalMethod, conditionIe, !isAssertTrue,
                        failureMessage, isRewriteNeeded);
            }
        } else if (ASTNodes.usesGivenSignature(conditionMi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
            if (canUseAssertNotEquals() || isAssertTrue) {
                final Pair<Expression, Expression> actualAndExpected= getActualAndExpected(conditionMi.getExpression(),
                        ASTNodes.arg0(conditionMi));
                return maybeRefactorToEquality(nodeToReplace, originalMethod, isAssertTrue,
                        actualAndExpected.getFirst(), actualAndExpected.getSecond(), failureMessage, true);
            }
        } else if (constantValue instanceof Boolean) {
            return maybeReplaceOrRemove(nodeToReplace, originalMethod, isAssertTrue ^ (Boolean) constantValue,
                    failureMessage);
        } else if (isRewriteNeeded) {
            return refactorToAssertTrueOrFalse(nodeToReplace, originalMethod, failureMessage, condition, isAssertTrue);
        }
        return true;
    }

    private boolean refactorToAssertTrueOrFalse(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final Expression failureMessage, final Expression condition, final boolean isAssertTrue) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();
        final String methodName= isAssertTrue ? "assertTrue" : "assertFalse"; //$NON-NLS-1$ $NON-NLS-2$

        r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b,
                invokeMethod(b, originalMethod, methodName, b.copy(condition), null, null, failureMessage)));
        return false;
    }

    private boolean maybeReplaceOrRemove(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean replace, final Expression failureMessage) {
        final Refactorings r= this.ctx.getRefactorings();
        if (replace) {
            r.replace(nodeToReplace, invokeFail(nodeToReplace, originalMethod, failureMessage));
            return false;
        }
        if (nodeToReplace.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
            r.remove(nodeToReplace.getParent());
            return false;
        }
        return true;
    }

    private MethodInvocation invokeFail(final ASTNode node, final MethodInvocation originalMethod,
            final Expression failureMessage) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final List<Expression> args= ASTNodes.arguments(originalMethod);
        if (args.size() == 1 || args.size() == 2) {
            return invokeMethod(b, originalMethod, "fail", null, null, null, failureMessage); //$NON-NLS-1$
        }
        throw new NotImplementedException(node);
    }

    private boolean maybeRefactorComparison(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final InfixExpression ie, final boolean isAssertEquals, final Expression failureMessage,
            final boolean isRewriteNeeded) {
        final Pair<Expression, Expression> actualAndExpected= getActualAndExpected(ie.getLeftOperand(),
                ie.getRightOperand());

        if (isComparingObjects(ie) && !ASTNodes.is(ie.getLeftOperand(), NullLiteral.class) && !ASTNodes.is(ie.getRightOperand(), NullLiteral.class)) {
            final ASTNodeFactory b= this.ctx.getASTBuilder();
            final Refactorings r= this.ctx.getRefactorings();

            final MethodInvocation newAssert= invokeMethod(b, originalMethod, getAssertName(isAssertEquals, "Same"), //$NON-NLS-1$
                    b.copy(actualAndExpected.getFirst()), b.copy(actualAndExpected.getSecond()), null, failureMessage);
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b, newAssert));
            return false;
        }
        return maybeRefactorToEquality(nodeToReplace, originalMethod, isAssertEquals,
                actualAndExpected.getFirst(), actualAndExpected.getSecond(), failureMessage, true);
    }

    /**
     * Maybe refactor the assert null or equals.
     *
     * @param nodeToReplace   The node to replace
     * @param originalMethod  The node
     * @param isAssertEquals  The is assert equals
     * @param actualValue     The actual value
     * @param expectedValue   The expected value
     * @param failureMessage  The failure message
     * @param isRewriteNeeded True if is the rewriting is needed.
     * @return The return
     */
    protected boolean maybeRefactorToEquality(final ASTNode nodeToReplace, final MethodInvocation originalMethod,
            final boolean isAssertEquals, final Expression actualValue, final Expression expectedValue,
            final Expression failureMessage, final boolean isRewriteNeeded) {
        final Refactorings r= this.ctx.getRefactorings();
        final ASTNodeFactory b= this.ctx.getASTBuilder();

        if (ASTNodes.is(actualValue, NullLiteral.class)) {
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b,
                    invokeAssertNull(originalMethod, isAssertEquals, expectedValue, failureMessage)));
            return false;
        }

        if (ASTNodes.is(expectedValue, NullLiteral.class)) {
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b,
                    invokeAssertNull(originalMethod, isAssertEquals, actualValue, failureMessage)));
            return false;
        }

        Expression copyOfExpected= b.copy(expectedValue);
        Expression copyOfActual= b.copy(actualValue);
        boolean localIsRewriteNeeded= isRewriteNeeded;

        if ((ASTNodes.isConstant(actualValue) || isVariableNamedExpected(actualValue)) && !ASTNodes.isConstant(expectedValue)
                && !isVariableNamedExpected(expectedValue)) {
            copyOfExpected= b.copy(actualValue);
            copyOfActual= b.copy(expectedValue);
            localIsRewriteNeeded= true;
        }

        if (localIsRewriteNeeded) {
            Expression delta= null;

            if (ASTNodes.hasType(actualValue, double.class.getCanonicalName()) && ASTNodes.hasType(expectedValue, double.class.getCanonicalName())) {
                delta= b.number(".0");
            } else if (ASTNodes.hasType(actualValue, float.class.getCanonicalName()) && ASTNodes.hasType(expectedValue, float.class.getCanonicalName())) {
                delta= b.number(".0F");
            }

            final MethodInvocation newAssert= invokeMethod(b, originalMethod, getAssertName(isAssertEquals, "Equals"), //$NON-NLS-1$
                    copyOfActual, copyOfExpected, delta, failureMessage);
            r.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, b, newAssert));
            return false;
        }

        return true;
    }

    private boolean isVariableNamedExpected(final Expression expression) {
        switch (expression.getNodeType()) {
        case SIMPLE_NAME:
            final SimpleName sn= (SimpleName) expression;
            return levenshteinDistance(sn.getIdentifier().toLowerCase(), "expected") <= 3; //$NON-NLS-1$

        case QUALIFIED_NAME:
            final QualifiedName qn= (QualifiedName) expression;
            return isVariableNamedExpected(qn.getName());

        default:
            return false;
        }
    }

    private String getAssertName(final boolean isPositive, final String assertType) {
        return "assert" + (isPositive ? "" : "Not") + assertType; //$NON-NLS-1$ $NON-NLS-2$ $NON-NLS-3$
    }

    private boolean isComparingObjects(final InfixExpression ie) {
        return !ASTNodes.isPrimitive(ie.getLeftOperand()) || !ASTNodes.isPrimitive(ie.getRightOperand());
    }

    private ASTNode invokeMethodOrStatement(final ASTNode nodeToReplace, final ASTNodeFactory b,
            final MethodInvocation newMethod) {
        if (nodeToReplace instanceof Statement) {
            // The new node should be also a statement
            return b.toStatement(newMethod);
        }
        return newMethod;
    }

    private MethodInvocation invokeAssertNull(final MethodInvocation originalMethod, final boolean isPositive,
            final Expression actual, final Expression failureMessage) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final String methodName= getAssertName(isPositive, "Null"); //$NON-NLS-1$
        final Expression copyOfActual= b.copy(actual);
        return invokeMethod(b, originalMethod, methodName, copyOfActual, null, null, failureMessage);
    }

    private MethodInvocation invokeMethod(final ASTNodeFactory b, final MethodInvocation originalMethod,
            final String methodName, final Expression copyOfActual, final Expression copyOfExpected,
            Expression delta, final Expression failureMessage) {
        final String qualifiedMethodName= originalMethod.resolveMethodBinding().getDeclaringClass().getQualifiedName();

        Expression qualifiedMethod;
        if (originalMethod.getExpression() == null && !staticImports.contains(qualifiedMethodName + "." + methodName) //$NON-NLS-1$
                && !staticImports.contains(qualifiedMethodName + ".*")) { //$NON-NLS-1$
            qualifiedMethod= b.name(qualifiedMethodName);
        } else {
            qualifiedMethod= b.copyExpression(originalMethod);
        }
        return invokeQualifiedMethod(b, qualifiedMethod, methodName, copyOfActual, copyOfExpected, delta, failureMessage);
    }

    /**
     * Returns the levenshtein distance between the two provided strings.
     * <p>
     * Note: Implementation comes from wikipedia.
     *
     * @param s1 the first string to compare
     * @param s2 the second string to compare
     * @return the levenshtein distance between the two strings
     * @see <a href=
     *      "https://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_full_matrix">
     *      Iterative implementation with full matrix</a>
     */
    private static int levenshteinDistance(final String s1, final String s2) {
        int s1Length= s1.length() + 1;
        int s2Length= s2.length() + 1;

        int[][] d= new int[s1Length][s2Length];
        for (int i= 0; i < s1Length; i++) {
            d[i][0]= i;
        }
        for (int j= 0; j < s2Length; j++) {
            d[0][j]= j;
        }

        for (int i= 1; i < s1Length; i++) {
            for (int j= 1; j < s2Length; j++) {
                int cost= s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;

                int deleteCost= d[i - 1][j] + 1;
                int insertCost= d[i][j - 1] + 1;
                int substitutionCost= d[i - 1][j - 1] + cost;
                d[i][j]= Math.min(Math.min(deleteCost, insertCost), substitutionCost);
            }
        }

        return d[s1Length - 1][s2Length - 1];
    }
}
