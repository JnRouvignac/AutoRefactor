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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
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
public abstract class AbstractUnitTestCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final MethodInvocation node) {
            return maybeRefactorMethodInvocation(node, getClassesToUseWithImport(), getImportsToAdd());
        }

        @Override
        public boolean visit(final IfStatement node) {
            return maybeRefactorIfStatement(node, getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    /**
     * Can use assert not equals.
     */
    protected boolean canUseAssertNotEquals;

    /**
     * The scan of the class imports.
     */
    protected final Set<String> staticImports= new HashSet<>();

    /**
     * Get the actual value and then the expected value.
     *
     * @param leftValue  The left value
     * @param rightValue The right value
     * @return The actual and the expected.
     */
    protected abstract Pair<Expression, Expression> getActualAndExpected(Expression leftValue,
            Expression rightValue);

    /**
     * Invoke the method with full qualified name if needed.
     *
     * @param classesToUseWithImport The classes to use with import
     * @param importsToAdd           The imports to add
     * @param ast                      The builder.
     * @param copyOfMethod           The copy of the original method.
     * @param methodName             The method name.
     * @param copyOfActual           The copy of the actual value or null.
     * @param copyOfExpected         The copy of the expected value or null.
     * @param delta                  The delta or null
     * @param failureMessage         The original failure message or null.
     *
     * @return The method invocation object.
     */
    protected abstract MethodInvocation invokeQualifiedMethod(Set<String> classesToUseWithImport, Set<String> importsToAdd,
            ASTNodeFactory ast, Expression copyOfMethod, String methodName,
            Expression copyOfActual, Expression copyOfExpected, Expression delta, Expression failureMessage);

    /**
     * Resolve the type binding.
     *
     * @param node The node
     * @return the type binding.
     */
    protected ITypeBinding resolveTypeBinding(final ImportDeclaration node) {
        IBinding resolveBinding= node.resolveBinding();

        if (resolveBinding instanceof ITypeBinding) {
            return (ITypeBinding) resolveBinding;
        }

        if (resolveBinding instanceof IMethodBinding) {
            return ((IMethodBinding) resolveBinding).getDeclaringClass();
        }

        return null;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(0);
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public boolean visit(final MethodInvocation node) {
        return maybeRefactorMethodInvocation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    /**
     * MaybeRefactorMethodInvocation.
     *
     * @param node node
     * @param classesToUseWithImport classesToUseWithImport
     * @param importsToAdd importsToAdd
     * @return MaybeRefactorMethodInvocation.
     */
    protected abstract boolean maybeRefactorMethodInvocation(MethodInvocation node, Set<String> classesToUseWithImport,
            Set<String> importsToAdd);

    @Override
    public boolean visit(final IfStatement node) {
        return maybeRefactorIfStatement(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    /**
     * MaybeRefactorIfStatement.
     *
     * @param node node
     * @param classesToUseWithImport classesToUseWithImport
     * @param importsToAdd importsToAdd
     * @return MaybeRefactorIfStatement.
     */
    protected abstract boolean maybeRefactorIfStatement(IfStatement node, Set<String> classesToUseWithImport,
            Set<String> importsToAdd);

    @Override
    public boolean visit(final CompilationUnit node) {
        staticImports.clear();

        for (Object anObject : node.imports()) {
            ImportDeclaration anImport= (ImportDeclaration) anObject;

            if (anImport.isStatic()) {
                if (anImport.isOnDemand()) {
                    staticImports.add(anImport.getName().getFullyQualifiedName() + ".*"); //$NON-NLS-1$
                } else {
                    staticImports.add(anImport.getName().getFullyQualifiedName());
                }
            }
        }

        return super.visit(node);
    }

    /**
     * Maybe refactor the statement.
     *
     * @param classesToUseWithImport The classes to use with import
     * @param importsToAdd           The imports to add
     * @param nodeToReplace          The node
     * @param originalMethod         The method invocation
     * @param isAssertTrue           True if assertTrue is used, False if assertFalse is
     *                               used.
     * @param condition              The condition on which the assert is based.
     * @param failureMessage         The failure message or null.
     * @param isRewriteNeeded        True if is the rewriting is needed.
     * @return True if refactored
     */
    protected boolean maybeRefactorStatement(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode nodeToReplace, final MethodInvocation originalMethod, final boolean isAssertTrue,
            final Expression condition, final Expression failureMessage, final boolean isRewriteNeeded) {
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

        InfixExpression conditionIe= ASTNodes.as(localCondition, InfixExpression.class);
        MethodInvocation conditionMi= ASTNodes.as(localCondition, MethodInvocation.class);
        Object constantValue= localCondition.resolveConstantExpressionValue();

        return maybeRefactorAssertTrueOrFalse(classesToUseWithImport, importsToAdd, nodeToReplace, originalMethod,
                localIsAssertTrue, localCondition, conditionIe, conditionMi, constantValue, failureMessage, localIsRewriteNeeded);
    }

    private boolean maybeRefactorAssertTrueOrFalse(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode nodeToReplace, final MethodInvocation originalMethod, final boolean isAssertTrue,
            final Expression condition, final InfixExpression conditionIe, final MethodInvocation conditionMi,
            final Object constantValue, final Expression failureMessage, final boolean isRewriteNeeded) {
        if (conditionIe != null) {
            if (ASTNodes.hasOperator(conditionIe, InfixExpression.Operator.EQUALS)) {
                return maybeRefactorComparison(classesToUseWithImport, importsToAdd, nodeToReplace, originalMethod, conditionIe, isAssertTrue, failureMessage);
            }
            if (ASTNodes.hasOperator(conditionIe, InfixExpression.Operator.NOT_EQUALS)) {
                return maybeRefactorComparison(classesToUseWithImport, importsToAdd, nodeToReplace, originalMethod,
                        conditionIe, !isAssertTrue, failureMessage);
            }
        } else if (ASTNodes.usesGivenSignature(conditionMi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName())) { //$NON-NLS-1$
            if (canUseAssertNotEquals || isAssertTrue) {
                Pair<Expression, Expression> actualAndExpected= getActualAndExpected(conditionMi.getExpression(),
                        ASTNodes.arguments(conditionMi).get(0));
                return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, nodeToReplace,
                        originalMethod, isAssertTrue, actualAndExpected.getFirst(), actualAndExpected.getSecond(), failureMessage, true);
            }
        } else if (constantValue instanceof Boolean) {
            return maybeReplaceOrRemove(classesToUseWithImport, importsToAdd, nodeToReplace,
                    originalMethod, isAssertTrue ^ (Boolean) constantValue, failureMessage);
        } else if (isRewriteNeeded) {
            refactorToAssertTrueOrFalse(classesToUseWithImport, importsToAdd, nodeToReplace, originalMethod, failureMessage, condition, isAssertTrue);
            return false;
        }

        return true;
    }

    private void refactorToAssertTrueOrFalse(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode nodeToReplace, final MethodInvocation originalMethod, final Expression failureMessage, final Expression condition, final boolean isAssertTrue) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        String methodName= isAssertTrue ? "assertTrue" : "assertFalse"; //$NON-NLS-1$ //$NON-NLS-2$

        rewrite.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, ast,
                invokeMethod(classesToUseWithImport, importsToAdd, ast, originalMethod, methodName, ast.createCopyTarget(condition), null, null, failureMessage)), null);
    }

    private boolean maybeReplaceOrRemove(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode nodeToReplace, final MethodInvocation originalMethod, final boolean replace, final Expression failureMessage) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        if (replace) {
            rewrite.replace(nodeToReplace, invokeFail(classesToUseWithImport, importsToAdd, nodeToReplace, originalMethod, failureMessage), null);
            return false;
        }

        if (nodeToReplace.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
            if (ASTNodes.canHaveSiblings((Statement) nodeToReplace.getParent()) || nodeToReplace.getParent().getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
                rewrite.remove(nodeToReplace.getParent(), null);
            } else {
                rewrite.replace(nodeToReplace.getParent(), cuRewrite.getASTBuilder().block(), null);
            }

            return false;
        }

        return true;
    }

    private MethodInvocation invokeFail(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode node, final MethodInvocation originalMethod, final Expression failureMessage) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        List<Expression> args= ASTNodes.arguments(originalMethod);
        if (args.size() == 1 || args.size() == 2) {
            return invokeMethod(classesToUseWithImport, importsToAdd, ast, originalMethod, "fail", null, null, null, failureMessage); //$NON-NLS-1$
        }
        throw new NotImplementedException(node);
    }

    private boolean maybeRefactorComparison(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode nodeToReplace, final MethodInvocation originalMethod, final InfixExpression ie, final boolean isAssertEquals, final Expression failureMessage) {
        Pair<Expression, Expression> actualAndExpected= getActualAndExpected(ie.getLeftOperand(),
                ie.getRightOperand());

        if (isComparingObjects(ie) && !ASTNodes.is(ie.getLeftOperand(), NullLiteral.class) && !ASTNodes.is(ie.getRightOperand(), NullLiteral.class)) {
            ASTNodeFactory ast= cuRewrite.getASTBuilder();
            ASTRewrite rewrite= cuRewrite.getASTRewrite();

            MethodInvocation newAssert= invokeMethod(classesToUseWithImport, importsToAdd, ast,
                    originalMethod, getAssertName(isAssertEquals, "Same"), ast.createCopyTarget(actualAndExpected.getFirst()), ast.createCopyTarget(actualAndExpected.getSecond()), null, failureMessage); //$NON-NLS-1$
            rewrite.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, ast, newAssert), null);
            return false;
        }

        return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, nodeToReplace,
                originalMethod, isAssertEquals, actualAndExpected.getFirst(), actualAndExpected.getSecond(), failureMessage, true);
    }

    /**
     * Maybe refactor the assert null or equals.
     *
     * @param classesToUseWithImport The classes to use with import
     * @param importsToAdd           The imports to add
     * @param nodeToReplace          The node to replace
     * @param originalMethod         The original method
     * @param isAssertEquals         The is assert equals
     * @param actualValue            The actual value
     * @param expectedValue          The expected value
     * @param failureMessage         The failure message
     * @return The return
     */
    protected boolean maybeRefactorToEquality(final Set<String> classesToUseWithImport, final Set<String> importsToAdd, final MethodInvocation nodeToReplace,
            final MethodInvocation originalMethod, final boolean isAssertEquals, final Expression actualValue, final Expression expectedValue, final Expression failureMessage) {
        return maybeRefactorToEquality(classesToUseWithImport, importsToAdd, nodeToReplace,
                originalMethod, isAssertEquals, actualValue, expectedValue, failureMessage, false);
    }

    private boolean maybeRefactorToEquality(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNode nodeToReplace, final MethodInvocation originalMethod, final boolean isAssertEquals,
            final Expression actualValue, final Expression expectedValue, final Expression failureMessage, final boolean isRewriteNeeded) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        ASTNodeFactory ast= cuRewrite.getASTBuilder();

        if (ASTNodes.is(actualValue, NullLiteral.class)) {
            rewrite.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, ast,
                    invokeAssertNull(classesToUseWithImport, importsToAdd, originalMethod, isAssertEquals, expectedValue, failureMessage)), null);
            return false;
        }

        if (ASTNodes.is(expectedValue, NullLiteral.class)) {
            rewrite.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, ast,
                    invokeAssertNull(classesToUseWithImport, importsToAdd, originalMethod, isAssertEquals, actualValue, failureMessage)), null);
            return false;
        }

        Expression copyOfExpected= ast.createCopyTarget(expectedValue);
        Expression copyOfActual= ast.createCopyTarget(actualValue);
        boolean localIsRewriteNeeded= isRewriteNeeded;

        if ((ASTNodes.isHardCoded(actualValue) || isVariableNamedExpected(actualValue)) && !ASTNodes.isHardCoded(expectedValue)
                && !isVariableNamedExpected(expectedValue)) {
            copyOfExpected= ast.createCopyTarget(actualValue);
            copyOfActual= ast.createCopyTarget(expectedValue);
            localIsRewriteNeeded= true;
        }

        if (localIsRewriteNeeded) {
            Expression delta= null;

            if (ASTNodes.hasType(actualValue, double.class.getCanonicalName()) && ASTNodes.hasType(expectedValue, double.class.getCanonicalName())) {
                delta= ast.number(".0"); //$NON-NLS-1$
            } else if (ASTNodes.hasType(actualValue, float.class.getCanonicalName()) && ASTNodes.hasType(expectedValue, float.class.getCanonicalName())) {
                delta= ast.number(".0F"); //$NON-NLS-1$
            }

            MethodInvocation newAssert= invokeMethod(classesToUseWithImport, importsToAdd, ast,
                    originalMethod, getAssertName(isAssertEquals, "Equals"), copyOfActual, copyOfExpected, delta, failureMessage); //$NON-NLS-1$
            rewrite.replace(nodeToReplace, invokeMethodOrStatement(nodeToReplace, ast, newAssert), null);
            return false;
        }

        return true;
    }

    private boolean isVariableNamedExpected(final Expression expression) {
        switch (expression.getNodeType()) {
        case ASTNode.SIMPLE_NAME:
            SimpleName sn= (SimpleName) expression;
            return levenshteinDistance(sn.getIdentifier().toLowerCase(), "expected") <= 3; //$NON-NLS-1$

        case ASTNode.QUALIFIED_NAME:
            QualifiedName qn= (QualifiedName) expression;
            return isVariableNamedExpected(qn.getName());

        default:
            return false;
        }
    }

    private String getAssertName(final boolean isPositive, final String assertType) {
        return "assert" + (isPositive ? "" : "Not") + assertType; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    private boolean isComparingObjects(final InfixExpression ie) {
        return !ASTNodes.isPrimitive(ie.getLeftOperand()) || !ASTNodes.isPrimitive(ie.getRightOperand());
    }

    private ASTNode invokeMethodOrStatement(final ASTNode nodeToReplace, final ASTNodeFactory ast,
            final MethodInvocation newMethod) {
        if (nodeToReplace instanceof Statement) {
            // The new node should be also a statement
            return ast.toStatement(newMethod);
        }

        return newMethod;
    }

    private MethodInvocation invokeAssertNull(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final MethodInvocation originalMethod, final boolean isPositive, final Expression actual, final Expression failureMessage) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        String methodName= getAssertName(isPositive, "Null"); //$NON-NLS-1$
        Expression copyOfActual= ast.createCopyTarget(actual);
        return invokeMethod(classesToUseWithImport, importsToAdd, ast, originalMethod, methodName, copyOfActual, null, null, failureMessage);
    }

    /**
     * Invoke the method with full qualified name if needed.
     * @param classesToUseWithImport The classes to use with import
     * @param importsToAdd           The imports to add
     * @param ast              The builder.
     * @param originalMethod The original method.
     * @param methodName     methodName.
     * @param copyOfActual   The copy of the actual value or null.
     * @param copyOfExpected The copy of the expected value or null.
     * @param delta          The delta or null
     * @param failureMessage The original failure message or null.
     *
     * @return The method invocation object.
     */
    protected MethodInvocation invokeMethod(final Set<String> classesToUseWithImport, final Set<String> importsToAdd,
            final ASTNodeFactory ast, final MethodInvocation originalMethod, final String methodName,
            final Expression copyOfActual, final Expression copyOfExpected, final Expression delta, final Expression failureMessage) {
        String qualifiedClassName= originalMethod.resolveMethodBinding().getDeclaringClass().getQualifiedName();

        Expression qualifiedClass;
        if (originalMethod.getExpression() == null && !staticImports.contains(qualifiedClassName + "." + methodName) //$NON-NLS-1$
                && !staticImports.contains(qualifiedClassName + ".*")) { //$NON-NLS-1$
            qualifiedClass= ast.name(qualifiedClassName);
        } else {
            qualifiedClass= ast.copyExpression(originalMethod);
        }

        return invokeQualifiedMethod(classesToUseWithImport, importsToAdd, ast, qualifiedClass, methodName, copyOfActual, copyOfExpected, delta, failureMessage);
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
