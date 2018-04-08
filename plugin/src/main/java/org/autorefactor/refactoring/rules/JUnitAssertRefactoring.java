/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;

/**
 * See {@link #getDescription()} method.
 */
public class JUnitAssertRefactoring extends AbstractUnitTestRefactoring {
    private static final String[] PACKAGE_PATHES = new String[] { "junit.framework.", "org.junit." };

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "JUnit asserts";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactors to a proper use of JUnit assertions.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility of the code and the report.";
    }

    @Override
    protected boolean canUseAssertNotEquals() {
        return false;
    }

    @Override
    protected Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
            final Expression rightValue) {
        return Pair.of(rightValue, leftValue);
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final List<Expression> args = arguments(node);
        int i = 0;
        boolean shouldVisit = VISIT_SUBTREE;
        while (shouldVisit == VISIT_SUBTREE && i < PACKAGE_PATHES.length) {
            shouldVisit = maybeRefactorMethod(node, PACKAGE_PATHES[i], args);
            i++;
        }
        return shouldVisit;
    }

    private boolean maybeRefactorMethod(final MethodInvocation node, final String unitTestPackagePath,
            final List<Expression> args) {
        if (isMethod(node, unitTestPackagePath + "Assert", "assertTrue", "boolean")) {
            return maybeRefactorStatement(node, node, true, args.get(0), null, false);
        } else if (isMethod(node, unitTestPackagePath + "Assert", "assertTrue", "java.lang.String", "boolean")) {
            return maybeRefactorStatement(node, node, true, args.get(1), args.get(0), false);
        } else if (isMethod(node, unitTestPackagePath + "Assert", "assertFalse", "boolean")) {
            return maybeRefactorStatement(node, node, false, args.get(0), null, false);
        } else if (isMethod(node, unitTestPackagePath + "Assert", "assertFalse", "java.lang.String", "boolean")) {
            return maybeRefactorStatement(node, node, false, args.get(1), args.get(0), false);
        } else if (isMethod(node, unitTestPackagePath + "Assert", "assertEquals", OBJECT, OBJECT)
                || isMethod(node, unitTestPackagePath + "Assert", "assertEquals", "long", "long")
                || isMethod(node, unitTestPackagePath + "Assert", "assertEquals", "double", "double")) {
            return maybeRefactorToAssertEquals(node, node, true, args.get(1), args.get(0), null, false);
        } else if (isMethod(node, unitTestPackagePath + "Assert", "assertEquals", "java.lang.String", OBJECT, OBJECT)
                || isMethod(node, unitTestPackagePath + "Assert", "assertEquals", "java.lang.String", "long", "long")
                || isMethod(node, unitTestPackagePath + "Assert", "assertEquals", "java.lang.String", "double",
                        "double")) {
            return maybeRefactorToAssertEquals(node, node, true, args.get(2), args.get(1), args.get(0), false);
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(IfStatement node) {
        final List<Statement> stmts = asList(node.getThenStatement());
        if (node.getElseStatement() == null && stmts.size() == 1) {
            final MethodInvocation mi = asExpression(stmts.get(0), MethodInvocation.class);
            int i = 0;
            boolean shouldVisit = VISIT_SUBTREE;
            while (shouldVisit == VISIT_SUBTREE && i < PACKAGE_PATHES.length) {
                shouldVisit = maybeRefactorIf(node, mi, PACKAGE_PATHES[i]);
                i++;
            }
            return shouldVisit;
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorIf(final IfStatement node, final MethodInvocation mi,
            final String unitTestPackagePath) {
        if (isMethod(mi, unitTestPackagePath + "Assert", "fail")) {
            return maybeRefactorStatement(node, mi, false, node.getExpression(), null, true);
        } else if (isMethod(mi, unitTestPackagePath + "Assert", "fail", "java.lang.String")) {
            return maybeRefactorStatement(node, mi, false, node.getExpression(), arguments(mi).get(0), true);
        }
        return VISIT_SUBTREE;
    }

    @Override
    protected MethodInvocation invokeQualifiedMethod(final ASTBuilder b, final Expression copyOfExpr,
            final String methodName,
            final Expression copyOfActual, final Expression copyOfExpected, final Expression failureMessage) {
        if (failureMessage == null) {
            if (copyOfActual == null) {
                return b.invoke(copyOfExpr, methodName);
            } else if (copyOfExpected == null) {
                return b.invoke(copyOfExpr, methodName, copyOfActual);
            } else {
                return b.invoke(copyOfExpr, methodName, copyOfExpected, copyOfActual);
            }
        } else if (copyOfActual == null) {
            return b.invoke(copyOfExpr, methodName, b.copy(failureMessage));
        } else if (copyOfExpected == null) {
            return b.invoke(copyOfExpr, methodName, b.copy(failureMessage), copyOfActual);
        } else {
            return b.invoke(copyOfExpr, methodName, b.copy(failureMessage), copyOfExpected, copyOfActual);
        }
    }
}
