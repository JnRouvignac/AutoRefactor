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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;

/**
 * See {@link #getDescription()} method.
 */
public class JUnitAssertCleanUp extends AbstractUnitTestCleanUp {
    private static final String[] PACKAGE_PATHES= { "junit.framework.", "org.junit." }; //$NON-NLS-1$ $NON-NLS-2$

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_JUnitAssertCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_JUnitAssertCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_JUnitAssertCleanUp_reason;
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
        final List<Expression> args= ASTNodes.arguments(node);
        int i= 0;
        boolean shouldVisit= true;
        while (shouldVisit && i < PACKAGE_PATHES.length) {
            shouldVisit= maybeRefactorMethod(node, PACKAGE_PATHES[i], args);
            i++;
        }
        return shouldVisit;
    }

    private boolean maybeRefactorMethod(final MethodInvocation node, final String unitTestPackagePath,
            final List<Expression> args) {
        if (ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertTrue", boolean.class.getSimpleName())) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorStatement(node, node, true, args.get(0), null, false);
        }
        if (ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertTrue", String.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorStatement(node, node, true, args.get(1), args.get(0), false);
        }
        if (ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertFalse", boolean.class.getSimpleName())) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorStatement(node, node, false, args.get(0), null, false);
        }
        if (ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertFalse", String.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorStatement(node, node, false, args.get(1), args.get(0), false);
        }
        if (ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertEquals", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$ $NON-NLS-2$
                || ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertEquals", long.class.getSimpleName(), long.class.getSimpleName()) //$NON-NLS-1$ $NON-NLS-2$
                || ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertEquals", double.class.getSimpleName(), double.class.getSimpleName())) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorToAssertEquals(node, node, true, args.get(1), args.get(0), null, false);
        }
        if (ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertEquals", String.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$ $NON-NLS-2$
                || ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertEquals", String.class.getCanonicalName(), long.class.getSimpleName(), long.class.getSimpleName()) //$NON-NLS-1$ $NON-NLS-2$
                || ASTNodes.usesGivenSignature(node, unitTestPackagePath + "Assert", "assertEquals", String.class.getCanonicalName(), double.class.getSimpleName(), //$NON-NLS-1$ $NON-NLS-2$
                        double.class.getSimpleName())) {
            return maybeRefactorToAssertEquals(node, node, true, args.get(2), args.get(1), args.get(0), false);
        }
        return true;
    }

    @Override
    public boolean visit(IfStatement node) {
        final List<Statement> statements= ASTNodes.asList(node.getThenStatement());
        if (node.getElseStatement() == null && statements.size() == 1) {
            final MethodInvocation mi= ASTNodes.asExpression(statements.get(0), MethodInvocation.class);
            int i= 0;
            boolean shouldVisit= true;
            while (shouldVisit && i < PACKAGE_PATHES.length) {
                shouldVisit= maybeRefactorIf(node, mi, PACKAGE_PATHES[i]);
                i++;
            }
            return shouldVisit;
        }
        return true;
    }

    private boolean maybeRefactorIf(final IfStatement node, final MethodInvocation mi,
            final String unitTestPackagePath) {
        if (ASTNodes.usesGivenSignature(mi, unitTestPackagePath + "Assert", "fail")) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorStatement(node, mi, false, node.getExpression(), null, true);
        }
        if (ASTNodes.usesGivenSignature(mi, unitTestPackagePath + "Assert", "fail", String.class.getCanonicalName())) { //$NON-NLS-1$ $NON-NLS-2$
            return maybeRefactorStatement(node, mi, false, node.getExpression(), ASTNodes.arguments(mi).get(0), true);
        }
        return true;
    }

    @Override
    protected MethodInvocation invokeQualifiedMethod(final ASTNodeFactory b, final Expression copyOfExpression,
            final String methodName, final Expression copyOfActual, final Expression copyOfExpected,
            final Expression failureMessage) {
        if (failureMessage == null) {
            if (copyOfActual == null) {
                return b.invoke(copyOfExpression, methodName);
            }
            if (copyOfExpected == null) {
                return b.invoke(copyOfExpression, methodName, copyOfActual);
            }
            return b.invoke(copyOfExpression, methodName, copyOfExpected, copyOfActual);
        }
        if (copyOfActual == null) {
            return b.invoke(copyOfExpression, methodName, b.copy(failureMessage));
        }
        if (copyOfExpected == null) {
            return b.invoke(copyOfExpression, methodName, b.copy(failureMessage), copyOfActual);
        }
        return b.invoke(copyOfExpression, methodName, b.copy(failureMessage), copyOfExpected, copyOfActual);
    }
}
