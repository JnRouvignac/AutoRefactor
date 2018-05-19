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

import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.asExpression;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.ImportDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Statement;

/**
 * See {@link #getDescription()} method.
 * <p>
 * FIXME: Assert.assertNotEquals() exists only since TestNG 6.1. This
 * refactoring should be made conditional on TestNG version.
 * </p>
 */
public class TestNGAssertRefactoring extends AbstractUnitTestRefactoring {

    private boolean canUseAssertNotEquals;

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "TestNG asserts";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactors to a proper use of TestNG assertions.";
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
    protected Pair<Expression, Expression> getActualAndExpected(final Expression leftValue,
            final Expression rightValue) {
        return Pair.of(leftValue, rightValue);
    }

    @Override
    public boolean visit(CompilationUnit node) {
        // new file: reset the value
        canUseAssertNotEquals = false;
        return super.visit(node);
    }

    @Override
    public boolean visit(ImportDeclaration node) {
        if (!canUseAssertNotEquals) {
            // we have not found testng yet for this file, go on looking for it
            canUseAssertNotEquals = canUseAssertNotEquals(node);
        }
        return super.visit(node);
    }

    private boolean canUseAssertNotEquals(final ImportDeclaration node) {
        final ITypeBinding typeBinding = resolveTypeBinding(node);
        if (hasType(typeBinding, "org.testng.Assert")) {
            for (IMethodBinding mb : typeBinding.getDeclaredMethods()) {
                if (mb.toString().contains("assertNotEquals")) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    protected boolean canUseAssertNotEquals() {
        return canUseAssertNotEquals;
    }

    private ITypeBinding resolveTypeBinding(final ImportDeclaration node) {
        IBinding resolveBinding = node.resolveBinding();
        if (resolveBinding instanceof ITypeBinding) {
            return (ITypeBinding) resolveBinding;
        } else if (resolveBinding instanceof IMethodBinding) {
            return ((IMethodBinding) resolveBinding).getDeclaringClass();
        }
        return null;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final List<Expression> args = arguments(node);
        if (isMethod(node, "org.testng.Assert", "assertTrue", "boolean")) {
            return maybeRefactorStatement(node, node, true, args.get(0), null, false);
        } else if (isMethod(node, "org.testng.Assert", "assertTrue", "boolean", "java.lang.String")) {
            return maybeRefactorStatement(node, node, true, args.get(0), args.get(1), false);
        } else if (isMethod(node, "org.testng.Assert", "assertFalse", "boolean")) {
            return maybeRefactorStatement(node, node, false, args.get(0), null, false);
        } else if (isMethod(node, "org.testng.Assert", "assertFalse", "boolean", "java.lang.String")) {
            return maybeRefactorStatement(node, node, false, args.get(0), args.get(1), false);
        } else if (isMethod(node, "org.testng.Assert", "assertEquals", OBJECT, OBJECT)
                || isMethod(node, "org.testng.Assert", "assertEquals", "long", "long")
                || isMethod(node, "org.testng.Assert", "assertEquals", "double", "double")) {
            return maybeRefactorToAssertEquals(node, node, true, args.get(0), args.get(1), null, false);
        } else if (isMethod(node, "org.testng.Assert", "assertEquals", OBJECT, OBJECT, "java.lang.String")
                || isMethod(node, "org.testng.Assert", "assertEquals", "long", "long", "java.lang.String")
                || isMethod(node, "org.testng.Assert", "assertEquals", "double", "double", "java.lang.String")) {
            return maybeRefactorToAssertEquals(node, node, true, args.get(0), args.get(1), args.get(2), false);
        } else if (isMethod(node, "org.testng.Assert", "assertNotEquals", OBJECT, OBJECT)
                || isMethod(node, "org.testng.Assert", "assertNotEquals", "long", "long")
                || isMethod(node, "org.testng.Assert", "assertNotEquals", "double", "double")) {
            return maybeRefactorToAssertEquals(node, node, false, args.get(0), args.get(1), null, false);
        } else if (isMethod(node, "org.testng.Assert", "assertNotEquals", OBJECT, OBJECT, "java.lang.String")
                || isMethod(node, "org.testng.Assert", "assertNotEquals", "long", "long", "java.lang.String")
                || isMethod(node, "org.testng.Assert", "assertNotEquals", "double", "double", "java.lang.String")) {
            return maybeRefactorToAssertEquals(node, node, false, args.get(0), args.get(1), args.get(2), false);
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(IfStatement node) {
        final List<Statement> stmts = asList(node.getThenStatement());
        if (node.getElseStatement() == null && stmts.size() == 1) {
            final MethodInvocation mi = asExpression(stmts.get(0), MethodInvocation.class);
            if (isMethod(mi, "org.testng.Assert", "fail")) {
                return maybeRefactorStatement(node, mi, false, node.getExpression(), null, true);
            } else if (isMethod(mi, "org.testng.Assert", "fail", "java.lang.String")) {
                return maybeRefactorStatement(node, mi, false, node.getExpression(), arguments(mi).get(0), true);
            }
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
                return b.invoke(copyOfExpr, methodName, copyOfActual, copyOfExpected);
            }
        } else if (copyOfActual == null) {
            return b.invoke(copyOfExpr, methodName, b.copy(failureMessage));
        } else if (copyOfExpected == null) {
            return b.invoke(copyOfExpr, methodName, copyOfActual, b.copy(failureMessage));
        } else {
            return b.invoke(copyOfExpr, methodName, copyOfActual, copyOfExpected, b.copy(failureMessage));
        }
    }
}
