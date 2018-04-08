/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.eclipse.jdt.core.dom.ASTNode.EXPRESSION_STATEMENT;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class MethodOnMapRatherThanMethodOnKeySetRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Method on map rather than method on keyset";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
                + "Directly invoke methods on Map rather than on Map.keySet() when possible.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading and debugging cost."
                + " It also improves the time and the space performance.";
    }

    @Override
    public boolean visit(MethodInvocation mi) {
        Expression miExpr = mi.getExpression();
        if (isKeySetMethod(miExpr)) {
            final MethodInvocation mapKeySetMi = (MethodInvocation) miExpr;
            if (isMethod(mi, "java.util.Set", "clear")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "clear");
            }
            if (isMethod(mi, "java.util.Set", "size")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "size");
            }
            if (isMethod(mi, "java.util.Set", "isEmpty")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "isEmpty");
            }
            if (isMethod(mi, "java.util.Set", "remove", "java.lang.Object")
                    // If parent is not an expression statement, the MethodInvocation must return a boolean.
                    // In that case, we cannot replace because `Map.removeKey(key) != null`
                    // is not strictly equivalent to `Map.keySet().remove(key)`
                    && mi.getParent().getNodeType() == EXPRESSION_STATEMENT) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "remove");
            }
            if (isMethod(mi, "java.util.Set", "contains", "java.lang.Object")) {
                return removeInvocationOfMapKeySet(mapKeySetMi, mi, "containsKey");
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean removeInvocationOfMapKeySet(
            MethodInvocation mapKeySetMi, MethodInvocation actualMi, String methodName) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(
            actualMi,
            b.invoke(
                b.copyExpression(mapKeySetMi),
                methodName,
                b.copyRange(arguments(actualMi))));
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean isKeySetMethod(Expression expr) {
        return expr instanceof MethodInvocation
            && isMethod((MethodInvocation) expr, "java.util.Map", "keySet");
    }
}
