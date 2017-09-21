/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class SetRatherThanListRefactoring extends AbstractClassSubstituteRefactoring {

    private boolean isContainsMethodUsed = false;

    @Override
    public String getDescription() {
        return ""
            + "Replace List by HashSet when only presence of items is used.";
    }

    @Override
    public String getName() {
        return "Set rather than List";
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.ArrayList"};
    }

    @Override
    protected String getSubstitutingClassName() {
        return "java.util.HashSet";
    }

    @Override
    protected boolean canInvokeIterator() {
        return false;
    }

    @Override
    protected boolean canCodeBeRefactored() {
        return isContainsMethodUsed;
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (isMethod(mi, "java.util.List", "contains", "java.lang.Object")) {
            isContainsMethodUsed = true;
        }

        if (isMethod(mi, "java.util.List", "add", "int", "java.lang.Object")
                || isMethod(mi, "java.util.List", "addAll", "int", "java.util.Collection")) {
            methodCallsToRefactor.add(mi);
            return true;
        }

        return isMethod(mi, "java.util.List", "add", "java.lang.Object")
                || isMethod(mi, "java.util.List", "addAll", "java.util.Collection")
                || isMethod(mi, "java.util.List", "clear")
                || isMethod(mi, "java.util.List", "contains", "java.lang.Object")
                || isMethod(mi, "java.util.List", "isEmpty")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int");
    }

    @Override
    protected void refactorMethod(final ASTBuilder b, final MethodInvocation originalMi,
            final MethodInvocation refactoredMi) {
        if (isMethod(originalMi, "java.util.List", "add", "int", "java.lang.Object")
                || isMethod(originalMi, "java.util.List", "addAll", "int", "java.util.Collection")) {
            Object item = refactoredMi.arguments().get(1);
            refactoredMi.arguments().clear();
            refactoredMi.arguments().add(item);

            if (refactoredMi.typeArguments() != null && !refactoredMi.typeArguments().isEmpty()) {
                Object itemType = refactoredMi.typeArguments().get(1);
                refactoredMi.typeArguments().clear();
                refactoredMi.typeArguments().add(itemType);
            }
        }
    }

    @Override
    public boolean visit(Block node) {
        isContainsMethodUsed = false;
        return super.visit(node);

    }
}
