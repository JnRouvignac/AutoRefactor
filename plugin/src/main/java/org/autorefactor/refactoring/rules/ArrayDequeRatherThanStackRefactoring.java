/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017 Jean-Noël Rouvignac - minor changes
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
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class ArrayDequeRatherThanStackRefactoring extends AbstractClassSubstituteRefactoring {
    @Override
    public String getDescription() {
        return ""
            + "Replace Stack by ArrayDeque when possible.";
    }

    @Override
    public String getName() {
        return "ArrayDeque rather than Stack";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 6;
    }

    @Override
    protected String getExistingClassCanonicalName() {
        return "java.util.Stack";
    }

    @Override
    protected String getSubstitutingClassName() {
        return "java.util.ArrayDeque";
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (isMethod(mi, "java.util.Vector", "addElement", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "copyInto", "java.lang.Object[]")
                || isMethod(mi, "java.util.Vector", "firstElement")
                || isMethod(mi, "java.util.Vector", "lastElement")
                || isMethod(mi, "java.util.Vector", "removeElement", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "removeAllElements")
                || isMethod(mi, "java.util.Stack", "empty")) {
            methodCallsToRefactor.add(mi);
            return true;
        }
        if (isMethod(mi, "java.util.Vector", "add", "int", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "addAll", "int", "java.util.Collection")
                || isMethod(mi, "java.util.Vector", "setSize", "int")
                || isMethod(mi, "java.util.Vector", "capacity")
                || isMethod(mi, "java.util.Vector", "elementAt", "int")
                || isMethod(mi, "java.util.Vector", "elements")
                || isMethod(mi, "java.util.Vector", "get", "int")
                || isMethod(mi, "java.util.Vector", "indexOf", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "indexOf", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "lastIndexOf", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "lastIndexOf", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "listIterator")
                || isMethod(mi, "java.util.Vector", "listIterator", "int")
                || isMethod(mi, "java.util.Vector", "insertElementAt", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "remove", "int")
                || isMethod(mi, "java.util.Vector", "removeElementAt", "int")
                || isMethod(mi, "java.util.Stack", "search", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "set", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "setElementAt", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "subList", "int", "int")) {
            return false;
        }
        return true;
    }

    @Override
    protected void refactorMethod(final ASTBuilder b, final MethodInvocation originalMi,
            final MethodInvocation refactoredMi) {
        if (isMethod(originalMi, "java.util.Vector", "addElement", "java.lang.Object")) {
            refactoredMi.setName(b.simpleName("add"));
        } else if (isMethod(originalMi, "java.util.Vector", "copyInto", "java.lang.Object[]")) {
            refactoredMi.setName(b.simpleName("toArray"));
        } else if (isMethod(originalMi, "java.util.Vector", "firstElement")) {
            refactoredMi.setName(b.simpleName("getFirst"));
        } else if (isMethod(originalMi, "java.util.Vector", "lastElement")) {
            refactoredMi.setName(b.simpleName("getLast"));
        } else if (isMethod(originalMi, "java.util.Vector", "removeElement", "java.lang.Object")) {
            refactoredMi.setName(b.simpleName("remove"));
        } else if (isMethod(originalMi, "java.util.Vector", "removeAllElements")) {
            refactoredMi.setName(b.simpleName("clear"));
        } else if (isMethod(originalMi, "java.util.Stack", "empty")) {
            refactoredMi.setName(b.simpleName("isEmpty"));
        }
    }
}
