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

import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class ArrayListRatherThanLinkedListRefactoring extends AbstractClassSubstituteRefactoring {
    @Override
    public String getDescription() {
        return ""
            + "Replace LinkedList by ArrayList when no item is inserted or removed in the middle of the list.";
    }

    @Override
    public String getName() {
        return "ArrayList rather than LinkedList";
    }

    @Override
    protected String getExistingClassCanonicalName() {
        return "java.util.LinkedList";
    }

    @Override
    protected String getSubstitutingClassName() {
        return "java.util.ArrayList";
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        return isMethod(mi, "java.util.LinkedList", "add", "java.lang.Object")
                || isMethod(mi, "java.util.List", "addAll", "java.util.Collection")
                || isMethod(mi, "java.util.List", "clear")
                || isMethod(mi, "java.util.List", "contains", "java.lang.Object")
                || isMethod(mi, "java.util.List", "containsAll", "java.util.Collection")
                || isMethod(mi, "java.util.List", "equals", "java.lang.Object")
                || isMethod(mi, "java.util.List", "get", "int")
                || isMethod(mi, "java.util.List", "hashCode")
                || isMethod(mi, "java.util.List", "indexOf", "java.lang.Object")
                || isMethod(mi, "java.util.List", "lastIndexOf", "java.lang.Object")
                || isMethod(mi, "java.util.List", "size")
                || isMethod(mi, "java.util.List", "sort", "java.util.Comparator")
                || isMethod(mi, "java.util.List", "subList", "int", "int")
                || isMethod(mi, "java.util.List", "toArray")
                || isMethod(mi, "java.util.LinkedList", "toArray", "java.lang.Object[]")
                || isMethod(mi, "java.util.AbstractCollection", "isEmpty")
                || isMethod(mi, "java.util.AbstractCollection", "toString")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int");
    }
}
