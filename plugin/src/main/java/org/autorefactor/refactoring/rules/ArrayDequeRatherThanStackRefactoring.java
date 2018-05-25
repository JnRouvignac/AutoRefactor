/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017-2018 Jean-Noël Rouvignac - minor changes
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class ArrayDequeRatherThanStackRefactoring extends AbstractClassSubstituteRefactoring {
    private static final Map<String, String[]> CAN_BE_CASTED_TO = new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put("java.lang.Object", new String[]{"java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.lang.Cloneable", new String[]{"java.lang.Cloneable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.io.Serializable",
                new String[]{"java.io.Serializable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.Collection", new String[]{"java.util.Collection", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.AbstractCollection",
                new String[]{"java.util.AbstractCollection", "java.util.Collection", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.Vector",
                new String[]{"java.util.Vector",
                    "java.util.AbstractCollection", "java.util.Collection",
                    "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.Stack",
                new String[]{"java.util.Stack", "java.util.Vector",
                    "java.util.AbstractCollection", "java.util.Collection",
                    "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object"});
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the time performance.";
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "ArrayDeque rather than Stack";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace Stack by ArrayDeque when possible.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 6;
    }

    @Override
    protected boolean canBeSharedInOtherThread() {
        return false;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.Stack"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if ("java.util.Stack".equals(origRawType)) {
            return "java.util.ArrayDeque";
        } else if ("java.util.Vector".equals(origRawType)) {
            return "java.util.Collection";
        } else {
            return null;
        }
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        final String argumentType;
        if (mi.getExpression().resolveTypeBinding().getTypeArguments() != null
                        && mi.getExpression().resolveTypeBinding().getTypeArguments().length == 1) {
            argumentType = mi.getExpression().resolveTypeBinding().getTypeArguments()[0].getQualifiedName();
        } else {
            argumentType = "java.lang.Object";
        }

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

        if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                || isMethod(mi, "java.util.List", "addAll", "int", "java.util.Collection")
                || isMethod(mi, "java.util.Collection", "clear")
                || isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "containsAll", "java.util.Collection")
                || isMethod(mi, "java.lang.Object", "equals", "java.lang.Object")
                || isMethod(mi, "java.lang.Object", "hashCode")
                || isMethod(mi, "java.util.Collection", "isEmpty")
                || isMethod(mi, "java.util.Collection", "iterator")
                || isMethod(mi, "java.util.Collection", "remove", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "removeAll", "java.util.Collection")
                || isMethod(mi, "java.util.Collection", "retainAll", "java.util.Collection")
                || isMethod(mi, "java.util.Collection", "size")
                || isMethod(mi, "java.util.Collection", "toArray")
                || isMethod(mi, "java.util.Collection", "toArray", argumentType + "[]")
                || isMethod(mi, "java.util.Stack", "clone")
                || isMethod(mi, "java.util.Stack", "retainAll", "java.util.Collection")
                || isMethod(mi, "java.lang.Object", "toString")
                || isMethod(mi, "java.util.Stack", "peek")
                || isMethod(mi, "java.util.Stack", "pop")
                || isMethod(mi, "java.util.Stack", "push", "java.lang.Object")) {
            return true;
        }

        return false;
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

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType,
            final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType)
                || hasType(variableType,
                           getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}
