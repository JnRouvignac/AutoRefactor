/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
 * Copyright (C) 2018 Jean-Noël Rouvignac - minor changes
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
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class SetRatherThanListRefactoring extends AbstractClassSubstituteRefactoring {
    private static final Map<String, String[]> CAN_BE_CASTED_TO = new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put("java.lang.Object", new String[]{"java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.lang.Cloneable", new String[]{"java.lang.Cloneable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.io.Serializable",
                new String[]{"java.io.Serializable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.Collection", new String[]{"java.util.Collection", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.List", new String[]{"java.util.List", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.AbstractList",
                new String[]{"java.util.AbstractList", "java.util.List", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.AbstractCollection",
                new String[]{"java.util.AbstractCollection", "java.util.Collection", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.LinkedList",
                new String[]{"java.util.LinkedList", "java.util.AbstractList", "java.util.List",
                    "java.util.AbstractCollection", "java.util.Collection",
                    "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.ArrayList",
                new String[]{"java.util.ArrayList", "java.util.AbstractList", "java.util.List",
                    "java.util.AbstractCollection", "java.util.Collection",
                    "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object"});
    }

    private boolean isContainsMethodUsed = false;

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Set rather than List";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace List by HashSet when only presence of items is used.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the space performance.";
    }

    @Override
    public boolean visit(Block node) {
        isContainsMethodUsed = false;
        return super.visit(node);

    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.ArrayList", "java.util.LinkedList"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if ("java.util.ArrayList".equals(origRawType) || "java.util.LinkedList".equals(origRawType)) {
            return "java.util.HashSet";
        } else if ("java.util.AbstractList".equals(origRawType) || "java.util.List".equals(origRawType)) {
            return "java.util.Set";
        } else {
            return null;
        }
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
        if (isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")) {
            isContainsMethodUsed = true;
        }

        if (isMethod(mi, "java.util.List", "add", "int", "java.lang.Object")
                || isMethod(mi, "java.util.List", "addAll", "int", "java.util.Collection")) {
            methodCallsToRefactor.add(mi);
            return true;
        }

        return isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "addAll", "java.util.Collection")
                || isMethod(mi, "java.util.Collection", "clear")
                || isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "isEmpty")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int");
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void refactorMethod(final ASTBuilder b, final MethodInvocation originalMi,
            final MethodInvocation refactoredMi) {
        if (isMethod(originalMi, "java.util.List", "add", "int", "java.lang.Object")
                || isMethod(originalMi, "java.util.List", "addAll", "int", "java.util.Collection")) {
            Object item = refactoredMi.arguments().get(1);
            refactoredMi.arguments().clear();
            refactoredMi.arguments().add(item);
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
