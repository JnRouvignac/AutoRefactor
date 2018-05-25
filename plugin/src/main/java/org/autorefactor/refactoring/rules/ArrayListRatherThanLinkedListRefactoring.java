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

import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class ArrayListRatherThanLinkedListRefactoring extends AbstractClassSubstituteRefactoring {
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
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "ArrayList rather than LinkedList";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace LinkedList by ArrayList when no item is inserted or removed in the middle of the list.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the time and space performance.";
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.LinkedList"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if ("java.util.LinkedList".equals(origRawType)) {
            return "java.util.ArrayList";
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

        return isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "addAll", "java.util.Collection")
                || isMethod(mi, "java.util.Collection", "clear")
                || isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "containsAll", "java.util.Collection")
                || isMethod(mi, "java.lang.Object", "equals", "java.lang.Object")
                || isMethod(mi, "java.util.List", "get", "int")
                || isMethod(mi, "java.lang.Object", "hashCode")
                || isMethod(mi, "java.util.List", "indexOf", "java.lang.Object")
                || isMethod(mi, "java.util.List", "lastIndexOf", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "size")
                || isMethod(mi, "java.util.List", "subList", "int", "int")
                || isMethod(mi, "java.util.Collection", "toArray")
                || isMethod(mi, "java.util.Collection", "toArray", argumentType + "[]")
                || isMethod(mi, "java.util.Collection", "isEmpty")
                || isMethod(mi, "java.lang.Object", "toString")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int");
    }

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType,
            final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType)
                || hasType(variableType,
                           getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}
