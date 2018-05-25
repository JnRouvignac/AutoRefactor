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

import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class HashMapRatherThanTreeMapRefactoring extends AbstractClassSubstituteRefactoring {
    private static final Map<String, String[]> CAN_BE_CASTED_TO = new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put("java.lang.Object", new String[]{"java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.lang.Cloneable", new String[]{"java.lang.Cloneable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.io.Serializable",
                new String[]{"java.io.Serializable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.Map", new String[]{"java.util.Map", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.AbstractMap",
                new String[]{"java.util.AbstractMap", "java.lang.Cloneable", "java.lang.Object"});
        CAN_BE_CASTED_TO.put("java.util.TreeMap",
                new String[]{"java.util.TreeMap", "java.io.Serializable", "java.util.Map",
                    "java.util.AbstractMap", "java.lang.Cloneable", "java.lang.Object"});
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "HashMap rather than TreeMap";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace TreeMap by HashMap when the entry order is not used.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the time and the space performance.";
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.TreeMap"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if ("java.util.TreeMap".equals(origRawType)) {
            return "java.util.HashMap";
        } else {
            return null;
        }
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        return instanceCreation.arguments().size() != 1
                || !hasType(((Expression) instanceCreation.arguments().get(0)), "java.util.Comparator");
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        return isMethod(mi, "java.util.Map", "clear")
                || isMethod(mi, "java.util.Map", "containsKey", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "containsValue", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "get", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "getOrDefault", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "isEmpty")
                || isMethod(mi, "java.util.Map", "put", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "putAll", "java.util.Map")
                || isMethod(mi, "java.util.Map", "putIfAbsent", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "remove", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "remove", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "replace", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "replace", "java.lang.Object", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "size")
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
