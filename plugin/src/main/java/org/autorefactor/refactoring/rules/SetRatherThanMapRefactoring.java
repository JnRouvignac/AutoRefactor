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
import org.autorefactor.refactoring.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.Type;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class SetRatherThanMapRefactoring extends AbstractClassSubstituteRefactoring {
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
        CAN_BE_CASTED_TO.put("java.util.HashMap",
                new String[]{"java.util.HashMap", "java.io.Serializable", "java.util.Map",
                    "java.util.AbstractMap", "java.lang.Cloneable", "java.lang.Object"});
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Set rather than map";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace map by set when values are not read.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It simplify the code showing that the values are not used.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 2;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.HashMap", "java.util.TreeMap"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        if ("java.util.HashMap".equals(origRawType)) {
            return "java.util.HashSet";
        } else if ("java.util.TreeMap".equals(origRawType)) {
            return "java.util.TreeSet";
        } else if ("java.util.AbstractMap".equals(origRawType)) {
            return "java.util.AbstractSet";
        } else if ("java.util.Map".equals(origRawType)) {
            return "java.util.Set";
        } else {
            return null;
        }
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        ITypeBinding[] parameterTypes = instanceCreation.resolveConstructorBinding().getParameterTypes();

        return parameterTypes.length == 0 || hasType(parameterTypes[0], "int");
    }

    /**
     * Returns the substitute type.
     *
     * @param b The builder.
     * @param origType The original type
     * @param originalExpr The original expression
     * @return the substitute type.
     */
    @Override
    protected Type substituteType(final ASTBuilder b, final Type origType, ASTNode originalExpr) {
        String substitutingType = getSubstitutingClassName(origType.resolveBinding().getErasure().getQualifiedName());

        final ITypeBinding origTypeBinding = origType.resolveBinding();
        final TypeNameDecider typeNameDecider = new TypeNameDecider(originalExpr);

        if (origTypeBinding.isParameterizedType()) {
            final ITypeBinding[] origTypeArgs = origTypeBinding.getTypeArguments();
            final Type[] newTypes;
            if (origTypeArgs.length > 0) {
                newTypes = new Type[1];
                newTypes[0] = b.toType(origTypeArgs[0], typeNameDecider);
            } else {
                newTypes = new Type[0];
            }
            return b.genericType(substitutingType,
                    newTypes);
        }

        return b.type(substitutingType);
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (isMethod(mi, "java.util.Map", "clear")
                || isMethod(mi, "java.util.Map", "isEmpty")
                || isMethod(mi, "java.util.Map", "size")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int")) {
            return true;
        } else if (isMethod(mi, "java.util.Map", "containsKey", "java.lang.Object")) {
            methodCallsToRefactor.add(mi);
            return true;
        } else if (isMethod(mi, "java.util.Map", "put", "java.lang.Object", "java.lang.Object")) {
            if (isPassive((Expression) mi.arguments().get(1))) {
                methodCallsToRefactor.add(mi);
                return true;
            } else {
                return false;
            }
        } else if (isMethod(mi, "java.util.Map", "remove", "java.lang.Object")) {
            return isReturnValueLost(mi);
        } else {
            // Here are the following cases:
            //
            // HashMap.clone()
            // HashMap.containsValue(Object)
            // HashMap.values()
            // HashMap.entrySet()
            // AbstractMap.equals(Object)
            // HashMap.get(Object)
            // AbstractMap.hashCode()
            // AbstractMap.toString()
            // HashMap.keySet()
            // HashMap.putAll(Map)
            return false;
        }
    }

    private boolean isReturnValueLost(final ASTNode node) {
        ASTNode parentNode = node.getParent();

        if (parentNode instanceof ExpressionStatement) {
            return true;
        } else if (parentNode instanceof ParenthesizedExpression) {
            return isReturnValueLost(parentNode);
        } else {
            return false;
        }
    }

    @Override
    protected void refactorMethod(final ASTBuilder b, final MethodInvocation originalMi,
            final MethodInvocation refactoredMi) {
        if (isMethod(originalMi, "java.util.Map", "containsKey", "java.lang.Object")) {
            refactoredMi.setName(b.simpleName("contains"));
        } else if (isMethod(originalMi, "java.util.Map", "put", "java.lang.Object", "java.lang.Object")) {
            refactoredMi.setName(b.simpleName("add"));
            refactoredMi.arguments().remove(1);
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
