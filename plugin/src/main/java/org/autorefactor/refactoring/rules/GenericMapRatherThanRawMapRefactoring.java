/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.getBoxedTypeBinding;
import static org.autorefactor.refactoring.ASTHelper.getDestinationType;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.util.Utils.getOrDefault;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public class GenericMapRatherThanRawMapRefactoring extends AbstractClassSubstituteRefactoring {
    private static final Map<String, String[]> CAN_BE_CASTED_TO = new HashMap<String, String[]>();

    static {
        CAN_BE_CASTED_TO.put("java.lang.Object", new String[] { "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.lang.Cloneable", new String[] { "java.lang.Cloneable", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.io.Serializable", new String[] { "java.io.Serializable", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.util.Map", new String[] { "java.util.Map", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.util.AbstractMap",
                new String[] { "java.util.AbstractMap", "java.util.Map", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.util.TreeMap",
                new String[] { "java.util.TreeMap", "java.util.AbstractMap",
                    "java.util.Map", "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.util.HashMap",
                new String[] { "java.util.HashMap", "java.util.AbstractMap",
                    "java.util.Map", "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.util.EnumMap",
                new String[] { "java.util.EnumMap", "java.util.AbstractMap",
                    "java.util.Map", "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object" });
        CAN_BE_CASTED_TO.put("java.util.Hashtable",
                new String[] { "java.util.Hashtable", "java.util.AbstractMap",
                    "java.util.Map", "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object" });
    }

    private ITypeBinding keyType;

    private ITypeBinding valueType;

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Generic map rather than raw map";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "" + "Genericize a map if possible.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the bug hazard.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 5;
    }

    @Override
    public boolean visit(Block node) {
        keyType = null;
        valueType = null;
        return super.visit(node);

    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {
            "java.util.jar.Attributes",
            "java.security.AuthProvider",
            "java.util.concurrent.ConcurrentHashMap",
            "java.util.concurrent.ConcurrentSkipListMap",
            "java.util.EnumMap",
            "java.util.HashMap",
            "java.util.Hashtable",
            "java.util.IdentityHashMap",
            "java.util.LinkedHashMap",
            "javax.print.attribute.standard.PrinterStateReasons",
            "java.util.Properties",
            "java.security.Provider",
            "java.awt.RenderingHints",
            "javax.script.SimpleBindings",
            "javax.management.openmbean.TabularDataSupport",
            "java.util.TreeMap",
            "javax.swing.UIDefaults",
            "java.util.WeakHashMap"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        return origRawType;
    }

    /**
     * Returns the substitute type.
     *
     * @param b            The builder.
     * @param origType     The original type
     * @param originalExpr The original expression
     * @return the substitute type.
     */
    @SuppressWarnings("unchecked")
    @Override
    protected Type substituteType(final ASTBuilder b, final Type origType, ASTNode originalExpr) {
        if (origType.isParameterizedType()) {
            return null;
        }

        final TypeNameDecider typeNameDecider = new TypeNameDecider(originalExpr);

        final ParameterizedType parameterizedType = b.getAST().newParameterizedType(b.copy(origType));
        parameterizedType.typeArguments().clear();
        parameterizedType.typeArguments().add(b.toType(keyType, typeNameDecider));
        parameterizedType.typeArguments().add(b.toType(valueType, typeNameDecider));

        return parameterizedType;
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        keyType = null;
        valueType = null;

        if (instanceCreation.resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        ITypeBinding[] parameterTypes = instanceCreation.resolveConstructorBinding().getParameterTypes();

        if (parameterTypes.length > 0 && hasType(parameterTypes[0], "java.util.Map")) {
            ITypeBinding actualParameter = ((Expression) instanceCreation.arguments().get(0)).resolveTypeBinding();

            if (actualParameter.isParameterizedType() && actualParameter.getTypeArguments().length == 2) {
                ITypeBinding newKeyType = actualParameter.getTypeArguments()[0];
                ITypeBinding newValueType = actualParameter.getTypeArguments()[1];
                return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
            }
        }
        return true;
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (mi.getExpression().resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        if (isMethod(mi, "java.lang.Object", "equals", "java.lang.Object")
                || isMethod(mi, "java.lang.Object", "toString")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "size")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int")
                || isMethod(mi, "java.util.Map", "clear")
                || isMethod(mi, "java.util.Map", "containsKey", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "containsValue", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "equals", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "forEach", "java.util.function.BiConsumer")
                || isMethod(mi, "java.util.Map", "hashCode")
                || isMethod(mi, "java.util.Map", "isEmpty")
                || isMethod(mi, "java.util.Map", "size")
                || isMethod(mi, "java.util.Map", "remove", "java.lang.Object", "java.lang.Object")) {
            return true;
        } else if (isMethod(mi, "java.util.Map", "ofEntries", "java.util.Map.Entry[]")) {
            final ITypeBinding paramType = ((Expression) mi.arguments().get(0)).resolveTypeBinding().getElementType();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 2) {
                final ITypeBinding newKeyType = paramType.getTypeArguments()[0];
                final ITypeBinding newValueType = paramType.getTypeArguments()[1];
                return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
            }
        } else if (isMethod(mi, "java.util.Map", "putAll", "java.util.Map")
                || isMethod(mi, "java.util.LinkedHashMap", "removeEldestEntry", "java.util.Map.Entry")) {
            final ITypeBinding paramType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 2) {
                final ITypeBinding newKeyType = paramType.getTypeArguments()[0];
                final ITypeBinding newValueType = paramType.getTypeArguments()[1];
                return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
            }
        } else if (isMethod(mi, "java.util.TreeMap", "lastKey")
                || isMethod(mi, "java.util.TreeMap", "firstKey")) {
            return resolveDestinationTypeCompatibleWithKey(mi);
        } else if (isMethod(mi, "java.util.Map", "get", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "remove", "java.lang.Object")) {
            return resolveDestinationTypeCompatibleWithValue(mi);
        } else if (isMethod(mi, "java.util.Map", "keySet")
                || isMethod(mi, "java.util.TreeMap", "comparator")
                || isMethod(mi, "java.util.TreeMap", "descendingKeySet")
                || isMethod(mi, "java.util.TreeMap", "navigableKeySet")) {
            return resolveDestinationParamTypeCompatibleWithKey(mi);
        } else if (isMethod(mi, "java.util.Map", "values")) {
            return resolveDestinationParamTypeCompatibleWithValue(mi);
        } else if (isMethod(mi, "java.util.TreeMap", "descendingMap")
                || isMethod(mi, "java.util.TreeMap", "firstEntry")
                || isMethod(mi, "java.util.TreeMap", "lastEntry")
                || isMethod(mi, "java.util.TreeMap", "pollFirstEntry")
                || isMethod(mi, "java.util.TreeMap", "pollLastEntry")
                || isMethod(mi, "java.util.Map", "of")) {
            return resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (isMethod(mi, "java.util.TreeMap", "ceilingEntry", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "floorEntry", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "headMap", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "headMap", "java.lang.Object", "boolean")
                || isMethod(mi, "java.util.TreeMap", "higherEntry", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "lowerEntry", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "tailMap", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "tailMap", "java.lang.Object", "boolean")) {
            final ITypeBinding newKeyType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();
            return resolveKeyTypeCompatible(newKeyType)
                    && resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (isMethod(mi, "java.util.Map", "entry", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "subMap", "java.lang.Object", "java.lang.Object")) {
            final ITypeBinding newKeyType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();
            final ITypeBinding newValueType = ((Expression) mi.arguments().get(1)).resolveTypeBinding();
            return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType)
                    && resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (isMethod(mi, "java.util.Map", "entrySet")) {
            if (isExprReceived(mi)) {
                final ITypeBinding newTargetType = getDestinationType(mi);

                if (newTargetType != null && newTargetType.isParameterizedType()
                        && newTargetType.getTypeArguments().length == 1) {
                    final ITypeBinding newElementType = newTargetType.getTypeArguments()[0];

                    if (newElementType != null && newElementType.isParameterizedType()
                            && newElementType.getTypeArguments().length == 2) {
                        return resolveKeyTypeCompatible(newElementType.getTypeArguments()[0])
                                && resolveValueTypeCompatible(newElementType.getTypeArguments()[1]);
                    }
                }
            } else {
                return true;
            }
        } else if (isMethod(mi, "java.util.TreeMap", "ceilingKey", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "floorKey", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "higherKey", "java.lang.Object")
                || isMethod(mi, "java.util.TreeMap", "lowerKey", "java.lang.Object")) {
            return resolveKeyTypeCompatible(((Expression) mi.arguments().get(0)).resolveTypeBinding())
                    && resolveDestinationTypeCompatibleWithKey(mi);
        } else if (isMethod(mi, "java.util.Map", "getOrDefault", "java.lang.Object", "java.lang.Object")) {
            return resolveValueTypeCompatible(((Expression) mi.arguments().get(1)).resolveTypeBinding())
                    && resolveDestinationTypeCompatibleWithValue(mi);
        } else if (isMethod(mi, "java.util.Map", "put", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "putIfAbsent", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "replace", "java.lang.Object", "java.lang.Object")) {
            return resolveKeyTypeCompatible(((Expression) mi.arguments().get(0)).resolveTypeBinding())
                    && resolveValueTypeCompatible(((Expression) mi.arguments().get(1)).resolveTypeBinding())
                    && resolveDestinationTypeCompatibleWithValue(mi);
        } else if (isMethod(mi, "java.util.Map", "replace", "java.lang.Object", "java.lang.Object",
                "java.lang.Object")) {
            return resolveKeyTypeCompatible(((Expression) mi.arguments().get(0)).resolveTypeBinding())
                    && resolveValueTypeCompatible(((Expression) mi.arguments().get(1)).resolveTypeBinding())
                    && resolveValueTypeCompatible(((Expression) mi.arguments().get(2)).resolveTypeBinding());
        } else if (isMethod(mi, "java.util.TreeMap", "subMap", "java.lang.Object", "boolean", "java.lang.Object",
                "boolean")) {
            final ITypeBinding newKeyType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();
            final ITypeBinding newValueType = ((Expression) mi.arguments().get(2)).resolveTypeBinding();
            return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType)
                    && resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object")
                || isMethod(mi, "java.util.Map", "of", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object", "java.lang.Object", "java.lang.Object", "java.lang.Object",
                        "java.lang.Object")) {
            final Iterator<?> argumentIterator = mi.arguments().iterator();

            final List<ITypeBinding> keyTypes = new ArrayList<ITypeBinding>();
            final List<ITypeBinding> valueTypes = new ArrayList<ITypeBinding>();

            while (argumentIterator.hasNext()) {
                keyTypes.add(((Expression) argumentIterator.next()).resolveTypeBinding());
                valueTypes.add(((Expression) argumentIterator.next()).resolveTypeBinding());
            }

            for (final ITypeBinding keyType : keyTypes) {
                if (!resolveKeyTypeCompatible(keyType)) {
                    return false;
                }
            }

            for (final ITypeBinding valueType : valueTypes) {
                if (!resolveValueTypeCompatible(valueType)) {
                    return false;
                }
            }

            return resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (isMethod(mi, "java.util.Map", "compute", "java.lang.Object", "java.util.function.BiFunction")
                || isMethod(mi, "java.util.Map", "computeIfPresent", "java.lang.Object",
                        "java.util.function.BiFunction")) {
            final ITypeBinding paramType = ((Expression) mi.arguments().get(1)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 3) {
                final ITypeBinding newValueType = paramType.getTypeArguments()[2];

                return resolveKeyTypeCompatible(((Expression) mi.arguments().get(0)).resolveTypeBinding())
                        && resolveValueTypeCompatible(newValueType)
                        && resolveDestinationTypeCompatibleWithValue(mi);
            }
        } else if (isMethod(mi, "java.util.Map", "computeIfAbsent", "java.lang.Object",
                "java.util.function.Function")) {
            final ITypeBinding paramType = ((Expression) mi.arguments().get(1)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 2) {
                final ITypeBinding newValueType = paramType.getTypeArguments()[1];

                return resolveKeyTypeCompatible(((Expression) mi.arguments().get(0)).resolveTypeBinding())
                        && resolveValueTypeCompatible(newValueType)
                        && resolveDestinationTypeCompatibleWithValue(mi);
            }
        } else if (isMethod(mi, "java.util.Map", "merge", "java.lang.Object", "java.lang.Object",
                "java.util.function.BiFunction")) {
            final ITypeBinding paramType = ((Expression) mi.arguments().get(2)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 3) {
                final ITypeBinding newValueType = paramType.getTypeArguments()[2];

                return resolveKeyTypeCompatible(((Expression) mi.arguments().get(0)).resolveTypeBinding())
                        && resolveValueTypeCompatible(((Expression) mi.arguments().get(1)).resolveTypeBinding())
                        && resolveValueTypeCompatible(newValueType)
                        && resolveDestinationTypeCompatibleWithValue(mi);
            }
        } else if (isMethod(mi, "java.util.Map", "replaceAll", "java.util.function.BiFunction")) {
            final ITypeBinding paramType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 3) {
                final ITypeBinding newValueType = paramType.getTypeArguments()[2];

                return resolveValueTypeCompatible(newValueType);
            }
        }

        return false;
    }

    private boolean isExprReceived(final ASTNode node) {
        final ASTNode parent = node.getParent();
        if (parent instanceof ParenthesizedExpression) {
            return isExprReceived(parent);
        } else {
            return !(parent instanceof ExpressionStatement);
        }
    }

    private boolean resolveDestinationTypeCompatibleWithKey(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            return resolveKeyTypeCompatible(getDestinationType(mi));
        } else {
            return true;
        }
    }

    private boolean resolveDestinationTypeCompatibleWithValue(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            return resolveValueTypeCompatible(getDestinationType(mi));
        } else {
            return true;
        }
    }

    private boolean resolveDestinationParamTypeCompatibleWithKey(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            final ITypeBinding newElementType = getDestinationType(mi);

            if (newElementType != null && newElementType.isParameterizedType()
                    && newElementType.getTypeArguments().length == 1) {
                return resolveKeyTypeCompatible(newElementType.getTypeArguments()[0]);
            }
            return false;
        } else {
            return true;
        }
    }

    private boolean resolveDestinationParamTypeCompatibleWithValue(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            final ITypeBinding newElementType = getDestinationType(mi);

            if (newElementType != null && newElementType.isParameterizedType()
                    && newElementType.getTypeArguments().length == 1) {
                return resolveValueTypeCompatible(newElementType.getTypeArguments()[0]);
            }
            return false;
        } else {
            return true;
        }
    }

    private boolean resolveDestinationParamTypeCompatibleWithKeyValue(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            final ITypeBinding newElementType = getDestinationType(mi);

            if (newElementType != null && newElementType.isParameterizedType()
                    && newElementType.getTypeArguments().length == 2) {
                return resolveKeyTypeCompatible(newElementType.getTypeArguments()[0])
                        && resolveValueTypeCompatible(newElementType.getTypeArguments()[1]);
            }
            return false;
        } else {
            return true;
        }
    }

    private boolean resolveKeyTypeCompatible(ITypeBinding newElementType) {
        if (newElementType == null) {
            return false;
        }
        if (newElementType.isPrimitive()) {
            newElementType = getBoxedTypeBinding(newElementType, ctx.getAST());
        }
        if (!hasType(newElementType, "java.lang.Object") && (keyType == null || newElementType.equals(keyType))) {
            keyType = newElementType;
            return true;
        }
        return false;
    }

    private boolean resolveValueTypeCompatible(ITypeBinding newElementType) {
        if (newElementType == null) {
            return false;
        }
        if (newElementType.isPrimitive()) {
            newElementType = getBoxedTypeBinding(newElementType, ctx.getAST());
        }
        if (!hasType(newElementType, "java.lang.Object") && (valueType == null || newElementType.equals(valueType))) {
            valueType = newElementType;
            return true;
        }
        return false;
    }

    @Override
    protected boolean canCodeBeRefactored() {
        return keyType != null && valueType != null;
    }

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType) || hasType(variableType,
                getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}
