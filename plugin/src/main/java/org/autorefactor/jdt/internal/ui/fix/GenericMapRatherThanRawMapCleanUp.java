/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix;

import java.awt.RenderingHints;
import java.io.Serializable;
import java.security.AuthProvider;
import java.security.Provider;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.jar.Attributes;

import javax.management.openmbean.TabularDataSupport;
import javax.print.attribute.standard.PrinterStateReasons;
import javax.script.SimpleBindings;
import javax.swing.UIDefaults;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.util.Utils;
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
public class GenericMapRatherThanRawMapCleanUp extends AbstractClassSubstituteCleanUp {
    private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

    static {
        CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Map.class.getCanonicalName(), new String[] { Map.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractMap.class.getCanonicalName(), new String[] { AbstractMap.class.getCanonicalName(), Map.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(TreeMap.class.getCanonicalName(), new String[] { TreeMap.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Map.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(HashMap.class.getCanonicalName(), new String[] { HashMap.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Map.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(EnumMap.class.getCanonicalName(), new String[] { EnumMap.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Map.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Hashtable.class.getCanonicalName(), new String[] { Hashtable.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Map.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
    }

    private ITypeBinding keyType;

    private ITypeBinding valueType;

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 5;
    }

    @Override
    public boolean visit(Block node) {
        keyType= null;
        valueType= null;
        return super.visit(node);
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] { Attributes.class.getCanonicalName(), AuthProvider.class.getCanonicalName(), ConcurrentHashMap.class.getCanonicalName(), ConcurrentSkipListMap.class.getCanonicalName(), EnumMap.class.getCanonicalName(), HashMap.class.getCanonicalName(), Hashtable.class.getCanonicalName(), IdentityHashMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(), PrinterStateReasons.class.getCanonicalName(), Properties.class.getCanonicalName(), Provider.class.getCanonicalName(), RenderingHints.class.getCanonicalName(), SimpleBindings.class.getCanonicalName(), TabularDataSupport.class.getCanonicalName(), TreeMap.class.getCanonicalName(), UIDefaults.class.getCanonicalName(), WeakHashMap.class.getCanonicalName() };
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        return origRawType;
    }

    /**
     * Returns the substitute type.
     *
     * @param b                      The builder.
     * @param origType               The original type
     * @param originalExpression     The original expression
     * @param classesToUseWithImport The classes that should be used with simple
     *                               name.
     * @param importsToAdd           The imports that need to be added during this
     *                               cleanup.
     * @return the substitute type.
     */
    @Override
    protected Type substituteType(final ASTNodeFactory b, final Type origType, final ASTNode originalExpression,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        if (origType.isParameterizedType()) {
            return null;
        }

        final TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpression);

        final ParameterizedType parameterizedType= b.getAST().newParameterizedType(b.copy(origType));
        final List<Type> typeArgs= ASTNodes.typeArguments(parameterizedType);
        typeArgs.clear();
        typeArgs.add(b.toType(keyType, typeNameDecider));
        typeArgs.add(b.toType(valueType, typeNameDecider));

        return parameterizedType;
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        keyType= null;
        valueType= null;

        if (instanceCreation.resolveTypeBinding() == null || instanceCreation.resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        ITypeBinding[] parameterTypes= instanceCreation.resolveConstructorBinding().getParameterTypes();

        if (parameterTypes.length > 0 && ASTNodes.hasType(parameterTypes[0], Map.class.getCanonicalName())) {
            ITypeBinding actualParameter= ASTNodes.arguments(instanceCreation).get(0).resolveTypeBinding();

            if (isParameterizedTypeWithNbArguments(actualParameter, 2)) {
                ITypeBinding newKeyType= actualParameter.getTypeArguments()[0];
                ITypeBinding newValueType= actualParameter.getTypeArguments()[1];
                return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
            }
        }

        return true;
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (mi.getExpression() == null || mi.getExpression().resolveTypeBinding() == null || mi.getExpression().resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        final List<Expression> arguments= ASTNodes.arguments(mi);
        if (ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "toString") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "finalize") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "notify") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "notifyAll") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "size") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName()) || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "clear") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "containsKey", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "containsValue", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "forEach", BiConsumer.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "hashCode") || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "size") //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
            return true;
        }
        if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "ofEntries", Entry[].class.getCanonicalName())) { //$NON-NLS-1$
            final ITypeBinding paramType= arguments.get(0).resolveTypeBinding().getElementType();

            if (isParameterizedTypeWithNbArguments(paramType, 2)) {
                final ITypeBinding newKeyType= paramType.getTypeArguments()[0];
                final ITypeBinding newValueType= paramType.getTypeArguments()[1];
                return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
            }
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "putAll", Map.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, LinkedHashMap.class.getCanonicalName(), "removeEldestEntry", Entry.class.getCanonicalName())) { //$NON-NLS-1$
            final ITypeBinding paramType= arguments.get(0).resolveTypeBinding();

            if (isParameterizedTypeWithNbArguments(paramType, 2)) {
                final ITypeBinding newKeyType= paramType.getTypeArguments()[0];
                final ITypeBinding newValueType= paramType.getTypeArguments()[1];
                return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
            }
        } else if (ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "lastKey") || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "firstKey")) { //$NON-NLS-1$ //$NON-NLS-2$
            return resolveDestinationTypeCompatibleWithKey(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "get", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName())) { //$NON-NLS-1$
            return resolveDestinationTypeCompatibleWithValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "keySet") || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "comparator") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "descendingKeySet") //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "navigableKeySet")) { //$NON-NLS-1$
            return resolveDestinationParamTypeCompatibleWithKey(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "values")) { //$NON-NLS-1$
            return resolveDestinationParamTypeCompatibleWithValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "descendingMap") || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "firstEntry") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "lastEntry") || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "pollFirstEntry") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "pollLastEntry") || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of")) { //$NON-NLS-1$ //$NON-NLS-2$
            return resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "ceilingEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "floorEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "headMap", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "headMap", Object.class.getCanonicalName(), boolean.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "higherEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "lowerEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "tailMap", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "tailMap", Object.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
            final ITypeBinding newKeyType= arguments.get(0).resolveTypeBinding();
            return resolveKeyTypeCompatible(newKeyType) && resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "entry", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "subMap", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
            final ITypeBinding newKeyType= arguments.get(0).resolveTypeBinding();
            final ITypeBinding newValueType= arguments.get(1).resolveTypeBinding();
            return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType)
                    && resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "entrySet")) { //$NON-NLS-1$
            if (!isExprReceived(mi)) {
                return true;
            }
            final ITypeBinding newTargetType= ASTNodes.getTargetType(mi);
            if (isParameterizedTypeWithNbArguments(newTargetType, 1)) {
                final ITypeBinding newElementType= newTargetType.getTypeArguments()[0];

                if (isParameterizedTypeWithNbArguments(newElementType, 2)) {
                    return resolveKeyTypeCompatible(newElementType.getTypeArguments()[0])
                            && resolveValueTypeCompatible(newElementType.getTypeArguments()[1]);
                }
            }
        } else if (ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "ceilingKey", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "floorKey", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "higherKey", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "lowerKey", Object.class.getCanonicalName())) { //$NON-NLS-1$
            return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
                    && resolveDestinationTypeCompatibleWithKey(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "getOrDefault", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
            return resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
                    && resolveDestinationTypeCompatibleWithValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "putIfAbsent", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "replace", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
            return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
                    && resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
                    && resolveDestinationTypeCompatibleWithValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "replace", Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                Object.class.getCanonicalName())) {
            return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
                    && resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
                    && resolveValueTypeCompatible(arguments.get(2).resolveTypeBinding());
        } else if (ASTNodes.usesGivenSignature(mi, TreeMap.class.getCanonicalName(), "subMap", Object.class.getCanonicalName(), boolean.class.getSimpleName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                boolean.class.getSimpleName())) {
            final ITypeBinding newKeyType= arguments.get(0).resolveTypeBinding();
            final ITypeBinding newValueType= arguments.get(2).resolveTypeBinding();
            return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType)
                    && resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
                        Object.class.getCanonicalName())) {
            final Iterator<Expression> argumentIterator= arguments.iterator();

            final List<ITypeBinding> keyTypes= new ArrayList<>();
            final List<ITypeBinding> valueTypes= new ArrayList<>();

            while (argumentIterator.hasNext()) {
                keyTypes.add(argumentIterator.next().resolveTypeBinding());
                valueTypes.add(argumentIterator.next().resolveTypeBinding());
            }

            for (ITypeBinding keyType : keyTypes) {
                if (!resolveKeyTypeCompatible(keyType)) {
                    return false;
                }
            }

            for (ITypeBinding valueType : valueTypes) {
                if (!resolveValueTypeCompatible(valueType)) {
                    return false;
                }
            }

            return resolveDestinationParamTypeCompatibleWithKeyValue(mi);
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "compute", Object.class.getCanonicalName(), BiFunction.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "computeIfPresent", Object.class.getCanonicalName(), //$NON-NLS-1$
                        BiFunction.class.getCanonicalName())) {
            final ITypeBinding paramType= arguments.get(1).resolveTypeBinding();

            if (isParameterizedTypeWithNbArguments(paramType, 3)) {
                final ITypeBinding newValueType= paramType.getTypeArguments()[2];

                return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
                        && resolveValueTypeCompatible(newValueType) && resolveDestinationTypeCompatibleWithValue(mi);
            }
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "computeIfAbsent", Object.class.getCanonicalName(), //$NON-NLS-1$
                Function.class.getCanonicalName())) {
            final ITypeBinding paramType= arguments.get(1).resolveTypeBinding();

            if (isParameterizedTypeWithNbArguments(paramType, 2)) {
                final ITypeBinding newValueType= paramType.getTypeArguments()[1];

                return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
                        && resolveValueTypeCompatible(newValueType) && resolveDestinationTypeCompatibleWithValue(mi);
            }
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "merge", Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
                BiFunction.class.getCanonicalName())) {
            final ITypeBinding paramType= arguments.get(2).resolveTypeBinding();

            if (isParameterizedTypeWithNbArguments(paramType, 3)) {
                final ITypeBinding newValueType= paramType.getTypeArguments()[2];

                return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
                        && resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
                        && resolveValueTypeCompatible(newValueType) && resolveDestinationTypeCompatibleWithValue(mi);
            }
        } else if (ASTNodes.usesGivenSignature(mi, Map.class.getCanonicalName(), "replaceAll", BiFunction.class.getCanonicalName())) { //$NON-NLS-1$
            final ITypeBinding paramType= arguments.get(0).resolveTypeBinding();

            if (isParameterizedTypeWithNbArguments(paramType, 3)) {
                final ITypeBinding newValueType= paramType.getTypeArguments()[2];

                return resolveValueTypeCompatible(newValueType);
            }
        }

        return false;
    }

    private boolean isExprReceived(final ASTNode node) {
        final ASTNode parent= node.getParent();
        if (parent instanceof ParenthesizedExpression) {
            return isExprReceived(parent);
        }

        return !(parent instanceof ExpressionStatement);
    }

    private boolean resolveDestinationTypeCompatibleWithKey(final MethodInvocation mi) {
        return !isExprReceived(mi) || resolveKeyTypeCompatible(ASTNodes.getTargetType(mi));
    }

    private boolean resolveDestinationTypeCompatibleWithValue(final MethodInvocation mi) {
        return !isExprReceived(mi) || resolveValueTypeCompatible(ASTNodes.getTargetType(mi));
    }

    private boolean resolveDestinationParamTypeCompatibleWithKey(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            final ITypeBinding newElementType= ASTNodes.getTargetType(mi);
            return isParameterizedTypeWithNbArguments(newElementType, 1)
                    && resolveKeyTypeCompatible(newElementType.getTypeArguments()[0]);
        }

        return true;
    }

    private boolean resolveDestinationParamTypeCompatibleWithValue(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            final ITypeBinding newElementType= ASTNodes.getTargetType(mi);
            return isParameterizedTypeWithNbArguments(newElementType, 1)
                    && resolveValueTypeCompatible(newElementType.getTypeArguments()[0]);
        }

        return true;
    }

    private boolean resolveDestinationParamTypeCompatibleWithKeyValue(final MethodInvocation mi) {
        if (isExprReceived(mi)) {
            final ITypeBinding newElementType= ASTNodes.getTargetType(mi);
            return isParameterizedTypeWithNbArguments(newElementType, 2)
                    && resolveKeyTypeCompatible(newElementType.getTypeArguments()[0])
                    && resolveValueTypeCompatible(newElementType.getTypeArguments()[1]);
        }

        return true;
    }

    private boolean isParameterizedTypeWithNbArguments(final ITypeBinding typeBinding, int nbArgs) {
        return typeBinding != null && typeBinding.isParameterizedType()
                && typeBinding.getTypeArguments().length == nbArgs;
    }

    private boolean resolveKeyTypeCompatible(ITypeBinding newElementType) {
        if (newElementType == null) {
            return false;
        }
        if (newElementType.isPrimitive()) {
            newElementType= Bindings.getBoxedTypeBinding(newElementType, ctx.getAST());
        }
        if (!ASTNodes.hasType(newElementType, Object.class.getCanonicalName()) && (keyType == null || newElementType.equals(keyType))) {
            keyType= newElementType;
            return true;
        }

        return false;
    }

    private boolean resolveValueTypeCompatible(ITypeBinding newElementType) {
        if (newElementType == null) {
            return false;
        }
        if (newElementType.isPrimitive()) {
            newElementType= Bindings.getBoxedTypeBinding(newElementType, ctx.getAST());
        }
        if (!ASTNodes.hasType(newElementType, Object.class.getCanonicalName()) && (valueType == null || newElementType.equals(valueType))) {
            valueType= newElementType;
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
        return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
                Utils.getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}
