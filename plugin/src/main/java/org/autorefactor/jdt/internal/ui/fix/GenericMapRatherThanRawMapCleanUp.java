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

	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_GenericMapRatherThanRawMapCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public boolean visit(final Block node) {
		keyType= null;
		valueType= null;
		return super.visit(node);
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { Attributes.class.getCanonicalName(), AuthProvider.class.getCanonicalName(), ConcurrentHashMap.class.getCanonicalName(), ConcurrentSkipListMap.class.getCanonicalName(), EnumMap.class.getCanonicalName(), HashMap.class.getCanonicalName(), Hashtable.class.getCanonicalName(), IdentityHashMap.class.getCanonicalName(), LinkedHashMap.class.getCanonicalName(), PrinterStateReasons.class.getCanonicalName(), Properties.class.getCanonicalName(), Provider.class.getCanonicalName(), RenderingHints.class.getCanonicalName(), SimpleBindings.class.getCanonicalName(), TabularDataSupport.class.getCanonicalName(), TreeMap.class.getCanonicalName(), UIDefaults.class.getCanonicalName(), WeakHashMap.class.getCanonicalName() };
	}

	@Override
	protected String getSubstitutingClassName(final String origRawType) {
		return origRawType;
	}

	@Override
	protected Type substituteType(final Type origType, final ASTNode originalExpression, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (origType.isParameterizedType()) {
			return null;
		}

		TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpression);

		ParameterizedType parameterizedType= ast.getAST().newParameterizedType(ast.createCopyTarget(origType));
		@SuppressWarnings("unchecked")
		List<Type> typeArgs= parameterizedType.typeArguments();
		typeArgs.clear();
		typeArgs.add(ast.toType(keyType, typeNameDecider));
		typeArgs.add(ast.toType(valueType, typeNameDecider));

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
			@SuppressWarnings("unchecked")
			ITypeBinding actualParameter= ((List<Expression>) instanceCreation.arguments()).get(0).resolveTypeBinding();

			if (isParameterizedTypeWithNbArguments(actualParameter, 2)) {
				ITypeBinding newKeyType= actualParameter.getTypeArguments()[0];
				ITypeBinding newValueType= actualParameter.getTypeArguments()[1];
				return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
			}
		}

		return true;
	}

	@Override
	protected boolean canMethodBeRefactored(final MethodInvocation methodInvocation,
			final List<MethodInvocation> methodCallsToRefactor) {
		if (methodInvocation.getExpression() == null || methodInvocation.getExpression().resolveTypeBinding() == null || methodInvocation.getExpression().resolveTypeBinding().isParameterizedType()) {
			return false;
		}

		@SuppressWarnings("unchecked")
		List<Expression> arguments= methodInvocation.arguments();
		if (ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "toString") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "finalize") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notify") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notifyAll") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "size") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName()) || ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "clear") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "containsKey", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "containsValue", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "forEach", BiConsumer.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "hashCode") || ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "size") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			return true;
		}
		if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "ofEntries", Entry[].class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding paramType= arguments.get(0).resolveTypeBinding().getElementType();

			if (isParameterizedTypeWithNbArguments(paramType, 2)) {
				ITypeBinding newKeyType= paramType.getTypeArguments()[0];
				ITypeBinding newValueType= paramType.getTypeArguments()[1];
				return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
			}
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "putAll", Map.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedHashMap.class.getCanonicalName(), "removeEldestEntry", Entry.class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding paramType= arguments.get(0).resolveTypeBinding();

			if (isParameterizedTypeWithNbArguments(paramType, 2)) {
				ITypeBinding newKeyType= paramType.getTypeArguments()[0];
				ITypeBinding newValueType= paramType.getTypeArguments()[1];
				return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType);
			}
		} else if (ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "lastKey") || ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "firstKey")) { //$NON-NLS-1$ //$NON-NLS-2$
			return resolveDestinationTypeCompatibleWithKey(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "get", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveDestinationTypeCompatibleWithValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "keySet") || ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "comparator") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "descendingKeySet") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "navigableKeySet")) { //$NON-NLS-1$
			return resolveDestinationParamTypeCompatibleWithKey(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "values")) { //$NON-NLS-1$
			return resolveDestinationParamTypeCompatibleWithValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "descendingMap") || ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "firstEntry") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "lastEntry") || ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "pollFirstEntry") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "pollLastEntry") || ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of")) { //$NON-NLS-1$ //$NON-NLS-2$
			return resolveDestinationParamTypeCompatibleWithKeyValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "ceilingEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "floorEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "headMap", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "headMap", Object.class.getCanonicalName(), boolean.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "higherEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "lowerEntry", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "tailMap", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "tailMap", Object.class.getCanonicalName(), boolean.class.getSimpleName())) { //$NON-NLS-1$
			ITypeBinding newKeyType= arguments.get(0).resolveTypeBinding();
			return resolveKeyTypeCompatible(newKeyType) && resolveDestinationParamTypeCompatibleWithKeyValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "entry", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "subMap", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding newKeyType= arguments.get(0).resolveTypeBinding();
			ITypeBinding newValueType= arguments.get(1).resolveTypeBinding();
			return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType)
					&& resolveDestinationParamTypeCompatibleWithKeyValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "entrySet")) { //$NON-NLS-1$
			if (!isExprReceived(methodInvocation)) {
				return true;
			}
			ITypeBinding newTargetType= ASTNodes.getTargetType(methodInvocation);
			if (isParameterizedTypeWithNbArguments(newTargetType, 1)) {
				ITypeBinding newElementType= newTargetType.getTypeArguments()[0];

				if (isParameterizedTypeWithNbArguments(newElementType, 2)) {
					return resolveKeyTypeCompatible(newElementType.getTypeArguments()[0])
							&& resolveValueTypeCompatible(newElementType.getTypeArguments()[1]);
				}
			}
		} else if (ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "ceilingKey", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "floorKey", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "higherKey", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "lowerKey", Object.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
					&& resolveDestinationTypeCompatibleWithKey(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "getOrDefault", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
					&& resolveDestinationTypeCompatibleWithValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "putIfAbsent", Object.class.getCanonicalName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "replace", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
					&& resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
					&& resolveDestinationTypeCompatibleWithValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "replace", Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
				Object.class.getCanonicalName())) {
			return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
					&& resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
					&& resolveValueTypeCompatible(arguments.get(2).resolveTypeBinding());
		} else if (ASTNodes.usesGivenSignature(methodInvocation, TreeMap.class.getCanonicalName(), "subMap", Object.class.getCanonicalName(), boolean.class.getSimpleName(), Object.class.getCanonicalName(), //$NON-NLS-1$
				boolean.class.getSimpleName())) {
			ITypeBinding newKeyType= arguments.get(0).resolveTypeBinding();
			ITypeBinding newValueType= arguments.get(2).resolveTypeBinding();
			return resolveKeyTypeCompatible(newKeyType) && resolveValueTypeCompatible(newValueType)
					&& resolveDestinationParamTypeCompatibleWithKeyValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
				Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName())
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "of", Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(), Object.class.getCanonicalName(),
						Object.class.getCanonicalName())) {
			Iterator<Expression> argumentIterator= arguments.iterator();

			List<ITypeBinding> keyTypes= new ArrayList<>();
			List<ITypeBinding> valueTypes= new ArrayList<>();

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

			return resolveDestinationParamTypeCompatibleWithKeyValue(methodInvocation);
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "compute", Object.class.getCanonicalName(), BiFunction.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "computeIfPresent", Object.class.getCanonicalName(), //$NON-NLS-1$
						BiFunction.class.getCanonicalName())) {
			ITypeBinding paramType= arguments.get(1).resolveTypeBinding();

			if (isParameterizedTypeWithNbArguments(paramType, 3)) {
				ITypeBinding newValueType= paramType.getTypeArguments()[2];

				return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
						&& resolveValueTypeCompatible(newValueType) && resolveDestinationTypeCompatibleWithValue(methodInvocation);
			}
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "computeIfAbsent", Object.class.getCanonicalName(), //$NON-NLS-1$
				Function.class.getCanonicalName())) {
			ITypeBinding paramType= arguments.get(1).resolveTypeBinding();

			if (isParameterizedTypeWithNbArguments(paramType, 2)) {
				ITypeBinding newValueType= paramType.getTypeArguments()[1];

				return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
						&& resolveValueTypeCompatible(newValueType) && resolveDestinationTypeCompatibleWithValue(methodInvocation);
			}
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "merge", Object.class.getCanonicalName(), Object.class.getCanonicalName(), //$NON-NLS-1$
				BiFunction.class.getCanonicalName())) {
			ITypeBinding paramType= arguments.get(2).resolveTypeBinding();

			if (isParameterizedTypeWithNbArguments(paramType, 3)) {
				ITypeBinding newValueType= paramType.getTypeArguments()[2];

				return resolveKeyTypeCompatible(arguments.get(0).resolveTypeBinding())
						&& resolveValueTypeCompatible(arguments.get(1).resolveTypeBinding())
						&& resolveValueTypeCompatible(newValueType) && resolveDestinationTypeCompatibleWithValue(methodInvocation);
			}
		} else if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "replaceAll", BiFunction.class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding paramType= arguments.get(0).resolveTypeBinding();

			if (isParameterizedTypeWithNbArguments(paramType, 3)) {
				ITypeBinding newValueType= paramType.getTypeArguments()[2];

				return resolveValueTypeCompatible(newValueType);
			}
		}

		return false;
	}

	private boolean isExprReceived(final ASTNode node) {
		ASTNode parent= node.getParent();
		if (parent instanceof ParenthesizedExpression) {
			return isExprReceived(parent);
		}

		return !(parent instanceof ExpressionStatement);
	}

	private boolean resolveDestinationTypeCompatibleWithKey(final MethodInvocation methodInvocation) {
		return !isExprReceived(methodInvocation) || resolveKeyTypeCompatible(ASTNodes.getTargetType(methodInvocation));
	}

	private boolean resolveDestinationTypeCompatibleWithValue(final MethodInvocation methodInvocation) {
		return !isExprReceived(methodInvocation) || resolveValueTypeCompatible(ASTNodes.getTargetType(methodInvocation));
	}

	private boolean resolveDestinationParamTypeCompatibleWithKey(final MethodInvocation methodInvocation) {
		if (isExprReceived(methodInvocation)) {
			ITypeBinding newElementType= ASTNodes.getTargetType(methodInvocation);
			return isParameterizedTypeWithNbArguments(newElementType, 1)
					&& resolveKeyTypeCompatible(newElementType.getTypeArguments()[0]);
		}

		return true;
	}

	private boolean resolveDestinationParamTypeCompatibleWithValue(final MethodInvocation methodInvocation) {
		if (isExprReceived(methodInvocation)) {
			ITypeBinding newElementType= ASTNodes.getTargetType(methodInvocation);
			return isParameterizedTypeWithNbArguments(newElementType, 1)
					&& resolveValueTypeCompatible(newElementType.getTypeArguments()[0]);
		}

		return true;
	}

	private boolean resolveDestinationParamTypeCompatibleWithKeyValue(final MethodInvocation methodInvocation) {
		if (isExprReceived(methodInvocation)) {
			ITypeBinding newElementType= ASTNodes.getTargetType(methodInvocation);
			return isParameterizedTypeWithNbArguments(newElementType, 2)
					&& resolveKeyTypeCompatible(newElementType.getTypeArguments()[0])
					&& resolveValueTypeCompatible(newElementType.getTypeArguments()[1]);
		}

		return true;
	}

	private boolean isParameterizedTypeWithNbArguments(final ITypeBinding typeBinding, final int nbArgs) {
		return typeBinding != null && typeBinding.isParameterizedType()
				&& typeBinding.getTypeArguments().length == nbArgs;
	}

	private boolean resolveKeyTypeCompatible(ITypeBinding newElementType) {
		if (newElementType == null) {
			return false;
		}
		if (newElementType.isPrimitive()) {
			newElementType= Bindings.getBoxedTypeBinding(newElementType, cuRewrite.getAST());
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
			newElementType= Bindings.getBoxedTypeBinding(newElementType, cuRewrite.getAST());
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
				CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
	}
}
