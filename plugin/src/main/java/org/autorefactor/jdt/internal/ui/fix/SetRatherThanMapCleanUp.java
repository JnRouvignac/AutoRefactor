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
package org.autorefactor.jdt.internal.ui.fix;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public class SetRatherThanMapCleanUp extends AbstractClassSubstituteCleanUp {
	private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

	static {
		CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Map.class.getCanonicalName(), new String[] { Map.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(AbstractMap.class.getCanonicalName(), new String[] { AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(TreeMap.class.getCanonicalName(), new String[] { TreeMap.class.getCanonicalName(), Serializable.class.getCanonicalName(), Map.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(HashMap.class.getCanonicalName(), new String[] { HashMap.class.getCanonicalName(), Serializable.class.getCanonicalName(), Map.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
	}

	@Override
	public String getName() {
		return MultiFixMessages.SetRatherThanMapCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.SetRatherThanMapCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.SetRatherThanMapCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 2;
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { HashMap.class.getCanonicalName(), TreeMap.class.getCanonicalName() };
	}

	@Override
	public Set<String> getClassesToImport() {
		return new HashSet<>(
				Arrays.asList(HashSet.class.getCanonicalName(), TreeSet.class.getCanonicalName(), AbstractSet.class.getCanonicalName(), Set.class.getCanonicalName()));
	}

	@Override
	protected String getSubstitutingClassName(final String origRawType) {
		if (HashMap.class.getCanonicalName().equals(origRawType)) {
			return HashSet.class.getCanonicalName();
		}
		if (TreeMap.class.getCanonicalName().equals(origRawType)) {
			return TreeSet.class.getCanonicalName();
		}
		if (AbstractMap.class.getCanonicalName().equals(origRawType)) {
			return AbstractSet.class.getCanonicalName();
		}
		if (Map.class.getCanonicalName().equals(origRawType)) {
			return Set.class.getCanonicalName();
		}

		return null;
	}

	@Override
	protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
		ITypeBinding[] parameterTypes= instanceCreation.resolveConstructorBinding().getParameterTypes();

		return parameterTypes.length == 0 || ASTNodes.hasType(parameterTypes[0], int.class.getSimpleName());
	}

	@Override
	protected Type substituteType(final Type origType, final ASTNode originalExpression, final Set<String> classesToUseWithImport,
			final Set<String> importsToAdd) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		ITypeBinding origTypeBinding= origType.resolveBinding();

		if (origTypeBinding == null) {
			return null;
		}

		String substitutingType= getSubstitutingClassName(origTypeBinding.getErasure().getQualifiedName());

		if (classesToUseWithImport.contains(substitutingType)) {
			importsToAdd.add(substitutingType);
			substitutingType= getSimpleName(substitutingType);
		}

		TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpression);

		if (origTypeBinding.isParameterizedType()) {
			ITypeBinding[] origTypeArgs= origTypeBinding.getTypeArguments();
			Type[] newTypes;
			if (origTypeArgs.length > 0 && !((ParameterizedType) origType).typeArguments().isEmpty()) {
				newTypes= new Type[1];
				newTypes[0]= ast.toType(origTypeArgs[0], typeNameDecider);
			} else {
				newTypes= new Type[0];
			}

			return ast.newParameterizedType(substitutingType, newTypes);
		}

		return ast.type(substitutingType);
	}

	@Override
	protected boolean canMethodBeRefactored(final MethodInvocation methodInvocation,
			final List<MethodInvocation> methodCallsToRefactor) {
		if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "clear") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "size") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "finalize") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notify") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notifyAll") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName())) { //$NON-NLS-1$
			return true;
		}
		if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "containsKey", Object.class.getCanonicalName())) { //$NON-NLS-1$
			methodCallsToRefactor.add(methodInvocation);
			return true;
		}
		if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			if (ASTNodes.isPassiveWithoutFallingThrough((Expression) methodInvocation.arguments().get(1))) {
				methodCallsToRefactor.add(methodInvocation);
				return true;
			}

			return false;
		}

		// Here are the following rejected cases:
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
		return ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) && isReturnValueLost(methodInvocation); //$NON-NLS-1$
	}

	private boolean isReturnValueLost(final ASTNode node) {
		ASTNode parentNode= node.getParent();

		return parentNode instanceof ExpressionStatement
				|| parentNode instanceof ParenthesizedExpression && isReturnValueLost(parentNode);
	}

	@Override
	protected void refactorMethod(final MethodInvocation originalMi, final MethodInvocation refactoredMi) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (ASTNodes.usesGivenSignature(originalMi, Map.class.getCanonicalName(), "containsKey", Object.class.getCanonicalName())) { //$NON-NLS-1$
			refactoredMi.setName(ast.newSimpleName("contains")); //$NON-NLS-1$
		} else if (ASTNodes.usesGivenSignature(originalMi, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			refactoredMi.setName(ast.newSimpleName("add")); //$NON-NLS-1$
			refactoredMi.arguments().remove(1);
		}
	}

	@Override
	protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
		return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
				CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
	}
}
