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

import java.io.Serializable;
import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;
import java.util.function.Consumer;
import java.util.function.Predicate;

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
public class GenericListRatherThanRawListCleanUp extends AbstractClassSubstituteCleanUp {
	private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

	static {
		CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(Collection.class.getCanonicalName(), new String[] { Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(List.class.getCanonicalName(), new String[] { List.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(AbstractList.class.getCanonicalName(), new String[] { AbstractList.class.getCanonicalName(), List.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(AbstractCollection.class.getCanonicalName(), new String[] { AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(LinkedList.class.getCanonicalName(), new String[] { LinkedList.class.getCanonicalName(), AbstractList.class.getCanonicalName(), List.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
		CAN_BE_CASTED_TO.put(ArrayList.class.getCanonicalName(), new String[] { ArrayList.class.getCanonicalName(), AbstractList.class.getCanonicalName(), List.class.getCanonicalName(), AbstractCollection.class.getCanonicalName(), Collection.class.getCanonicalName(), Serializable.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
	}

	private ITypeBinding elementType;

	@Override
	public String getName() {
		return MultiFixMessages.GenericListRatherThanRawListCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.GenericListRatherThanRawListCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.GenericListRatherThanRawListCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public boolean visit(final Block visited) {
		elementType= null;
		return super.visit(visited);
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { LinkedList.class.getCanonicalName(), ArrayList.class.getCanonicalName(), Vector.class.getCanonicalName(), Stack.class.getCanonicalName() };
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
		parameterizedType.typeArguments().clear();
		parameterizedType.typeArguments().add(ast.toType(elementType, typeNameDecider));
		return parameterizedType;
	}

	@Override
	protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
		elementType= null;

		if (instanceCreation.resolveTypeBinding() == null || instanceCreation.resolveTypeBinding().isParameterizedType()) {
			return false;
		}

		ITypeBinding[] parameterTypes= instanceCreation.resolveConstructorBinding().getParameterTypes();

		if (parameterTypes.length > 0 && ASTNodes.hasType(parameterTypes[0], Collection.class.getCanonicalName())) {
			ITypeBinding actualParameter= ((Expression) instanceCreation.arguments().get(0)).resolveTypeBinding();

			if (isParameterizedTypeWithOneArgument(actualParameter)) {
				return resolveTypeCompatible(actualParameter.getTypeArguments()[0]);
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

		if (ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "clear") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "hashCode") || ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "size") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "isEmpty") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "toString") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "finalize") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notify") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notifyAll") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, ArrayList.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, ArrayList.class.getCanonicalName(), "removeRange", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, ArrayList.class.getCanonicalName(), "forEach", Consumer.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, ArrayList.class.getCanonicalName(), "removeIf", Predicate.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, ArrayList.class.getCanonicalName(), "sort", Comparator.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "trimToSize") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "setSize", int.class.getSimpleName()) || ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "capacity") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "removeElementAt", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "removeAllElements") || ASTNodes.usesGivenSignature(methodInvocation, Stack.class.getCanonicalName(), "empty")) { //$NON-NLS-1$ //$NON-NLS-2$
			return true;
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "addFirst", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "addLast", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "offer", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "offerFirst", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "offerLast", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "push", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "removeFirstOccurrence", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "removeLastOccurrence", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "setElementAt", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "insertElementAt", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "addElement", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "removeElement", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Stack.class.getCanonicalName(), "push", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Stack.class.getCanonicalName(), "search", Object.class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding newElementType= ((Expression) methodInvocation.arguments().get(0)).resolveTypeBinding();
			return resolveTypeCompatible(newElementType);
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "add", int.class.getSimpleName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "set", int.class.getSimpleName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveTypeCompatible(((Expression) methodInvocation.arguments().get(1)).resolveTypeBinding());
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "toArray", Object[].class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "copyInto", Object[].class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding newElementType= ((Expression) methodInvocation.arguments().get(0)).resolveTypeBinding().getElementType();
			return resolveTypeCompatible(newElementType);
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "containsAll", Collection.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveTypeCompatibleIfPossible(((Expression) methodInvocation.arguments().get(0)).resolveTypeBinding());
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "addAll", int.class.getSimpleName(), Collection.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveTypeCompatibleIfPossible(((Expression) methodInvocation.arguments().get(1)).resolveTypeBinding());
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "get", int.class.getSimpleName()) || ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "remove") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "remove", int.class.getSimpleName()) || ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "element") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "getFirst") || ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "getLast") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "peek") || ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "peekFirst") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "peekLast") || ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "poll") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "pollFirst") || ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "pollLast") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "pop") || ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "removeFirst") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "removeLast") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "elementAt", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "firstElement") || ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "lastElement") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, Stack.class.getCanonicalName(), "pop") || ASTNodes.usesGivenSignature(methodInvocation, Stack.class.getCanonicalName(), "peek")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (isExprReceived(methodInvocation)) {
				ITypeBinding newElementType= ASTNodes.getTargetType(methodInvocation);
				return resolveTypeCompatible(newElementType);
			}

			return true;
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, LinkedList.class.getCanonicalName(), "descendingIterator") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "iterator") || ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "listIterator") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "listIterator", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "spliterator") || ASTNodes.usesGivenSignature(methodInvocation, Vector.class.getCanonicalName(), "elements")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (isExprReceived(methodInvocation)) {
				ITypeBinding newElementType= ASTNodes.getTargetType(methodInvocation);
				return resolveTypeCompatibleIfPossible(newElementType);
			}

			return true;
		}

		if (ASTNodes.usesGivenSignature(methodInvocation, List.class.getCanonicalName(), "subList", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(methodInvocation, Collection.class.getCanonicalName(), "toArray")) { //$NON-NLS-1$
			if (!isExprReceived(methodInvocation)) {
				return true;
			}
			ITypeBinding newCollectionType= ASTNodes.getTargetType(methodInvocation);
			if (newCollectionType != null) {
				ITypeBinding newElementType= newCollectionType.getElementType();
				return resolveTypeCompatible(newElementType);
			}
		}

		return false;
	}

	private boolean resolveTypeCompatibleIfPossible(final ITypeBinding paramType) {
		return isParameterizedTypeWithOneArgument(paramType) && resolveTypeCompatible(paramType.getTypeArguments()[0]);
	}

	private boolean isParameterizedTypeWithOneArgument(final ITypeBinding typeBinding) {
		return typeBinding != null && typeBinding.isParameterizedType() && typeBinding.getTypeArguments().length == 1;
	}

	private boolean isExprReceived(final ASTNode node) {
		ASTNode parent= node.getParent();
		if (parent instanceof ParenthesizedExpression) {
			return isExprReceived(parent);
		}

		return !(parent instanceof ExpressionStatement);
	}

	private boolean resolveTypeCompatible(ITypeBinding newElementType) {
		if (newElementType == null) {
			return false;
		}
		if (newElementType.isPrimitive()) {
			newElementType= Bindings.getBoxedTypeBinding(newElementType, cuRewrite.getAST());
		}
		if (!ASTNodes.hasType(newElementType, Object.class.getCanonicalName())
				&& (elementType == null || newElementType.equals(elementType))) {
			elementType= newElementType;
			return true;
		}

		return false;
	}

	@Override
	protected boolean canCodeBeRefactored() {
		return elementType != null;
	}

	@Override
	protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
		return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
				CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
	}
}
