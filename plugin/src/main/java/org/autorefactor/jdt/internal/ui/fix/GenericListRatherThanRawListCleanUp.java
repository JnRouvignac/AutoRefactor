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

	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 5;
	}

	@Override
	public boolean visit(final Block node) {
		elementType= null;
		return super.visit(node);
	}

	@Override
	protected String[] getExistingClassCanonicalName() {
		return new String[] { LinkedList.class.getCanonicalName(), ArrayList.class.getCanonicalName(), Vector.class.getCanonicalName(), Stack.class.getCanonicalName() };
	}

	@Override
	protected String getSubstitutingClassName(final String origRawType) {
		return origRawType;
	}

	/**
	 * Returns the substitute type.
	 *
	 * @param ast                      The builder.
	 * @param origType               The original type
	 * @param originalExpression     The original expression
	 * @param classesToUseWithImport The classes that should be used with simple
	 *                               name.
	 * @param importsToAdd           The imports that need to be added during this
	 *                               cleanup.
	 * @return the substitute type.
	 */
	@Override
	protected Type substituteType(final ASTNodeFactory ast, final Type origType, final ASTNode originalExpression,
			final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
		if (origType.isParameterizedType()) {
			return null;
		}

		TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpression);

		ParameterizedType parameterizedType= ast.getAST().newParameterizedType(ast.createCopyTarget(origType));
		ASTNodes.typeArguments(parameterizedType).clear();
		ASTNodes.typeArguments(parameterizedType).add(ast.toType(elementType, typeNameDecider));
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
			ITypeBinding actualParameter= ASTNodes.arguments(instanceCreation).get(0).resolveTypeBinding();

			if (isParameterizedTypeWithOneArgument(actualParameter)) {
				return resolveTypeCompatible(actualParameter.getTypeArguments()[0]);
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

		if (ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "clear") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "hashCode") || ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "size") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "isEmpty") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "toString") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "finalize") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "notify") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "notifyAll") || ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, ArrayList.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, ArrayList.class.getCanonicalName(), "removeRange", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, ArrayList.class.getCanonicalName(), "forEach", Consumer.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, ArrayList.class.getCanonicalName(), "removeIf", Predicate.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, ArrayList.class.getCanonicalName(), "sort", Comparator.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "trimToSize") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "setSize", int.class.getSimpleName()) || ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "capacity") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "removeElementAt", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "removeAllElements") || ASTNodes.usesGivenSignature(mi, Stack.class.getCanonicalName(), "empty")) { //$NON-NLS-1$ //$NON-NLS-2$
			return true;
		}
		if (ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "addFirst", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "addLast", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "offer", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "offerFirst", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "offerLast", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "push", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "removeFirstOccurrence", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "removeLastOccurrence", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "setElementAt", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "insertElementAt", Object.class.getCanonicalName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "addElement", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "removeElement", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Stack.class.getCanonicalName(), "push", Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Stack.class.getCanonicalName(), "search", Object.class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding newElementType= ASTNodes.arguments(mi).get(0).resolveTypeBinding();
			return resolveTypeCompatible(newElementType);
		}
		if (ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "add", int.class.getSimpleName(), Object.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "set", int.class.getSimpleName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveTypeCompatible(ASTNodes.arguments(mi).get(1).resolveTypeBinding());
		}
		if (ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "toArray", Object[].class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "copyInto", Object[].class.getCanonicalName())) { //$NON-NLS-1$
			ITypeBinding newElementType= ASTNodes.arguments(mi).get(0).resolveTypeBinding().getElementType();
			return resolveTypeCompatible(newElementType);
		}
		if (ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "containsAll", Collection.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveTypeCompatibleIfPossible(ASTNodes.arguments(mi).get(0).resolveTypeBinding());
		}
		if (ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "addAll", int.class.getSimpleName(), Collection.class.getCanonicalName())) { //$NON-NLS-1$
			return resolveTypeCompatibleIfPossible(ASTNodes.arguments(mi).get(1).resolveTypeBinding());
		}
		if (ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "get", int.class.getSimpleName()) || ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "remove") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "remove", int.class.getSimpleName()) || ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "element") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "getFirst") || ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "getLast") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "peek") || ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "peekFirst") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "peekLast") || ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "poll") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "pollFirst") || ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "pollLast") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "pop") || ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "removeFirst") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "removeLast") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "elementAt", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "firstElement") || ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "lastElement") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, Stack.class.getCanonicalName(), "pop") || ASTNodes.usesGivenSignature(mi, Stack.class.getCanonicalName(), "peek")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (isExprReceived(mi)) {
				ITypeBinding newElementType= ASTNodes.getTargetType(mi);
				return resolveTypeCompatible(newElementType);
			}

			return true;
		}
		if (ASTNodes.usesGivenSignature(mi, LinkedList.class.getCanonicalName(), "descendingIterator") //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "iterator") || ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "listIterator") //$NON-NLS-1$ //$NON-NLS-2$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "listIterator", int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "spliterator") || ASTNodes.usesGivenSignature(mi, Vector.class.getCanonicalName(), "elements")) { //$NON-NLS-1$ //$NON-NLS-2$
			if (isExprReceived(mi)) {
				ITypeBinding newElementType= ASTNodes.getTargetType(mi);
				return resolveTypeCompatibleIfPossible(newElementType);
			}

			return true;
		}
		if (ASTNodes.usesGivenSignature(mi, List.class.getCanonicalName(), "subList", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
				|| ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "toArray")) { //$NON-NLS-1$
			if (!isExprReceived(mi)) {
				return true;
			}
			ITypeBinding newCollectionType= ASTNodes.getTargetType(mi);
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
