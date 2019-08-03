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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.*;
import static org.autorefactor.util.Utils.getOrDefault;

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

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
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
    private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<String, String[]>();

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
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_GenericListRatherThanRawListCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 5;
    }

    @Override
    public boolean visit(Block node) {
        elementType= null;
        return super.visit(node);
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] { LinkedList.class.getCanonicalName(), ArrayList.class.getCanonicalName(), Vector.class.getCanonicalName(), Stack.class.getCanonicalName() };
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
     * @param originalExpr           The original expression
     * @param classesToUseWithImport The classes that should be used with simple
     *                               name.
     * @param importsToAdd           The imports that need to be added during this
     *                               refactoring.
     * @return the substitute type.
     */
    @Override
    protected Type substituteType(final ASTBuilder b, final Type origType, final ASTNode originalExpr,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        if (origType.isParameterizedType()) {
            return null;
        }

        final TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpr);

        final ParameterizedType parameterizedType= b.getAST().newParameterizedType(b.copy(origType));
        typeArguments(parameterizedType).clear();
        typeArguments(parameterizedType).add(b.toType(elementType, typeNameDecider));
        return parameterizedType;
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        elementType= null;

        if (instanceCreation.resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        ITypeBinding[] parameterTypes= instanceCreation.resolveConstructorBinding().getParameterTypes();

        if (parameterTypes.length > 0 && hasType(parameterTypes[0], Collection.class.getCanonicalName())) {
            ITypeBinding actualParameter= arguments(instanceCreation).get(0).resolveTypeBinding();

            if (isParameterizedTypeWithOneArgument(actualParameter)) {
                return resolveTypeCompatible(actualParameter.getTypeArguments()[0]);
            }
        }
        return true;
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (mi.getExpression() == null || mi.getExpression().resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        if (isMethod(mi, Collection.class.getCanonicalName(), "clear")
                || isMethod(mi, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName())
                || isMethod(mi, Object.class.getCanonicalName(), "hashCode") || isMethod(mi, Collection.class.getCanonicalName(), "size")
                || isMethod(mi, Collection.class.getCanonicalName(), "isEmpty") || isMethod(mi, Object.class.getCanonicalName(), "toString")
                || isMethod(mi, Object.class.getCanonicalName(), "finalize") || isMethod(mi, Object.class.getCanonicalName(), "notify")
                || isMethod(mi, Object.class.getCanonicalName(), "notifyAll") || isMethod(mi, Object.class.getCanonicalName(), "wait")
                || isMethod(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName())
                || isMethod(mi, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName())
                || isMethod(mi, ArrayList.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName())
                || isMethod(mi, ArrayList.class.getCanonicalName(), "removeRange", int.class.getSimpleName(), int.class.getSimpleName())
                || isMethod(mi, ArrayList.class.getCanonicalName(), "forEach", "java.util.function.Consumer")
                || isMethod(mi, ArrayList.class.getCanonicalName(), "removeIf", "java.util.function.Predicate")
                || isMethod(mi, ArrayList.class.getCanonicalName(), "sort", Comparator.class.getCanonicalName())
                || isMethod(mi, Vector.class.getCanonicalName(), "trimToSize")
                || isMethod(mi, Vector.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "setSize", int.class.getSimpleName()) || isMethod(mi, Vector.class.getCanonicalName(), "capacity")
                || isMethod(mi, Vector.class.getCanonicalName(), "removeElementAt", int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "removeAllElements") || isMethod(mi, Stack.class.getCanonicalName(), "empty")) {
            return true;
        } else if (isMethod(mi, Collection.class.getCanonicalName(), "add", Object.class.getCanonicalName())
                || isMethod(mi, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName())
                || isMethod(mi, List.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName())
                || isMethod(mi, List.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "addFirst", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "addLast", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "offer", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "offerFirst", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "offerLast", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "push", Object.class.getCanonicalName())
                || isMethod(mi, Collection.class.getCanonicalName(), "remove", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "removeFirstOccurrence", Object.class.getCanonicalName())
                || isMethod(mi, LinkedList.class.getCanonicalName(), "removeLastOccurrence", Object.class.getCanonicalName())
                || isMethod(mi, Vector.class.getCanonicalName(), "indexOf", Object.class.getCanonicalName(), int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "lastIndexOf", Object.class.getCanonicalName(), int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "setElementAt", Object.class.getCanonicalName(), int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "insertElementAt", Object.class.getCanonicalName(), int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "addElement", Object.class.getCanonicalName())
                || isMethod(mi, Vector.class.getCanonicalName(), "removeElement", Object.class.getCanonicalName())
                || isMethod(mi, Stack.class.getCanonicalName(), "push", Object.class.getCanonicalName())
                || isMethod(mi, Stack.class.getCanonicalName(), "search", Object.class.getCanonicalName())) {
            ITypeBinding newElementType= arguments(mi).get(0).resolveTypeBinding();
            return resolveTypeCompatible(newElementType);
        } else if (isMethod(mi, List.class.getCanonicalName(), "add", int.class.getSimpleName(), Object.class.getCanonicalName())
                || isMethod(mi, List.class.getCanonicalName(), "set", int.class.getSimpleName(), Object.class.getCanonicalName())) {
            return resolveTypeCompatible(arguments(mi).get(1).resolveTypeBinding());
        } else if (isMethod(mi, Collection.class.getCanonicalName(), "toArray", Object[].class.getCanonicalName())
                || isMethod(mi, Vector.class.getCanonicalName(), "copyInto", Object[].class.getCanonicalName())) {
            ITypeBinding newElementType= arguments(mi).get(0).resolveTypeBinding().getElementType();
            return resolveTypeCompatible(newElementType);
        } else if (isMethod(mi, Collection.class.getCanonicalName(), "addAll", Collection.class.getCanonicalName())
                || isMethod(mi, Collection.class.getCanonicalName(), "containsAll", Collection.class.getCanonicalName())) {
            return resolveTypeCompatibleIfPossible(arguments(mi).get(0).resolveTypeBinding());
        } else if (isMethod(mi, List.class.getCanonicalName(), "addAll", int.class.getSimpleName(), Collection.class.getCanonicalName())) {
            return resolveTypeCompatibleIfPossible(arguments(mi).get(1).resolveTypeBinding());
        } else if (isMethod(mi, List.class.getCanonicalName(), "get", int.class.getSimpleName()) || isMethod(mi, List.class.getCanonicalName(), "remove")
                || isMethod(mi, List.class.getCanonicalName(), "remove", int.class.getSimpleName()) || isMethod(mi, LinkedList.class.getCanonicalName(), "element")
                || isMethod(mi, LinkedList.class.getCanonicalName(), "getFirst") || isMethod(mi, LinkedList.class.getCanonicalName(), "getLast")
                || isMethod(mi, LinkedList.class.getCanonicalName(), "peek") || isMethod(mi, LinkedList.class.getCanonicalName(), "peekFirst")
                || isMethod(mi, LinkedList.class.getCanonicalName(), "peekLast") || isMethod(mi, LinkedList.class.getCanonicalName(), "poll")
                || isMethod(mi, LinkedList.class.getCanonicalName(), "pollFirst") || isMethod(mi, LinkedList.class.getCanonicalName(), "pollLast")
                || isMethod(mi, LinkedList.class.getCanonicalName(), "pop") || isMethod(mi, LinkedList.class.getCanonicalName(), "removeFirst")
                || isMethod(mi, LinkedList.class.getCanonicalName(), "removeLast")
                || isMethod(mi, Vector.class.getCanonicalName(), "elementAt", int.class.getSimpleName())
                || isMethod(mi, Vector.class.getCanonicalName(), "firstElement") || isMethod(mi, Vector.class.getCanonicalName(), "lastElement")
                || isMethod(mi, Stack.class.getCanonicalName(), "pop") || isMethod(mi, Stack.class.getCanonicalName(), "peek")) {
            if (isExprReceived(mi)) {
                ITypeBinding newElementType= getTargetType(mi);
                return resolveTypeCompatible(newElementType);
            } else {
                return true;
            }
        } else if (isMethod(mi, LinkedList.class.getCanonicalName(), "descendingIterator")
                || isMethod(mi, List.class.getCanonicalName(), "iterator") || isMethod(mi, List.class.getCanonicalName(), "listIterator")
                || isMethod(mi, List.class.getCanonicalName(), "listIterator", int.class.getSimpleName())
                || isMethod(mi, List.class.getCanonicalName(), "spliterator") || isMethod(mi, Vector.class.getCanonicalName(), "elements")) {
            if (isExprReceived(mi)) {
                ITypeBinding newElementType= getTargetType(mi);
                return resolveTypeCompatibleIfPossible(newElementType);
            } else {
                return true;
            }
        } else if (isMethod(mi, List.class.getCanonicalName(), "subList", int.class.getSimpleName(), int.class.getSimpleName())
                || isMethod(mi, Collection.class.getCanonicalName(), "toArray")) {
            if (isExprReceived(mi)) {
                ITypeBinding newCollectionType= getTargetType(mi);
                if (newCollectionType != null) {
                    ITypeBinding newElementType= newCollectionType.getElementType();
                    return resolveTypeCompatible(newElementType);
                }
            } else {
                return true;
            }
        }

        return false;
    }

    private boolean resolveTypeCompatibleIfPossible(ITypeBinding paramType) {
        return isParameterizedTypeWithOneArgument(paramType) && resolveTypeCompatible(paramType.getTypeArguments()[0]);
    }

    private boolean isParameterizedTypeWithOneArgument(ITypeBinding typeBinding) {
        return typeBinding != null && typeBinding.isParameterizedType() && typeBinding.getTypeArguments().length == 1;
    }

    private boolean isExprReceived(final ASTNode node) {
        final ASTNode parent= node.getParent();
        if (parent instanceof ParenthesizedExpression) {
            return isExprReceived(parent);
        } else {
            return !(parent instanceof ExpressionStatement);
        }
    }

    private boolean resolveTypeCompatible(ITypeBinding newElementType) {
        if (newElementType == null) {
            return false;
        }
        if (newElementType.isPrimitive()) {
            newElementType= getBoxedTypeBinding(newElementType, ctx.getAST());
        }
        if (!hasType(newElementType, Object.class.getCanonicalName())
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
        return super.isTypeCompatible(variableType, refType) || hasType(variableType,
                getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}
