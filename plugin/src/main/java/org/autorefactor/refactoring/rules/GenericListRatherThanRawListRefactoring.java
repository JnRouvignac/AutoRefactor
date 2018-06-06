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
package org.autorefactor.refactoring.rules;

import java.util.HashMap;
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

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;

/** See {@link #getDescription()} method. */
public class GenericListRatherThanRawListRefactoring extends AbstractClassSubstituteRefactoring {
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
        CAN_BE_CASTED_TO.put("java.util.ArrayList",
                new String[]{"java.util.ArrayList", "java.util.AbstractList", "java.util.List",
                    "java.util.AbstractCollection", "java.util.Collection",
                    "java.io.Serializable", "java.lang.Cloneable", "java.lang.Object"});
    }

    private ITypeBinding elementType;

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Generic list rather than raw list";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Genericize a list if possible.";
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
        elementType = null;
        return super.visit(node);

    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.util.LinkedList", "java.util.ArrayList", "java.util.Vector", "java.util.Stack"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        return origRawType;
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
        if (origType.isParameterizedType()) {
            return null;
        }

        final TypeNameDecider typeNameDecider = new TypeNameDecider(originalExpr);

        final ParameterizedType parameterizedType = b.getAST().newParameterizedType(b.copy(origType));
        parameterizedType.typeArguments().clear();
        parameterizedType.typeArguments().add(b.toType(elementType, typeNameDecider));

        return parameterizedType;
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        elementType = null;

        if (instanceCreation.resolveTypeBinding().isParameterizedType()) {
            return false;
        }

        ITypeBinding[] parameterTypes = instanceCreation.resolveConstructorBinding().getParameterTypes();

        if (parameterTypes.length > 0
                && hasType(parameterTypes[0], "java.util.Collection")) {
            ITypeBinding actualParameter = ((Expression) instanceCreation.arguments().get(0)).resolveTypeBinding();

            if (actualParameter.isParameterizedType() && actualParameter.getTypeArguments().length == 1) {
                ITypeBinding newElementType = actualParameter.getTypeArguments()[0];
                return resolveTypeCompatible(
                        newElementType);
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

        if (isMethod(mi, "java.util.Collection", "clear")
                || isMethod(mi, "java.lang.Object", "equals", "java.lang.Object")
                || isMethod(mi, "java.lang.Object", "hashCode")
                || isMethod(mi, "java.util.Collection", "size")
                || isMethod(mi, "java.util.Collection", "isEmpty")
                || isMethod(mi, "java.lang.Object", "toString")
                || isMethod(mi, "java.lang.Object", "finalize")
                || isMethod(mi, "java.lang.Object", "notify")
                || isMethod(mi, "java.lang.Object", "notifyAll")
                || isMethod(mi, "java.lang.Object", "wait")
                || isMethod(mi, "java.lang.Object", "wait", "long")
                || isMethod(mi, "java.lang.Object", "wait", "long", "int")
                || isMethod(mi, "java.util.ArrayList", "ensureCapacity", "int")
                || isMethod(mi, "java.util.ArrayList", "removeRange", "int", "int")
                || isMethod(mi, "java.util.ArrayList", "forEach", "java.util.function.Consumer")
                || isMethod(mi, "java.util.ArrayList", "removeIf", "java.util.function.Predicate")
                || isMethod(mi, "java.util.ArrayList", "sort", "java.util.Comparator")
                || isMethod(mi, "java.util.Vector", "trimToSize")
                || isMethod(mi, "java.util.Vector", "ensureCapacity", "int")
                || isMethod(mi, "java.util.Vector", "setSize", "int")
                || isMethod(mi, "java.util.Vector", "capacity")
                || isMethod(mi, "java.util.Vector", "removeElementAt", "int")
                || isMethod(mi, "java.util.Vector", "removeAllElements")
                || isMethod(mi, "java.util.Stack", "empty")) {
            return true;
        } else if (isMethod(mi, "java.util.Collection", "add", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "contains", "java.lang.Object")
                || isMethod(mi, "java.util.List", "indexOf", "java.lang.Object")
                || isMethod(mi, "java.util.List", "lastIndexOf", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "addFirst", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "addLast", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "offer", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "offerFirst", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "offerLast", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "push", "java.lang.Object")
                || isMethod(mi, "java.util.Collection", "remove", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "removeFirstOccurrence", "java.lang.Object")
                || isMethod(mi, "java.util.LinkedList", "removeLastOccurrence", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "indexOf", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "lastIndexOf", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "setElementAt", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "insertElementAt", "java.lang.Object", "int")
                || isMethod(mi, "java.util.Vector", "addElement", "java.lang.Object")
                || isMethod(mi, "java.util.Vector", "removeElement", "java.lang.Object")
                || isMethod(mi, "java.util.Stack", "push", "java.lang.Object")
                || isMethod(mi, "java.util.Stack", "search", "java.lang.Object")) {
            ITypeBinding newElementType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();
            return resolveTypeCompatible(newElementType);
        } else if (isMethod(mi, "java.util.List", "add", "int", "java.lang.Object")
                || isMethod(mi, "java.util.List", "set", "int", "java.lang.Object")) {
            return resolveTypeCompatible(((Expression) mi.arguments().get(1)).resolveTypeBinding());
        } else if (isMethod(mi, "java.util.Collection", "toArray", "java.lang.Object[]")
                || isMethod(mi, "java.util.Vector", "copyInto", "java.lang.Object[]")) {
            ITypeBinding newElementType = ((Expression) mi.arguments().get(0)).resolveTypeBinding().getElementType();
            return resolveTypeCompatible(newElementType);
        } else if (isMethod(mi, "java.util.Collection", "addAll", "java.util.Collection")
                || isMethod(mi, "java.util.Collection", "containsAll", "java.util.Collection")) {
            ITypeBinding paramType = ((Expression) mi.arguments().get(0)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 1) {
                ITypeBinding newElementType = paramType.getTypeArguments()[0];
                return resolveTypeCompatible(
                        newElementType);
            }
        } else if (isMethod(mi, "java.util.List", "addAll", "int", "java.util.Collection")) {
            ITypeBinding paramType = ((Expression) mi.arguments().get(1)).resolveTypeBinding();

            if (paramType.isParameterizedType() && paramType.getTypeArguments().length == 1) {
                ITypeBinding newElementType = paramType.getTypeArguments()[0];
                return resolveTypeCompatible(
                        newElementType);
            }
        } else if (isMethod(mi, "java.util.List", "get", "int")
                || isMethod(mi, "java.util.List", "remove")
                || isMethod(mi, "java.util.List", "remove", "int")
                || isMethod(mi, "java.util.LinkedList", "element")
                || isMethod(mi, "java.util.LinkedList", "getFirst")
                || isMethod(mi, "java.util.LinkedList", "getLast")
                || isMethod(mi, "java.util.LinkedList", "peek")
                || isMethod(mi, "java.util.LinkedList", "peekFirst")
                || isMethod(mi, "java.util.LinkedList", "peekLast")
                || isMethod(mi, "java.util.LinkedList", "poll")
                || isMethod(mi, "java.util.LinkedList", "pollFirst")
                || isMethod(mi, "java.util.LinkedList", "pollLast")
                || isMethod(mi, "java.util.LinkedList", "pop")
                || isMethod(mi, "java.util.LinkedList", "removeFirst")
                || isMethod(mi, "java.util.LinkedList", "removeLast")
                || isMethod(mi, "java.util.Vector", "elementAt", "int")
                || isMethod(mi, "java.util.Vector", "firstElement")
                || isMethod(mi, "java.util.Vector", "lastElement")
                || isMethod(mi, "java.util.Stack", "pop")
                || isMethod(mi, "java.util.Stack", "peek")) {
            if (isExprReceived(mi)) {
                ITypeBinding newElementType = getDestinationType(mi);
                return resolveTypeCompatible(newElementType);
            } else {
                return true;
            }
        } else if (isMethod(mi, "java.util.LinkedList", "descendingIterator")
                || isMethod(mi, "java.util.List", "iterator")
                || isMethod(mi, "java.util.List", "listIterator")
                || isMethod(mi, "java.util.List", "listIterator", "int")
                || isMethod(mi, "java.util.List", "spliterator")
                || isMethod(mi, "java.util.Vector", "elements")) {
            if (isExprReceived(mi)) {
                ITypeBinding newElementType = getDestinationType(mi);
                if (newElementType != null && newElementType.isParameterizedType()
                        && newElementType.getTypeArguments().length == 1) {
                    return resolveTypeCompatible(newElementType.getTypeArguments()[0]);
                }
            } else {
                return true;
            }
        } else if (isMethod(mi, "java.util.List", "subList", "int", "int")
                || isMethod(mi, "java.util.Collection", "toArray")) {
            if (isExprReceived(mi)) {
                ITypeBinding newCollectionType = getDestinationType(mi);
                if (newCollectionType != null) {
                    ITypeBinding newElementType = newCollectionType.getElementType();
                    return resolveTypeCompatible(newElementType);
                }
            } else {
                return true;
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

    private boolean resolveTypeCompatible(ITypeBinding newElementType) {
        if (newElementType == null) {
            return false;
        }
        if (newElementType.isPrimitive()) {
            newElementType = getBoxedTypeBinding(newElementType, ctx.getAST());
        }
        if (!hasType(newElementType, "java.lang.Object")
                && (elementType == null || newElementType.equals(elementType))) {
            elementType = newElementType;
            return true;
        }
        return false;
    }

    @Override
    protected boolean canCodeBeRefactored() {
        return elementType != null;
    }

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType,
            final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType)
                || hasType(variableType,
                           getOrDefault(CAN_BE_CASTED_TO, refType.getErasure().getQualifiedName(), new String[0]));
    }
}
