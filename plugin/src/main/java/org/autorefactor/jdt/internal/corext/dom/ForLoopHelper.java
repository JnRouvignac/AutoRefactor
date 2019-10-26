/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** Helper class for dealing with loops. */
public final class ForLoopHelper {
    private ForLoopHelper() {
    }

    /** The element container that the for loop iterates over. */
    public enum ContainerType {
        /** Means the for loop iterates over an array. */
        ARRAY,
        /** Means the for loop iterates over a collection. */
        COLLECTION
    }

    /** The for loop iteration type. */
    public enum IterationType {
        /** The for loop iterates using an integer index. */
        INDEX,
        /** The for loop iterates using an iterator. */
        ITERATOR,
        /**
         * The for loop iterates via a foreach. Technically this could be desugared by
         * using an iterator.
         */
        FOREACH
    }

    /** The content of the for loop. */
    public static final class ForLoopContent {
        private IterationType iterationType;
        private ContainerType containerType;
        private Expression containerVariable;
        private Expression iteratorVariable;
        private Name loopVariable;
        private Name elementVariable;
        private boolean isLoopingForward;

        private ForLoopContent() {
            // Use method factories
        }

        private static ForLoopContent indexedArray(Expression containerVariable, Name loopVariable, boolean isLoopingForward) {
            final ForLoopContent content= new ForLoopContent();
            content.iterationType= IterationType.INDEX;
            content.containerType= ContainerType.ARRAY;
            content.containerVariable= containerVariable;
            content.loopVariable= loopVariable;
            content.isLoopingForward= isLoopingForward;
            return content;
        }

        private static ForLoopContent indexedCollection(Expression containerVariable, Name loopVariable, boolean isLoopingForward) {
            final ForLoopContent content= new ForLoopContent();
            content.iterationType= IterationType.INDEX;
            content.containerType= ContainerType.COLLECTION;
            content.containerVariable= containerVariable;
            content.loopVariable= loopVariable;
            content.isLoopingForward= isLoopingForward;
            return content;
        }

        private static ForLoopContent iteratedCollection(Expression containerVariable, Expression iteratorVariable) {
            final ForLoopContent content= new ForLoopContent();
            content.iterationType= IterationType.ITERATOR;
            content.containerType= ContainerType.COLLECTION;
            content.containerVariable= containerVariable;
            content.iteratorVariable= iteratorVariable;
            content.isLoopingForward= true;
            return content;
        }

        /**
         * Returns the name of the index variable.
         *
         * @return the name of the index variable
         */
        public Name getLoopVariable() {
            return loopVariable;
        }

        /**
         * Returns the name of each elements extracted from the container.
         *
         * @return the name of each elements extracted from the container
         */
        public Name getElementVariable() {
            return elementVariable;
        }

        /**
         * Returns the name of the container variable.
         *
         * @return the name of the container variable
         */
        public Expression getContainerVariable() {
            return containerVariable;
        }

        /**
         * Returns the name of the iterator variable.
         *
         * @return the name of the iterator variable
         */
        public Expression getIteratorVariable() {
            return iteratorVariable;
        }

        /**
         * Returns the container type.
         *
         * @return the container type
         */
        public ContainerType getContainerType() {
            return containerType;
        }

        /**
         * Returns the for loop's iteration type.
         *
         * @return the for loop's iteration type
         */
        public IterationType getIterationType() {
            return iterationType;
        }

        /**
         * Returns true if the loop iterate from the start to the end of the container.
         *
         * @return true if the loop iterate from the start to the end of the container
         */
        public boolean isLoopingForward() {
            return isLoopingForward;
        }

        @Override
        public String toString() {
            return getClass().getSimpleName() + "(" + "iterationType=" + iterationType + ", containerType=" //$NON-NLS-1$ $NON-NLS-2$ $NON-NLS-3$
                    + containerType + ", containerVariable=" + containerVariable + ", iteratorVariable=" //$NON-NLS-1$ $NON-NLS-2$
                    + iteratorVariable + ", loopVariable=" + loopVariable + ", elementVariable=" + elementVariable //$NON-NLS-1$ $NON-NLS-2$
                    + ", isLoopingForward=" + isLoopingForward //$NON-NLS-1$
                    + ")"; //$NON-NLS-1$
        }
    }

    /**
     * Returns the {@link ForLoopContent} if this for loop iterates over a
     * container.
     *
     * @param node the for statement
     * @return the {@link ForLoopContent} if this for loop iterates over a
     *         container, null otherwise
     */
    public static ForLoopContent iterateOverContainer(ForStatement node) {
        final List<Expression> initializers= ASTNodes.initializers(node);
        final Expression condition= node.getExpression();
        final List<Expression> updaters= ASTNodes.updaters(node);

        if (initializers.size() == 1) {
            Expression firstInit= initializers.get(0);

            if (updaters.isEmpty()) {
                final Pair<Expression, Expression> initPair= decomposeInitializer(firstInit);
                final Expression init= initPair.getFirst();
                final MethodInvocation condMi= ASTNodes.as(node.getExpression(), MethodInvocation.class);
                final MethodInvocation initMi= ASTNodes.as(initPair.getSecond(), MethodInvocation.class);

                if (condMi != null && ASTNodes.isSameVariable(init, condMi.getExpression())
                        && ASTNodes.usesGivenSignature(initMi, Collection.class.getCanonicalName(), "iterator") //$NON-NLS-1$
                        && ASTNodes.usesGivenSignature(condMi, Iterator.class.getCanonicalName(), "hasNext")) { //$NON-NLS-1$
                    return getIteratorOnCollection(initMi.getExpression(), condMi.getExpression());
                }
            } else if (updaters.size() == 1 && ASTNodes.isPrimitive(firstInit, int.class.getSimpleName())) {
                final Pair<Expression, Expression> initPair= decomposeInitializer(firstInit);
                final Expression init= initPair.getFirst();
                final Expression startValue= initPair.getSecond();
                Long zero= ASTNodes.integerLiteral(startValue);
                final InfixExpression startValueMinusOne= ASTNodes.as(startValue, InfixExpression.class);
                Expression collectionOnSize= null;
                Expression arrayOnLength= null;

                if (startValueMinusOne != null && !startValueMinusOne.hasExtendedOperands() && ASTNodes.hasOperator(startValueMinusOne, InfixExpression.Operator.MINUS)) {
                    final Long one= ASTNodes.integerLiteral(startValueMinusOne.getRightOperand());

                    if (one != null && one == 1) {
                        collectionOnSize= getCollectionOnSize(startValueMinusOne.getLeftOperand());
                        arrayOnLength= getArrayOnLength(startValueMinusOne.getLeftOperand());
                    }
                }

                final ForLoopContent forContent= getIndexOnIterable(condition, init, zero, collectionOnSize, arrayOnLength);
                final Name updater= getUpdaterOperand(updaters.get(0), zero != null && zero == 0);

                if (forContent != null && ASTNodes.isSameVariable(init, forContent.loopVariable)
                        && ASTNodes.isSameVariable(init, updater)) {
                    return forContent;
                }
            }
        }

        return null;
    }

    private static ForLoopContent getIteratorOnCollection(Expression containerVar, Expression iteratorVariable) {
        if (containerVar instanceof Name || containerVar instanceof FieldAccess) {
            return ForLoopContent.iteratedCollection(containerVar, iteratorVariable);
        }

        return null;
    }

    private static Name getUpdaterOperand(Expression updater, boolean isLoopingForward) {
        Expression updaterOperand= null;

        if (updater instanceof PostfixExpression) {
            final PostfixExpression pe= (PostfixExpression) updater;

            if (isLoopingForward ? ASTNodes.hasOperator(pe, PostfixExpression.Operator.INCREMENT) : ASTNodes.hasOperator(pe, PostfixExpression.Operator.DECREMENT)) {
                updaterOperand= pe.getOperand();
            }
        } else if (updater instanceof PrefixExpression) {
            final PrefixExpression pe= (PrefixExpression) updater;

            if (isLoopingForward ? ASTNodes.hasOperator(pe, PrefixExpression.Operator.INCREMENT) : ASTNodes.hasOperator(pe, PrefixExpression.Operator.DECREMENT)) {
                updaterOperand= pe.getOperand();
            }
        }

        if (updaterOperand instanceof Name) {
            return (Name) updaterOperand;
        }

        return null;
    }

    /**
     * Decomposes an initializer into a {@link Pair} with the name of the
     * initialized variable and the initializing expression.
     *
     * @param init the initializer to decompose
     * @return a {@link Pair} with the name of the initialized variable and the
     *         initializing expression, or {@code null} if the initializer could not
     *         be decomposed
     */
    public static Pair<Expression, Expression> decomposeInitializer(Expression init) {
        if (init instanceof VariableDeclarationExpression) {
            final VariableDeclarationExpression vde= (VariableDeclarationExpression) init;
            final List<VariableDeclarationFragment> fragments= ASTNodes.fragments(vde);

            if (fragments.size() == 1) {
                final VariableDeclarationFragment fragment= fragments.get(0);
                return Pair.of((Name) fragment.getName(), fragment.getInitializer());
            }
        } else if (init instanceof Assignment) {
            final Assignment as= (Assignment) init;

            if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN)) {
                final Name name= ASTNodes.as(as.getLeftHandSide(), Name.class);
                final FieldAccess fieldAccess= ASTNodes.as(as.getLeftHandSide(), FieldAccess.class);

                if (name != null) {
                    return Pair.of(name, as.getRightHandSide());
                }

                if (fieldAccess != null) {
                    return Pair.of(fieldAccess, as.getRightHandSide());
                }
            }
        }

        return Pair.empty();
    }

    private static ForLoopContent getIndexOnIterable(final Expression condition, Expression loopVariable, Long zero, Expression collectionOnSize, Expression arrayOnLength) {
        final InfixExpression ie= ASTNodes.as(condition, InfixExpression.class);

        if (ie != null && !ie.hasExtendedOperands()) {
            final Expression leftOp= ie.getLeftOperand();
            final Expression rightOp= ie.getRightOperand();

            if (!(loopVariable instanceof Name)) {
                return null;
            }

            if (zero != null && zero == 0) {
                if (ASTNodes.hasOperator(ie, InfixExpression.Operator.LESS) && ASTNodes.isSameLocalVariable(loopVariable, leftOp)) {
                    return buildForLoopContent((Name) loopVariable, rightOp, zero, collectionOnSize, arrayOnLength);
                }
                if (ASTNodes.hasOperator(ie, InfixExpression.Operator.GREATER) && ASTNodes.isSameLocalVariable(loopVariable, rightOp)) {
                    return buildForLoopContent((Name) loopVariable, leftOp, zero, collectionOnSize, arrayOnLength);
                }
            } else if (collectionOnSize != null || arrayOnLength != null) {
                if (ASTNodes.hasOperator(ie, InfixExpression.Operator.GREATER_EQUALS) && ASTNodes.isSameLocalVariable(loopVariable, leftOp)) {
                    return buildForLoopContent((Name) loopVariable, rightOp, zero, collectionOnSize, arrayOnLength);
                }
                if (ASTNodes.hasOperator(ie, InfixExpression.Operator.LESS_EQUALS) && ASTNodes.isSameLocalVariable(loopVariable, rightOp)) {
                    return buildForLoopContent((Name) loopVariable, leftOp, zero, collectionOnSize, arrayOnLength);
                }
            }
        }

        return null;
    }

    private static ForLoopContent buildForLoopContent(final Name loopVar, final Expression containerVar, Long zero, Expression collectionOnSize, Expression arrayOnLength) {
        Long zero2= ASTNodes.integerLiteral(containerVar);
        Expression collectionOnSize2= getCollectionOnSize(containerVar);
        Expression arrayOnLength2= getArrayOnLength(containerVar);

        if (zero != null && zero == 0) {
            if (collectionOnSize2 != null) {
                return ForLoopContent.indexedCollection(collectionOnSize2, loopVar, true);
            }

            if (arrayOnLength2 != null) {
                return ForLoopContent.indexedArray(arrayOnLength2, loopVar, true);
            }
        } else if (zero2 != null && zero2 == 0) {
            if (collectionOnSize != null) {
                return ForLoopContent.indexedCollection(collectionOnSize, loopVar, false);
            }

            if (arrayOnLength != null) {
                return ForLoopContent.indexedArray(arrayOnLength, loopVar, false);
            }
        }

        return null;
    }

    private static Expression getCollectionOnSize(final Expression containerVar) {
        final MethodInvocation mi= ASTNodes.as(containerVar, MethodInvocation.class);

        if (mi != null) {
            final Expression containerVarName= ASTNodes.getUnparenthesedExpression(mi.getExpression());

            if (containerVarName != null && ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "size")) { //$NON-NLS-1$
                return containerVarName;
            }
        }

        return null;
    }

    private static Expression getArrayOnLength(final Expression containerVar) {
        if (containerVar instanceof QualifiedName) {
            final QualifiedName containerVarName= (QualifiedName) containerVar;

            if (ASTNodes.isArray(containerVarName.getQualifier()) && "length".equals(containerVarName.getName().getIdentifier())) { //$NON-NLS-1$
                return containerVarName.getQualifier();
            }
        } else if (containerVar instanceof FieldAccess) {
            final FieldAccess containerVarName= (FieldAccess) containerVar;

            if (ASTNodes.isArray(containerVarName.getExpression()) && "length".equals(containerVarName.getName().getIdentifier())) { //$NON-NLS-1$
                return containerVarName.getExpression();
            }
        }

        return null;
    }
}
