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
package org.autorefactor.refactoring;

import java.util.List;

import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** Helper class for dealing with loops. */
public final class ForLoopHelper {
    private ForLoopHelper() {
        super();
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
        /** The for loop iterates via a foreach. Technically this could be desugared by using an iterator. */
        FOREACH
    }

    /** The content of the for loop. */
    public static final class ForLoopContent {
        private IterationType iterationType;
        private ContainerType containerType;
        private Name containerVariable;
        private Expression iteratorVariable;
        private Name loopVariable;
        private Name elementVariable;

        private ForLoopContent() {
           // use method factories
        }

        private static ForLoopContent indexedArray(Name containerVariable, Name loopVariable) {
            final ForLoopContent content = new ForLoopContent();
            content.iterationType = IterationType.INDEX;
            content.containerType = ContainerType.ARRAY;
            content.containerVariable = containerVariable;
            content.loopVariable = loopVariable;
            return content;
        }

        private static ForLoopContent indexedCollection(Name containerVariable, Name loopVariable) {
            final ForLoopContent content = new ForLoopContent();
            content.iterationType = IterationType.INDEX;
            content.containerType = ContainerType.COLLECTION;
            content.containerVariable = containerVariable;
            content.loopVariable = loopVariable;
            return content;
        }

        private static ForLoopContent iteratedCollection(Expression iteratorVariable, Name containerVariable) {
            final ForLoopContent content = new ForLoopContent();
            content.iterationType = IterationType.ITERATOR;
            content.containerType = ContainerType.COLLECTION;
            content.containerVariable = containerVariable;
            content.iteratorVariable = iteratorVariable;
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
        public Name getContainerVariable() {
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

        @Override
        public String toString() {
            return getClass().getSimpleName() + "("
                    + "iterationType=" + iterationType
                    + ", containerType=" + containerType
                    + ", containerVariable=" + containerVariable
                    + ", iteratorVariable=" + iteratorVariable
                    + ", loopVariable=" + loopVariable
                    + ", elementVariable=" + elementVariable
                    + ")";
        }
    }

    /**
     * Returns the {@link ForLoopContent} if this for loop iterates over a container.
     *
     * @param node the for statement
     * @return the {@link ForLoopContent} if this for loop iterates over a container, null otherwise
     */
    public static ForLoopContent iterateOverContainer(ForStatement node) {
        final List<Expression> initializers = initializers(node);
        final Expression condition = node.getExpression();
        final List<Expression> updaters = updaters(node);
        if (initializers.size() == 1) {
            Expression firstInit = initializers.get(0);
            if (updaters.isEmpty()) {
                final Pair<Name, Expression> initPair = decomposeInitializer(firstInit);
                final Name init = initPair.getFirst();
                final MethodInvocation condMi = as(node.getExpression(), MethodInvocation.class);
                final MethodInvocation initMi = as(initPair.getSecond(), MethodInvocation.class);
                if (condMi != null
                        && isSameVariable(init, condMi.getExpression())
                        && isMethod(initMi, "java.util.Collection", "iterator")
                        && isMethod(condMi, "java.util.Iterator", "hasNext")) {
                    return getIteratorOnCollection(initMi.getExpression(), condMi.getExpression());
                }
            } else if (updaters.size() == 1
                    && isPrimitive(firstInit, "int")) {
                final Pair<Name, Expression> initPair = decomposeInitializer(firstInit);
                final Name init = initPair.getFirst();
                final ForLoopContent forContent = getIndexOnIterable(condition, init);
                final Name updater = getUpdaterOperand(updaters.get(0));
                if (forContent != null
                        && isZero(initPair.getSecond())
                        && isSameVariable(init, forContent.loopVariable)
                        && isSameVariable(init, updater)) {
                    return forContent;
                }
            }
        }
        return null;
    }

    private static ForLoopContent getIteratorOnCollection(Expression containerVar, Expression iteratorVariable) {
        if (containerVar instanceof Name) {
            return ForLoopContent.iteratedCollection(iteratorVariable, (Name) containerVar);
        }
        return null;
    }

    private static Name getUpdaterOperand(Expression updater) {
        Expression updaterOperand = null;
        if (updater instanceof PostfixExpression) {
            final PostfixExpression pe = (PostfixExpression) updater;
            if (hasOperator(pe, PostfixExpression.Operator.INCREMENT)) {
                updaterOperand = pe.getOperand();
            }
        } else if (updater instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) updater;
            if (hasOperator(pe, PrefixExpression.Operator.INCREMENT)) {
                updaterOperand = pe.getOperand();
            }
        }
        if (updaterOperand instanceof Name) {
            return (Name) updaterOperand;
        }
        return null;
    }

    /**
     * Decomposes an initializer into a {@link Pair} with the name of the initialized variable
     * and the initializing expression.
     *
     * @param init
     *          the initializer to decompose
     * @return a {@link Pair} with the name of the initialized variable and the initializing
     *         expression, or {@code null} if the initializer could not be decomposed
     */
    public static Pair<Name, Expression> decomposeInitializer(Expression init) {
        if (init instanceof VariableDeclarationExpression) {
            final VariableDeclarationExpression vde = (VariableDeclarationExpression) init;
            final List<VariableDeclarationFragment> fragments = fragments(vde);
            if (fragments.size() == 1) {
                final VariableDeclarationFragment fragment = fragments.get(0);
                return Pair.of((Name) fragment.getName(), fragment.getInitializer());
            }
        } else if (init instanceof Assignment) {
            final Assignment as = (Assignment) init;
            if (hasOperator(as, ASSIGN)
                    && as.getLeftHandSide() instanceof Name) {
                return Pair.of((Name) as.getLeftHandSide(), as.getRightHandSide());
            }
        }
        return Pair.empty();
    }

    private static boolean isZero(final Expression expr) {
        if (expr != null) {
            final Object val = expr.resolveConstantExpressionValue();
            if (val instanceof Integer) {
                return ((Integer) val).intValue() == 0;
            }
        }
        return false;
    }

    private static ForLoopContent getIndexOnIterable(final Expression condition, Name loopVariable) {
        final InfixExpression ie = as(condition, InfixExpression.class);
        if (ie != null && !ie.hasExtendedOperands()) {
            final Expression leftOp = ie.getLeftOperand();
            final Expression rightOp = ie.getRightOperand();
            if (hasOperator(ie, LESS)) {
                return buildForLoopContent(loopVariable, rightOp);
            } else if (hasOperator(ie, GREATER)) {
                return buildForLoopContent(loopVariable, leftOp);
            }
        }
        return null;
    }

    private static ForLoopContent buildForLoopContent(final Expression loopVar, final Expression containerVar) {
        if (!(loopVar instanceof Name)) {
            return null;
        }
        if (containerVar instanceof MethodInvocation) {
            final MethodInvocation mi = (MethodInvocation) containerVar;
            final Name containerVarName = as(mi.getExpression(), Name.class);
            if (containerVarName != null
                    && isMethod(mi, "java.util.Collection", "size")) {
                return ForLoopContent.indexedCollection(containerVarName, (Name) loopVar);
            }
        } else if (containerVar instanceof QualifiedName) {
            final QualifiedName containerVarName = (QualifiedName) containerVar;
            if (isArrayLength(containerVarName)) {
                Name containerVariable = ((QualifiedName) containerVar).getQualifier();
                return ForLoopContent.indexedArray(containerVariable, (Name) loopVar);
            }
        }
        return null;
    }

    private static boolean isArrayLength(final QualifiedName containerVarName) {
        return isArray(containerVarName.getQualifier())
                && containerVarName.getName().getIdentifier().equals("length");
    }
}
