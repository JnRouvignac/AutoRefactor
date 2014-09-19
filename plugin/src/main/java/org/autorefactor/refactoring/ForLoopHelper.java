/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.*;

import java.util.List;

import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/**
 * Helper class for dealing with loops.
 */
public final class ForLoopHelper {

    private ForLoopHelper() {
        super();
    }

    /** The element container that the for loop iterates over. */
    public static enum ContainerType {
        /** Means the for loop iterates over an array. */
        ARRAY,
        /** Means the for loop iterates over a collection. */
        COLLECTION
    }

    /** The for loop iteration type. */
    public static enum IterationType {
        /** The for loop iterates using an integer index. */
        INDEX,
        /** The for loop iterates using an iterator. */
        ITERATOR,
        /** The for loop iterates via a foreach. Technically this could be desugared by using an iterator. */
        FOREACH
    }

    /**
     * The content of the for loop.
     */
    public static final class ForLoopContent {
        private Name loopVariable;
        private Name elementVariable;
        private Name containerVariable;
        private ContainerType containerType;
        private IterationType iterationType;

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
        if (initializers.size() == 1 && updaters.size() == 1) {
            final Name init = getInitializerOperand(initializers.get(0));
            final ForLoopContent forContent = getIndexOnCollection(condition);
            final Name updater = getUpdaterOperand(updaters.get(0));
            if (forContent != null
                    && isSameVariable(init, forContent.loopVariable)
                    && isSameVariable(init, updater)) {
                return forContent;
            }
        }
        return null;
    }

    private static Name getUpdaterOperand(Expression updater) {
        Expression updaterOperand = null;
        if (updater instanceof PostfixExpression) {
            final PostfixExpression pe = (PostfixExpression) updater;
            if (PostfixExpression.Operator.INCREMENT.equals(pe.getOperator())) {
                updaterOperand = pe.getOperand();
            }
        } else if (updater instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) updater;
            if (PrefixExpression.Operator.INCREMENT.equals(pe.getOperator())) {
                updaterOperand = pe.getOperand();
            }
        }
        if (updaterOperand instanceof Name) {
            return (Name) updaterOperand;
        }
        return null;
    }

    private static Name getInitializerOperand(Expression init) {
        if (!isPrimitive(init, "int")) {
            return null;
        }
        if (init instanceof VariableDeclarationExpression) {
            final VariableDeclarationExpression vde = (VariableDeclarationExpression) init;
            final List<VariableDeclarationFragment> fragments = fragments(vde);
            if (fragments.size() == 1) {
                final VariableDeclarationFragment fragment = fragments.get(0);
                if (isZero(fragment.getInitializer())) {
                    return fragment.getName();
                }
            }
        } else if (init instanceof Assignment) {
            final Assignment as = (Assignment) init;
            if (Assignment.Operator.ASSIGN.equals(as.getOperator())
                    && isZero(as.getRightHandSide())
                    && as.getLeftHandSide() instanceof Name) {
                return (Name) as.getLeftHandSide();
            }
        }
        return null;
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

    private static ForLoopContent getIndexOnCollection(final Expression condition) {
        final InfixExpression ie = as(condition, InfixExpression.class);
        if (ie != null && !ie.hasExtendedOperands()) {
            final Expression leftOp = ie.getLeftOperand();
            final Expression rightOp = ie.getRightOperand();
            if (InfixExpression.Operator.LESS.equals(ie.getOperator())) {
                return buildForLoopContent(leftOp, rightOp);
            } else if (InfixExpression.Operator.GREATER.equals(ie.getOperator())) {
                return buildForLoopContent(rightOp, leftOp);
            }
        }
        return null;
    }

    private static ForLoopContent buildForLoopContent(final Expression loopVar, final Expression containerVar) {
        if (containerVar instanceof MethodInvocation
                && loopVar instanceof Name) {
            final MethodInvocation mi = (MethodInvocation) containerVar;
            final Name containerVarName = as(mi.getExpression(), Name.class);
            if (isMethod(mi, "java.util.Collection", "size")
                    && containerVarName != null) {
                final ForLoopContent content = new ForLoopContent();
                content.loopVariable = (Name) loopVar;
                content.containerVariable = containerVarName;
                content.containerType = ContainerType.COLLECTION;
                content.iterationType = IterationType.INDEX;
                return content;
            }
        }
        return null;
    }

}
