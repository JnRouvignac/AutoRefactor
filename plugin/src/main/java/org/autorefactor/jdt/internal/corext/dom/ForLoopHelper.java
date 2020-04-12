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
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;

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

		private static ForLoopContent indexedArray(final Expression containerVariable, final Name loopVariable, final boolean isLoopingForward) {
			ForLoopContent content= new ForLoopContent();
			content.iterationType= IterationType.INDEX;
			content.containerType= ContainerType.ARRAY;
			content.containerVariable= containerVariable;
			content.loopVariable= loopVariable;
			content.isLoopingForward= isLoopingForward;
			return content;
		}

		private static ForLoopContent indexedCollection(final Expression containerVariable, final Name loopVariable, final boolean isLoopingForward) {
			ForLoopContent content= new ForLoopContent();
			content.iterationType= IterationType.INDEX;
			content.containerType= ContainerType.COLLECTION;
			content.containerVariable= containerVariable;
			content.loopVariable= loopVariable;
			content.isLoopingForward= isLoopingForward;
			return content;
		}

		private static ForLoopContent iteratedCollection(final Expression containerVariable, final Expression iteratorVariable) {
			ForLoopContent content= new ForLoopContent();
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
			return getClass().getSimpleName() + "(" + "iterationType=" + iterationType + ", containerType=" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
					+ containerType + ", containerVariable=" + containerVariable + ", iteratorVariable=" //$NON-NLS-1$ //$NON-NLS-2$
					+ iteratorVariable + ", loopVariable=" + loopVariable + ", elementVariable=" + elementVariable //$NON-NLS-1$ //$NON-NLS-2$
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
	public static ForLoopContent iterateOverContainer(final ForStatement node) {
		List<Expression> initializers= ASTNodes.initializers(node);
		Expression condition= node.getExpression();
		List<Expression> updaters= ASTNodes.updaters(node);

		if (initializers.size() == 1) {
			Expression firstInit= initializers.get(0);

			if (updaters.isEmpty()) {
				Pair<Expression, Expression> initPair= ASTNodes.decomposeInitializer(firstInit);
				Expression init= initPair.getFirst();
				MethodInvocation condMi= ASTNodes.as(node.getExpression(), MethodInvocation.class);
				MethodInvocation initMi= ASTNodes.as(initPair.getSecond(), MethodInvocation.class);

				if (condMi != null && ASTNodes.isSameVariable(init, condMi.getExpression())
						&& ASTNodes.usesGivenSignature(initMi, Collection.class.getCanonicalName(), "iterator") //$NON-NLS-1$
						&& ASTNodes.usesGivenSignature(condMi, Iterator.class.getCanonicalName(), "hasNext")) { //$NON-NLS-1$
					return getIteratorOnCollection(initMi.getExpression(), condMi.getExpression());
				}
			} else if (updaters.size() == 1 && ASTNodes.isPrimitive(firstInit, int.class.getSimpleName())) {
				Pair<Expression, Expression> initPair= ASTNodes.decomposeInitializer(firstInit);
				Expression init= initPair.getFirst();
				Expression startValue= initPair.getSecond();
				Long zero= ASTNodes.integerLiteral(startValue);
				InfixExpression startValueMinusOne= ASTNodes.as(startValue, InfixExpression.class);
				Expression collectionOnSize= null;
				Expression arrayOnLength= null;

				if (startValueMinusOne != null && !startValueMinusOne.hasExtendedOperands() && ASTNodes.hasOperator(startValueMinusOne, InfixExpression.Operator.MINUS)) {
					Long one= ASTNodes.integerLiteral(startValueMinusOne.getRightOperand());

					if (one != null && one == 1) {
						collectionOnSize= getCollectionOnSize(startValueMinusOne.getLeftOperand());
						arrayOnLength= getArrayOnLength(startValueMinusOne.getLeftOperand());
					}
				}

				ForLoopContent forContent= getIndexOnIterable(condition, init, zero, collectionOnSize, arrayOnLength);
				Name updater= getUpdaterOperand(updaters.get(0), zero != null && zero == 0);

				if (forContent != null && ASTNodes.isSameVariable(init, forContent.loopVariable)
						&& ASTNodes.isSameVariable(init, updater)) {
					return forContent;
				}
			}
		}

		return null;
	}

	private static ForLoopContent getIteratorOnCollection(final Expression containerVar, final Expression iteratorVariable) {
		if (containerVar instanceof Name || containerVar instanceof FieldAccess) {
			return ForLoopContent.iteratedCollection(containerVar, iteratorVariable);
		}

		return null;
	}

	private static Name getUpdaterOperand(final Expression updater, final boolean isLoopingForward) {
		Expression updaterOperand= null;

		if (updater instanceof PostfixExpression) {
			PostfixExpression pe= (PostfixExpression) updater;

			if (isLoopingForward ? ASTNodes.hasOperator(pe, PostfixExpression.Operator.INCREMENT) : ASTNodes.hasOperator(pe, PostfixExpression.Operator.DECREMENT)) {
				updaterOperand= pe.getOperand();
			}
		} else if (updater instanceof PrefixExpression) {
			PrefixExpression pe= (PrefixExpression) updater;

			if (isLoopingForward ? ASTNodes.hasOperator(pe, PrefixExpression.Operator.INCREMENT) : ASTNodes.hasOperator(pe, PrefixExpression.Operator.DECREMENT)) {
				updaterOperand= pe.getOperand();
			}
		}

		if (updaterOperand instanceof Name) {
			return (Name) updaterOperand;
		}

		return null;
	}

	private static ForLoopContent getIndexOnIterable(final Expression condition, final Expression loopVariable, final Long zero, final Expression collectionOnSize, final Expression arrayOnLength) {
		InfixExpression ie= ASTNodes.as(condition, InfixExpression.class);

		if (ie != null && !ie.hasExtendedOperands()) {
			Expression leftOp= ie.getLeftOperand();
			Expression rightOp= ie.getRightOperand();

			if (!(loopVariable instanceof Name)) {
				return null;
			}

			if (zero != null && zero == 0) {
				if (ASTNodes.hasOperator(ie, InfixExpression.Operator.LESS, InfixExpression.Operator.NOT_EQUALS) && ASTNodes.isSameLocalVariable(loopVariable, leftOp)) {
					return buildForLoopContent((Name) loopVariable, rightOp, zero, collectionOnSize, arrayOnLength);
				}
				if (ASTNodes.hasOperator(ie, InfixExpression.Operator.GREATER, InfixExpression.Operator.NOT_EQUALS) && ASTNodes.isSameLocalVariable(loopVariable, rightOp)) {
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

	private static ForLoopContent buildForLoopContent(final Name loopVar, final Expression containerVar, final Long zero, final Expression collectionOnSize, final Expression arrayOnLength) {
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
		MethodInvocation mi= ASTNodes.as(containerVar, MethodInvocation.class);

		if (mi != null) {
			Expression containerVarName= ASTNodes.getUnparenthesedExpression(mi.getExpression());

			if (containerVarName != null && ASTNodes.usesGivenSignature(mi, Collection.class.getCanonicalName(), "size")) { //$NON-NLS-1$
				return containerVarName;
			}
		}

		return null;
	}

	private static Expression getArrayOnLength(final Expression containerVar) {
		if (containerVar instanceof QualifiedName) {
			QualifiedName containerVarName= (QualifiedName) containerVar;

			if (ASTNodes.isArray(containerVarName.getQualifier()) && "length".equals(containerVarName.getName().getIdentifier())) { //$NON-NLS-1$
				return containerVarName.getQualifier();
			}
		} else if (containerVar instanceof FieldAccess) {
			FieldAccess containerVarName= (FieldAccess) containerVar;

			if (ASTNodes.isArray(containerVarName.getExpression()) && "length".equals(containerVarName.getName().getIdentifier())) { //$NON-NLS-1$
				return containerVarName.getExpression();
			}
		}

		return null;
	}
}
