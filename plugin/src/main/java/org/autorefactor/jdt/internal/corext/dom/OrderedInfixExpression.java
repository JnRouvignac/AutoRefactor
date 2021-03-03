/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;

/**
 * Ordered infix expression for commutative operations.
 *
 * @param <F> First operand
 * @param <S> Second operand
 */
public class OrderedInfixExpression<F extends Expression, S extends Expression> {
	private final F firstOperand;
	private final InfixExpression.Operator operator;
	private final S secondOperand;

	/**
	 * Ordered infix expression.
	 *
	 * @param firstOperand first operand
	 * @param operator operator
	 * @param secondOperand second operand
	 */
	public OrderedInfixExpression(final F firstOperand, final InfixExpression.Operator operator, final S secondOperand) {
		this.firstOperand= firstOperand;
		this.operator= operator;
		this.secondOperand= secondOperand;
	}

	/**
	 * Get the first operand.
	 *
	 * @return the first operand.
	 */
	public F getFirstOperand() {
		return firstOperand;
	}

	/**
	 * Get the operator.
	 *
	 * @return the operator.
	 */
	public InfixExpression.Operator getOperator() {
		return operator;
	}

	/**
	 * Get the second operand.
	 *
	 * @return the second operand.
	 */
	public S getSecondOperand() {
		return secondOperand;
	}
}
