/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

public class OperatorEnumTest {

	@DataProvider
	public Iterator<Object[]> getPairsOfOperatorsWithSamePrecendence() {
		final List<Object[]> results = new ArrayList<Object[]>();
		for (int i = 0; i < OperatorEnum.values().length; i++) {
			OperatorEnum op1 = OperatorEnum.values()[i];
			for (int j = i; j < OperatorEnum.values().length; j++) {
				OperatorEnum op2 = OperatorEnum.values()[j];
				if (op1 != op2 && op1.getPrecedence() == op2.getPrecedence()) {
					results.add(new Object[] { op1, op2 });
				}
			}
		}
		return results.iterator();
	}

	@DataProvider
	public Iterator<Object[]> getPairsOfOperatorsWithDifferentPrecendence() {
		final List<Object[]> results = new ArrayList<Object[]>();
		for (int i = 0; i < OperatorEnum.values().length; i++) {
			OperatorEnum op1 = OperatorEnum.values()[i];
			for (int j = i + 1; j < OperatorEnum.values().length; j++) {
				OperatorEnum op2 = OperatorEnum.values()[j];
				if (op1.getPrecedence() != op2.getPrecedence()) {
					results.add(new Object[] { op1, op2 });
				}
			}
		}
		return results.iterator();
	}

	@Test(dataProvider = "getPairsOfOperatorsWithSamePrecendence")
	public void compareSamePrecendenceOperators(OperatorEnum op1, OperatorEnum op2) {
		assertTrue(OperatorEnum.compareTo(op1, op2) == 0, "Expected but did not get: " + op1 + " == " + op2);
	}

	@Test(dataProvider = "getPairsOfOperatorsWithDifferentPrecendence")
	public void compareDifferentPrecendenceOperators(OperatorEnum op1, OperatorEnum op2) {
		assertTrue(OperatorEnum.compareTo(op1, op2) > 0, "Expected but did not get: " + op1 + " > " + op2);
		assertTrue(OperatorEnum.compareTo(op2, op1) < 0, "Expected but did not get: " + op1 + " < " + op2);
	}

	@Test
	public void simpleTestCompareExpressions() {
		final AST ast = AST.newAST(AST.JLS4);
		final Assignment op1 = ast.newAssignment();
		op1.setOperator(Assignment.Operator.ASSIGN);
		final InfixExpression op2 = ast.newInfixExpression();
		op2.setOperator(InfixExpression.Operator.CONDITIONAL_AND);

		assertTrue(OperatorEnum.compareTo(op1, op2) < 0);

		assertEquals(OperatorEnum.compareTo(op1, null), 0, "Comparing unknown objects result in no decision");
	}
}
