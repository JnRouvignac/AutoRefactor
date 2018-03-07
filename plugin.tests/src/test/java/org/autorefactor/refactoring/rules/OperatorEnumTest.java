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
import org.junit.Test;

import static org.junit.Assert.*;

public class OperatorEnumTest {

    // @DataProvider
    public Iterator<Object[]> getPairsOfOperatorsWithSamePrecedence() {
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

    // @DataProvider
    public Iterator<Object[]> getPairsOfOperatorsWithDifferentPrecedence() {
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

    @Test
    public void compareSamePrecedenceOperators() {
        for (final Iterator<Object[]> iter = getPairsOfOperatorsWithSamePrecedence(); iter.hasNext();) {
            final Object[] args = iter.next();
            compareSamePrecedenceOperators((OperatorEnum) args[0], (OperatorEnum) args[1]);
        }
    }

    public void compareSamePrecedenceOperators(OperatorEnum op1, OperatorEnum op2) {
        assertTrue("Expected but did not get: " + op1 + " == " + op2, OperatorEnum.compareTo(op1, op2) == 0);
    }

    @Test
    public void compareDifferentPrecedenceOperators() {
        for (final Iterator<Object[]> iter = getPairsOfOperatorsWithDifferentPrecedence(); iter.hasNext();) {
            final Object[] args = iter.next();
            compareDifferentPrecedenceOperators((OperatorEnum) args[0], (OperatorEnum) args[1]);
        }
    }

    public void compareDifferentPrecedenceOperators(OperatorEnum op1, OperatorEnum op2) {
        assertTrue("Expected but did not get: " + op1 + " > " + op2, OperatorEnum.compareTo(op1, op2) > 0);
        assertTrue("Expected but did not get: " + op1 + " < " + op2, OperatorEnum.compareTo(op2, op1) < 0);
    }

    @Test
    public void simpleTestCompareExpressions() {
        final AST ast = AST.newAST(AST.JLS8);
        final Assignment op1 = ast.newAssignment();
        op1.setOperator(Assignment.Operator.ASSIGN);
        final InfixExpression op2 = ast.newInfixExpression();
        op2.setOperator(InfixExpression.Operator.CONDITIONAL_AND);

        assertTrue(OperatorEnum.compareTo(op1, op2) < 0);

        assertEquals("Comparing unknown objects result in no decision", 0, OperatorEnum.compareTo(op1, null));
    }
}
