/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - include more cases
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
package org.autorefactor.refactoring.rules.samples_in;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.fail;
import static org.testng.Assert.*;

import org.testng.Assert;

public class TestNGAssertSample {

    private static final int FOURTYTWO = 42;

    public void shouldRefactorWithPrimitives(int i1, int i2) throws Exception {
        Assert.assertEquals(i1, i2);
        Assert.assertEquals(i1, i2, "Failure message to keep");
        Assert.assertNotEquals(i1, i2);
        Assert.assertNotEquals(i1, i2, "Failure message to keep");
        Assert.assertEquals(i1, i2);
        Assert.assertEquals(i1, i2, "Failure message to keep");
        Assert.assertNotEquals(i1, i2);
        Assert.assertNotEquals(i1, i2, "Failure message to keep");

        org.testng.Assert.assertEquals(i1, i2);
        org.testng.Assert.assertEquals(i1, i2, "Failure message to keep");
        org.testng.Assert.assertNotEquals(i1, i2);
        org.testng.Assert.assertNotEquals(i1, i2, "Failure message to keep");
        org.testng.Assert.assertEquals(i1, i2);
        org.testng.Assert.assertEquals(i1, i2, "Failure message to keep");
        org.testng.Assert.assertNotEquals(i1, i2);
        org.testng.Assert.assertNotEquals(i1, i2, "Failure message to keep");
    }

    public void shouldRefactorFailures() throws Exception {
        Assert.fail();
        Assert.fail("Failure message to keep");
        Assert.fail();
        Assert.fail("Failure message to keep");

        org.testng.Assert.fail();
        org.testng.Assert.fail("Failure message to keep");
        org.testng.Assert.fail();
        org.testng.Assert.fail("Failure message to keep");
    }

    public void shouldRemoveDeadChecks() throws Exception {
    }

    public void shouldRefactorNegatedConditions(boolean b) throws Exception {
        Assert.assertFalse(b);
        Assert.assertFalse(b, "Failure message to keep");
        Assert.assertTrue(b);
        Assert.assertTrue(b, "Failure message to keep");

        org.testng.Assert.assertFalse(b);
        org.testng.Assert.assertFalse(b, "Failure message to keep");
        org.testng.Assert.assertTrue(b);
        org.testng.Assert.assertTrue(b, "Failure message to keep");
    }

    public void shouldRefactorWithObjectReferences(Object o1, Object o2) throws Exception {
        Assert.assertSame(o1, o2);
        Assert.assertSame(o1, o2, "Failure message to keep");
        Assert.assertNotSame(o1, o2);
        Assert.assertNotSame(o1, o2, "Failure message to keep");
        Assert.assertSame(o1, o2);
        Assert.assertSame(o1, o2, "Failure message to keep");
        Assert.assertNotSame(o1, o2);
        Assert.assertNotSame(o1, o2, "Failure message to keep");

        org.testng.Assert.assertSame(o1, o2);
        org.testng.Assert.assertSame(o1, o2, "Failure message to keep");
        org.testng.Assert.assertNotSame(o1, o2);
        org.testng.Assert.assertNotSame(o1, o2, "Failure message to keep");
        org.testng.Assert.assertSame(o1, o2);
        org.testng.Assert.assertSame(o1, o2, "Failure message to keep");
        org.testng.Assert.assertNotSame(o1, o2);
        org.testng.Assert.assertNotSame(o1, o2, "Failure message to keep");
    }

    public void shouldRefactorWithObjects(Object o1, Object o2) throws Exception {
        Assert.assertEquals(o1, o2);
        Assert.assertEquals(o1, o2, "Failure message to keep");
        Assert.assertNotEquals(o1, o2);
        Assert.assertNotEquals(o1, o2, "Failure message to keep");
        Assert.assertEquals(o1, o2);
        Assert.assertEquals(o1, o2, "Failure message to keep");
        Assert.assertNotEquals(o1, o2);
        Assert.assertNotEquals(o1, o2, "Failure message to keep");

        org.testng.Assert.assertEquals(o1, o2);
        org.testng.Assert.assertEquals(o1, o2, "Failure message to keep");
        org.testng.Assert.assertNotEquals(o1, o2);
        org.testng.Assert.assertNotEquals(o1, o2, "Failure message to keep");
        org.testng.Assert.assertEquals(o1, o2);
        org.testng.Assert.assertEquals(o1, o2, "Failure message to keep");
        org.testng.Assert.assertNotEquals(o1, o2);
        org.testng.Assert.assertNotEquals(o1, o2, "Failure message to keep");
    }

    public void shouldRefactorNullCheckFirstArg(Object o) throws Exception {
        Assert.assertNull(o);
        Assert.assertNull(o, "Failure message to keep");
        Assert.assertNotNull(o);
        Assert.assertNotNull(o, "Failure message to keep");
        Assert.assertNull(o);
        Assert.assertNull(o, "Failure message to keep");
        Assert.assertNotNull(o);
        Assert.assertNotNull(o, "Failure message to keep");

        org.testng.Assert.assertNull(o);
        org.testng.Assert.assertNull(o, "Failure message to keep");
        org.testng.Assert.assertNotNull(o);
        org.testng.Assert.assertNotNull(o, "Failure message to keep");
        org.testng.Assert.assertNull(o);
        org.testng.Assert.assertNull(o, "Failure message to keep");
        org.testng.Assert.assertNotNull(o);
        org.testng.Assert.assertNotNull(o, "Failure message to keep");
    }

    public void shouldRefactorNullCheckSecondArg(Object o) throws Exception {
        Assert.assertNull(o);
        Assert.assertNull(o, "Failure message to keep");
        Assert.assertNotNull(o);
        Assert.assertNotNull(o, "Failure message to keep");
        Assert.assertNull(o);
        Assert.assertNull(o, "Failure message to keep");
        Assert.assertNotNull(o);
        Assert.assertNotNull(o, "Failure message to keep");

        org.testng.Assert.assertNull(o);
        org.testng.Assert.assertNull(o, "Failure message to keep");
        org.testng.Assert.assertNotNull(o);
        org.testng.Assert.assertNotNull(o, "Failure message to keep");
        org.testng.Assert.assertNull(o);
        org.testng.Assert.assertNull(o, "Failure message to keep");
        org.testng.Assert.assertNotNull(o);
        org.testng.Assert.assertNotNull(o, "Failure message to keep");
    }

    public void shouldRefactorNullCheckFirstArgWithEquals(Object o) throws Exception {
        Assert.assertNull(o);
        Assert.assertNull(o, "Failure message to keep");
        Assert.assertNotNull(o);
        Assert.assertNotNull(o, "Failure message to keep");

        org.testng.Assert.assertNull(o);
        org.testng.Assert.assertNull(o, "Failure message to keep");
        org.testng.Assert.assertNotNull(o);
        org.testng.Assert.assertNotNull(o, "Failure message to keep");
    }

    public void shouldRefactorNullCheckSecondArgWithEquals(Object o) throws Exception {
        Assert.assertNull(o);
        Assert.assertNull(o, "Failure message to keep");
        Assert.assertNotNull(o);
        Assert.assertNotNull(o, "Failure message to keep");

        org.testng.Assert.assertNull(o);
        org.testng.Assert.assertNull(o, "Failure message to keep");
        org.testng.Assert.assertNotNull(o);
        org.testng.Assert.assertNotNull(o, "Failure message to keep");
    }

    public void shouldMoveLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(o, 42);
        Assert.assertEquals(o, 42, "Failure message to keep");
        Assert.assertNotEquals(o, 42);
        Assert.assertNotEquals(o, 42, "Failure message to keep");

        org.testng.Assert.assertEquals(o, 42);
        org.testng.Assert.assertEquals(o, 42, "Failure message to keep");
        org.testng.Assert.assertNotEquals(o, 42);
        org.testng.Assert.assertNotEquals(o, 42, "Failure message to keep");
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(o, 42);
        Assert.assertEquals(o, 42, "Failure message to keep");
        Assert.assertNotEquals(o, 42);
        Assert.assertNotEquals(o, 42, "Failure message to keep");

        assertEquals(o, 42);
        assertEquals(o, 42, "Failure message to keep");
        assertNotEquals(o, 42);
        assertNotEquals(o, 42, "Failure message to keep");
    }

    public void shouldMoveConstantAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(o, FOURTYTWO);
        Assert.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assert.assertNotEquals(o, FOURTYTWO);
        Assert.assertNotEquals(o, FOURTYTWO, "Failure message to keep");

        org.testng.Assert.assertEquals(o, FOURTYTWO);
        org.testng.Assert.assertEquals(o, FOURTYTWO, "Failure message to keep");
        org.testng.Assert.assertNotEquals(o, FOURTYTWO);
        org.testng.Assert.assertNotEquals(o, FOURTYTWO, "Failure message to keep");
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(o, FOURTYTWO);
        Assert.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assert.assertNotEquals(o, FOURTYTWO);
        Assert.assertNotEquals(o, FOURTYTWO, "Failure message to keep");

        assertEquals(o, FOURTYTWO);
        assertEquals(o, FOURTYTWO, "Failure message to keep");
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO, "Failure message to keep");
    }

    public void shouldMoveExpectedVariableAsExpectedArgWithEquals(Object o, int expected) throws Exception {
        Assert.assertEquals(o, expected);
        Assert.assertEquals(o, expected, "Failure message to keep");
        Assert.assertNotEquals(o, expected);
        Assert.assertNotEquals(o, expected, "Failure message to keep");

        org.testng.Assert.assertEquals(o, expected);
        org.testng.Assert.assertEquals(o, expected, "Failure message to keep");
        org.testng.Assert.assertNotEquals(o, expected);
        org.testng.Assert.assertNotEquals(o, expected, "Failure message to keep");

        // tests that this works according to levenshtein distance
        int expceted = 0;
        org.testng.Assert.assertEquals(o, expceted);
    }

    public void shouldRefactorIfPrimitiveThenFail(int i1, int i2) throws Exception {
        Assert.assertNotEquals(i1, i2);
        Assert.assertNotEquals(i1, i2, "Failure message to keep");
        Assert.assertEquals(i1, i2);
        Assert.assertEquals(i1, i2, "Failure message to keep");

        org.testng.Assert.assertNotEquals(i1, i2);
        org.testng.Assert.assertNotEquals(i1, i2, "Failure message to keep");
        org.testng.Assert.assertEquals(i1, i2);
        org.testng.Assert.assertEquals(i1, i2, "Failure message to keep");
    }

    public void shouldRefactorIfSameObjectThenFail(Object o1, Object o2) throws Exception {
        Assert.assertNotSame(o1, o2);
        Assert.assertNotSame(o1, o2, "Failure message to keep");
        Assert.assertSame(o1, o2);
        Assert.assertSame(o1, o2, "Failure message to keep");

        org.testng.Assert.assertNotSame(o1, o2);
        org.testng.Assert.assertNotSame(o1, o2, "Failure message to keep");
        org.testng.Assert.assertSame(o1, o2);
        org.testng.Assert.assertSame(o1, o2, "Failure message to keep");
    }

    public void shouldRefactorIfNullThenFail(Object o1) throws Exception {
        Assert.assertNotNull(o1);
        Assert.assertNotNull(o1, "Failure message to keep");
        Assert.assertNull(o1);
        Assert.assertNull(o1, "Failure message to keep");
        Assert.assertNotNull(o1);
        Assert.assertNotNull(o1, "Failure message to keep");
        Assert.assertNull(o1);
        Assert.assertNull(o1, "Failure message to keep");

        org.testng.Assert.assertNotNull(o1);
        org.testng.Assert.assertNotNull(o1, "Failure message to keep");
        org.testng.Assert.assertNull(o1);
        org.testng.Assert.assertNull(o1, "Failure message to keep");
        org.testng.Assert.assertNotNull(o1);
        org.testng.Assert.assertNotNull(o1, "Failure message to keep");
        org.testng.Assert.assertNull(o1);
        org.testng.Assert.assertNull(o1, "Failure message to keep");
    }

    public void shouldRefactorIfObjectThenFail(Object o1, Object o2) throws Exception {
        Assert.assertNotEquals(o1, o2);
        Assert.assertNotEquals(o1, o2, "Failure message to keep");
        Assert.assertEquals(o1, o2);
        Assert.assertEquals(o1, o2, "Failure message to keep");

        org.testng.Assert.assertNotEquals(o1, o2);
        org.testng.Assert.assertNotEquals(o1, o2, "Failure message to keep");
        org.testng.Assert.assertEquals(o1, o2);
        org.testng.Assert.assertEquals(o1, o2, "Failure message to keep");
    }

    public void shouldRefactorIfLiteralThenFail(int i) throws Exception {
        Assert.assertNotEquals(i, 42);
        Assert.assertNotEquals(i, 42, "Failure message to keep");
        Assert.assertEquals(i, 42);
        Assert.assertEquals(i, 42, "Failure message to keep");
        Assert.assertNotEquals(i, 42);
        Assert.assertNotEquals(i, 42, "Failure message to keep");
        Assert.assertEquals(i, 42);
        Assert.assertEquals(i, 42, "Failure message to keep");

        org.testng.Assert.assertNotEquals(i, 42);
        org.testng.Assert.assertNotEquals(i, 42, "Failure message to keep");
        org.testng.Assert.assertEquals(i, 42);
        org.testng.Assert.assertEquals(i, 42, "Failure message to keep");
        org.testng.Assert.assertNotEquals(i, 42);
        org.testng.Assert.assertNotEquals(i, 42, "Failure message to keep");
        org.testng.Assert.assertEquals(i, 42);
        org.testng.Assert.assertEquals(i, 42, "Failure message to keep");
    }

    public void shouldRefactorIfConstantThenFail(int i) throws Exception {
        Assert.assertNotEquals(i, FOURTYTWO);
        Assert.assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        Assert.assertEquals(i, FOURTYTWO);
        Assert.assertEquals(i, FOURTYTWO, "Failure message to keep");
        Assert.assertNotEquals(i, FOURTYTWO);
        Assert.assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        Assert.assertEquals(i, FOURTYTWO);
        Assert.assertEquals(i, FOURTYTWO, "Failure message to keep");

        org.testng.Assert.assertNotEquals(i, FOURTYTWO);
        org.testng.Assert.assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        org.testng.Assert.assertEquals(i, FOURTYTWO);
        org.testng.Assert.assertEquals(i, FOURTYTWO, "Failure message to keep");
        org.testng.Assert.assertNotEquals(i, FOURTYTWO);
        org.testng.Assert.assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        org.testng.Assert.assertEquals(i, FOURTYTWO);
        org.testng.Assert.assertEquals(i, FOURTYTWO, "Failure message to keep");
    }

    public void shouldRefactorIfExpectedThenFail(int i, int expected) throws Exception {
        Assert.assertNotEquals(i, expected);
        Assert.assertNotEquals(i, expected, "Failure message to keep");
        Assert.assertEquals(i, expected);
        Assert.assertEquals(i, expected, "Failure message to keep");
        Assert.assertNotEquals(i, expected);
        Assert.assertNotEquals(i, expected, "Failure message to keep");
        Assert.assertEquals(i, expected);
        Assert.assertEquals(i, expected, "Failure message to keep");

        org.testng.Assert.assertNotEquals(i, expected);
        org.testng.Assert.assertNotEquals(i, expected, "Failure message to keep");
        org.testng.Assert.assertEquals(i, expected);
        org.testng.Assert.assertEquals(i, expected, "Failure message to keep");
        org.testng.Assert.assertNotEquals(i, expected);
        org.testng.Assert.assertNotEquals(i, expected, "Failure message to keep");
        org.testng.Assert.assertEquals(i, expected);
        org.testng.Assert.assertEquals(i, expected, "Failure message to keep");
    }

    public void shouldRefactorIfNearlyExpectedThenFail(int i, int expectedI) throws Exception {
        Assert.assertNotEquals(i, expectedI);
        Assert.assertNotEquals(i, expectedI, "Failure message to keep");
        Assert.assertEquals(i, expectedI);
        Assert.assertEquals(i, expectedI, "Failure message to keep");
        Assert.assertNotEquals(i, expectedI);
        Assert.assertNotEquals(i, expectedI, "Failure message to keep");
        Assert.assertEquals(i, expectedI);
        Assert.assertEquals(i, expectedI, "Failure message to keep");

        org.testng.Assert.assertNotEquals(i, expectedI);
        org.testng.Assert.assertNotEquals(i, expectedI, "Failure message to keep");
        org.testng.Assert.assertEquals(i, expectedI);
        org.testng.Assert.assertEquals(i, expectedI, "Failure message to keep");
        org.testng.Assert.assertNotEquals(i, expectedI);
        org.testng.Assert.assertNotEquals(i, expectedI, "Failure message to keep");
        org.testng.Assert.assertEquals(i, expectedI);
        org.testng.Assert.assertEquals(i, expectedI, "Failure message to keep");
    }

    public void doNotRefactorBecauseOfElseStatement(int i1, int i2, Object o1) throws Exception {
        if (i1 == i2) {
            Assert.fail();
        } else {
            System.out.println("keep me!");
        }
        if (o1 == null) {
            Assert.fail();
        } else {
            System.out.println("keep me!");
        }
        Object o2 = i2;
        if (o1 == o2) {
            Assert.fail();
        } else {
            System.out.println("keep me!");
        }
        if (o1.equals(o2)) {
            Assert.fail();
        } else {
            System.out.println("keep me!");
        }
    }
}
