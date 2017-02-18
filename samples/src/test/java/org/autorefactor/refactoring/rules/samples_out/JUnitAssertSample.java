/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.refactoring.rules.samples_out;

import static org.junit.Assert.*;

import org.junit.Assert;

public class JUnitAssertSample {

    private static final int FOURTYTWO = 42;

    public void shouldRefactorWithPrimitives(int i1, int i2) throws Exception {
        // Keep this comment
        Assert.assertEquals(i1, i2);
        Assert.assertEquals("Failure message to keep", i1, i2);
        Assert.assertEquals(i1, i2);
        Assert.assertEquals("Failure message to keep", i1, i2);

        assertEquals(i1, i2);
        assertEquals("Failure message to keep", i1, i2);
        assertEquals(i1, i2);
        assertEquals("Failure message to keep", i1, i2);
    }

    public void shouldRefactorFailures() throws Exception {
        // Keep this comment
        Assert.fail();
        Assert.fail("Failure message to keep");
        Assert.fail();
        Assert.fail("Failure message to keep");

        fail();
        fail("Failure message to keep");
        fail();
        fail("Failure message to keep");
    }

    public void shouldRemoveDeadChecks() throws Exception {
    }

    public void shouldRefactorNegatedConditions(boolean b) throws Exception {
        // Keep this comment
        Assert.assertFalse(b);
        Assert.assertFalse("Failure message to keep", b);
        Assert.assertTrue(b);
        Assert.assertTrue("Failure message to keep", b);

        assertFalse(b);
        assertFalse("Failure message to keep", b);
        assertTrue(b);
        assertTrue("Failure message to keep", b);
    }

    public void shouldRefactorWithObjectReferences(Object o1, Object o2) throws Exception {
        // Keep this comment
        Assert.assertSame(o1, o2);
        Assert.assertSame("Failure message to keep", o1, o2);
        Assert.assertNotSame(o1, o2);
        Assert.assertNotSame("Failure message to keep", o1, o2);
        Assert.assertSame(o1, o2);
        Assert.assertSame("Failure message to keep", o1, o2);
        Assert.assertNotSame(o1, o2);
        Assert.assertNotSame("Failure message to keep", o1, o2);

        assertSame(o1, o2);
        assertSame("Failure message to keep", o1, o2);
        assertNotSame(o1, o2);
        assertNotSame("Failure message to keep", o1, o2);
        assertSame(o1, o2);
        assertSame("Failure message to keep", o1, o2);
        assertNotSame(o1, o2);
        assertNotSame("Failure message to keep", o1, o2);
    }

    public void shouldRefactorWithObjects(Object o1, Object o2) throws Exception {
        // Keep this comment
        Assert.assertEquals(o1, o2);
        Assert.assertEquals("Failure message to keep", o1, o2);
        Assert.assertEquals(o1, o2);
        Assert.assertEquals("Failure message to keep", o1, o2);

        assertEquals(o1, o2);
        assertEquals("Failure message to keep", o1, o2);
        assertEquals(o1, o2);
        assertEquals("Failure message to keep", o1, o2);
    }

    public void shouldRefactorNullCheckFirstArg(Object o) throws Exception {
        // Keep this comment
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);
        Assert.assertNotNull(o);
        Assert.assertNotNull("Failure message to keep", o);
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);
        Assert.assertNotNull(o);
        Assert.assertNotNull("Failure message to keep", o);

        assertNull(o);
        assertNull("Failure message to keep", o);
        assertNotNull(o);
        assertNotNull("Failure message to keep", o);
        assertNull(o);
        assertNull("Failure message to keep", o);
        assertNotNull(o);
        assertNotNull("Failure message to keep", o);
    }

    public void shouldRefactorNullCheckSecondArg(Object o) throws Exception {
        // Keep this comment
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);
        Assert.assertNotNull(o);
        Assert.assertNotNull("Failure message to keep", o);
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);
        Assert.assertNotNull(o);
        Assert.assertNotNull("Failure message to keep", o);

        assertNull(o);
        assertNull("Failure message to keep", o);
        assertNotNull(o);
        assertNotNull("Failure message to keep", o);
        assertNull(o);
        assertNull("Failure message to keep", o);
        assertNotNull(o);
        assertNotNull("Failure message to keep", o);
    }

    public void shouldRefactorNullCheckFirstArgWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);

        assertNull(o);
        assertNull("Failure message to keep", o);
    }

    public void shouldRefactorNullCheckSecondArgWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);

        assertNull(o);
        assertNull("Failure message to keep", o);
    }

    public void shouldMoveLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertEquals(42, o);
        Assert.assertEquals("Failure message to keep", 42, o);

        assertEquals(42, o);
        assertEquals("Failure message to keep", 42, o);
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(42, o);
        Assert.assertEquals("Failure message to keep", 42, o);

        assertEquals(42, o);
        assertEquals("Failure message to keep", 42, o);
    }

    public void shouldMoveConstantAsExpectedArgInWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertEquals(FOURTYTWO, o);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, o);

        assertEquals(FOURTYTWO, o);
        assertEquals("Failure message to keep", FOURTYTWO, o);
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(FOURTYTWO, o);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, o);

        assertEquals(FOURTYTWO, o);
        assertEquals("Failure message to keep", FOURTYTWO, o);
    }

    public void shouldMoveExpectedObjectAsExpectedArgWithEquals(Object o, int expected) throws Exception {
        // Keep this comment
        Assert.assertEquals(expected, o);
        Assert.assertEquals("Failure message to keep", expected, o);

        assertEquals(expected, o);
        assertEquals("Failure message to keep", expected, o);

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, o);
    }

    public void doNotRefactorExpectedObjectAsExpectedArgWithEquals(Object o, int expected) throws Exception {
        Assert.assertEquals(expected, o);
        Assert.assertEquals("Failure message to keep", expected, o);

        assertEquals(expected, o);
        assertEquals("Failure message to keep", expected, o);

        int expceted = 0;
        assertEquals(expceted, o);
    }

    public void shouldMoveExpectedLongAsExpectedArgWithEquals(long l, long expected) throws Exception {
        // Keep this comment
        Assert.assertEquals(expected, l);
        Assert.assertEquals("Failure message to keep", expected, l);

        assertEquals(expected, l);
        assertEquals("Failure message to keep", expected, l);

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, l);
    }

    public void shouldMoveExpectedDoubleAsExpectedArgWithEquals(double d, double expected) throws Exception {
        // Keep this comment
        Assert.assertEquals(expected, d);
        Assert.assertEquals("Failure message to keep", expected, d);

        assertEquals(expected, d);
        assertEquals("Failure message to keep", expected, d);

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, d);
    }

    public void shouldRefactorIfOnBoolean(boolean b) throws Exception {
        // Keep this comment
        Assert.assertFalse(b);
        Assert.assertFalse("Failure message to keep", b);

        assertTrue(b);
        assertTrue("Failure message to keep", b);
    }

    public void shouldRefactorIfPrimitiveThenFail(int i1, int i2) throws Exception {
        // Keep this comment
        Assert.assertEquals(i1, i2);
        Assert.assertEquals("Failure message to keep", i1, i2);

        assertEquals(i1, i2);
        assertEquals("Failure message to keep", i1, i2);
    }

    public void shouldRefactorIfSameObjectThenFail(Object o1, Object o2) throws Exception {
        // Keep this comment
        Assert.assertNotSame(o1, o2);
        Assert.assertNotSame("Failure message to keep", o1, o2);
        Assert.assertSame(o1, o2);
        Assert.assertSame("Failure message to keep", o1, o2);

        assertNotSame(o1, o2);
        assertNotSame("Failure message to keep", o1, o2);
        assertSame(o1, o2);
        assertSame("Failure message to keep", o1, o2);
    }

    public void shouldRefactorIfNullThenFail(Object o1) throws Exception {
        // Keep this comment
        Assert.assertNotNull(o1);
        Assert.assertNotNull("Failure message to keep", o1);
        Assert.assertNull(o1);
        Assert.assertNull("Failure message to keep", o1);
        Assert.assertNotNull(o1);
        Assert.assertNotNull("Failure message to keep", o1);
        Assert.assertNull(o1);
        Assert.assertNull("Failure message to keep", o1);

        assertNotNull(o1);
        assertNotNull("Failure message to keep", o1);
        assertNull(o1);
        assertNull("Failure message to keep", o1);
        assertNotNull(o1);
        assertNotNull("Failure message to keep", o1);
        assertNull(o1);
        assertNull("Failure message to keep", o1);
    }

    public void shouldRefactorIfObjectThenFail(Object o1, Object o2) throws Exception {
        // Keep this comment
        Assert.assertEquals(o1, o2);
        Assert.assertEquals("Failure message to keep", o1, o2);

        assertEquals(o1, o2);
        assertEquals("Failure message to keep", o1, o2);
    }

    public void shouldRefactorIfLiteralThenFail(int i) throws Exception {
        // Keep this comment
        Assert.assertNotEquals(42, i);
        Assert.assertNotEquals("Failure message to keep", 42, i);
        Assert.assertEquals(42, i);
        Assert.assertEquals("Failure message to keep", 42, i);
        Assert.assertNotEquals(42, i);
        Assert.assertNotEquals("Failure message to keep", 42, i);
        Assert.assertEquals(42, i);
        Assert.assertEquals("Failure message to keep", 42, i);

        assertNotEquals(42, i);
        assertNotEquals("Failure message to keep", 42, i);
        assertEquals(42, i);
        assertEquals("Failure message to keep", 42, i);
        assertNotEquals(42, i);
        assertNotEquals("Failure message to keep", 42, i);
        assertEquals(42, i);
        assertEquals("Failure message to keep", 42, i);
    }

    public void shouldRefactorIfConstantThenFail(int i) throws Exception {
        // Keep this comment
        Assert.assertNotEquals(FOURTYTWO, i);
        Assert.assertNotEquals("Failure message to keep", FOURTYTWO, i);
        Assert.assertEquals(FOURTYTWO, i);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, i);
        Assert.assertNotEquals(FOURTYTWO, i);
        Assert.assertNotEquals("Failure message to keep", FOURTYTWO, i);
        Assert.assertEquals(FOURTYTWO, i);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, i);

        assertNotEquals(FOURTYTWO, i);
        assertNotEquals("Failure message to keep", FOURTYTWO, i);
        assertEquals(FOURTYTWO, i);
        assertEquals("Failure message to keep", FOURTYTWO, i);
        assertNotEquals(FOURTYTWO, i);
        assertNotEquals("Failure message to keep", FOURTYTWO, i);
        assertEquals(FOURTYTWO, i);
        assertEquals("Failure message to keep", FOURTYTWO, i);
    }

    public void shouldRefactorIfExpectedThenFail(int i, int expected) throws Exception {
        // Keep this comment
        Assert.assertNotEquals(expected, i);
        Assert.assertNotEquals("Failure message to keep", expected, i);
        Assert.assertEquals(expected, i);
        Assert.assertEquals("Failure message to keep", expected, i);
        Assert.assertNotEquals(expected, i);
        Assert.assertNotEquals("Failure message to keep", expected, i);
        Assert.assertEquals(expected, i);
        Assert.assertEquals("Failure message to keep", expected, i);

        assertNotEquals(expected, i);
        assertNotEquals("Failure message to keep", expected, i);
        assertEquals(expected, i);
        assertEquals("Failure message to keep", expected, i);
        assertNotEquals(expected, i);
        assertNotEquals("Failure message to keep", expected, i);
        assertEquals(expected, i);
        assertEquals("Failure message to keep", expected, i);
    }

    public void shouldRefactorIfNearlyExpectedThenFail(int i, int expectedI) throws Exception {
        // Keep this comment
        Assert.assertNotEquals(expectedI, i);
        Assert.assertNotEquals("Failure message to keep", expectedI, i);
        Assert.assertEquals(expectedI, i);
        Assert.assertEquals("Failure message to keep", expectedI, i);
        Assert.assertNotEquals(expectedI, i);
        Assert.assertNotEquals("Failure message to keep", expectedI, i);
        Assert.assertEquals(expectedI, i);
        Assert.assertEquals("Failure message to keep", expectedI, i);

        assertNotEquals(expectedI, i);
        assertNotEquals("Failure message to keep", expectedI, i);
        assertEquals(expectedI, i);
        assertEquals("Failure message to keep", expectedI, i);
        assertNotEquals(expectedI, i);
        assertNotEquals("Failure message to keep", expectedI, i);
        assertEquals(expectedI, i);
        assertEquals("Failure message to keep", expectedI, i);
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
