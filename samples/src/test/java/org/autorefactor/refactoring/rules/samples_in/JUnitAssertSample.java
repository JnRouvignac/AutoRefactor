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
package org.autorefactor.refactoring.rules.samples_in;

import static org.junit.Assert.*;

import org.junit.Assert;

public class JUnitAssertSample {

    private static final int FOURTYTWO = 42;

    public void shouldRefactorWithPrimitives(int i1, int i2) throws Exception {
        // Keep this comment
        Assert.assertTrue(i1 == i2);
        Assert.assertTrue("Failure message to keep", i1 == i2);
        Assert.assertFalse(i1 != i2);
        Assert.assertFalse("Failure message to keep", i1 != i2);

        assertTrue(i1 == i2);
        assertTrue("Failure message to keep", i1 == i2);
        assertFalse(i1 != i2);
        assertFalse("Failure message to keep", i1 != i2);
    }

    public void shouldRefactorFailures() throws Exception {
        // Keep this comment
        Assert.assertTrue(false);
        Assert.assertTrue("Failure message to keep", false);
        Assert.assertFalse(true);
        Assert.assertFalse("Failure message to keep", true);

        assertTrue(false);
        assertTrue("Failure message to keep", false);
        assertFalse(true);
        assertFalse("Failure message to keep", true);
    }

    public void shouldRemoveDeadChecks() throws Exception {
        Assert.assertTrue(true);
        Assert.assertTrue("Useless message", true);
        Assert.assertFalse(false);
        Assert.assertFalse("Useless message", false);

        assertTrue(true);
        assertTrue("Useless message", true);
        assertFalse(false);
        assertFalse("Useless message", false);
    }

    public void shouldRefactorNegatedConditions(boolean b) throws Exception {
        // Keep this comment
        Assert.assertTrue(!b);
        Assert.assertTrue("Failure message to keep", !b);
        Assert.assertFalse(!b);
        Assert.assertFalse("Failure message to keep", !b);

        assertTrue(!b);
        assertTrue("Failure message to keep", !b);
        assertFalse(!b);
        assertFalse("Failure message to keep", !b);
    }

    public void shouldRefactorWithObjectReferences(Object o1, Object o2) throws Exception {
        // Keep this comment
        Assert.assertTrue(o1 == o2);
        Assert.assertTrue("Failure message to keep", o1 == o2);
        Assert.assertTrue(o1 != o2);
        Assert.assertTrue("Failure message to keep", o1 != o2);
        Assert.assertFalse(o1 != o2);
        Assert.assertFalse("Failure message to keep", o1 != o2);
        Assert.assertFalse(o1 == o2);
        Assert.assertFalse("Failure message to keep", o1 == o2);

        assertTrue(o1 == o2);
        assertTrue("Failure message to keep", o1 == o2);
        assertTrue(o1 != o2);
        assertTrue("Failure message to keep", o1 != o2);
        assertFalse(o1 != o2);
        assertFalse("Failure message to keep", o1 != o2);
        assertFalse(o1 == o2);
        assertFalse("Failure message to keep", o1 == o2);
    }

    public void shouldRefactorWithObjects(Object o1, Object o2) throws Exception {
        // Keep this comment
        Assert.assertTrue(o1.equals(o2));
        Assert.assertTrue("Failure message to keep", o1.equals(o2));
        Assert.assertFalse(!(o1.equals(o2)));
        Assert.assertFalse("Failure message to keep", !(o1.equals(o2)));

        assertTrue(o1.equals(o2));
        assertTrue("Failure message to keep", o1.equals(o2));
        assertFalse(!(o1.equals(o2)));
        assertFalse("Failure message to keep", !(o1.equals(o2)));
    }

    public void shouldRefactorNullCheckFirstArg(Object o) throws Exception {
        // Keep this comment
        Assert.assertTrue(null == o);
        Assert.assertTrue("Failure message to keep", null == o);
        Assert.assertTrue(null != o);
        Assert.assertTrue("Failure message to keep", null != o);
        Assert.assertFalse(null != o);
        Assert.assertFalse("Failure message to keep", null != o);
        Assert.assertFalse(null == o);
        Assert.assertFalse("Failure message to keep", null == o);

        assertTrue(null == o);
        assertTrue("Failure message to keep", null == o);
        assertTrue(null != o);
        assertTrue("Failure message to keep", null != o);
        assertFalse(null != o);
        assertFalse("Failure message to keep", null != o);
        assertFalse(null == o);
        assertFalse("Failure message to keep", null == o);
    }

    public void shouldRefactorNullCheckSecondArg(Object o) throws Exception {
        // Keep this comment
        Assert.assertTrue(o == null);
        Assert.assertTrue("Failure message to keep", o == null);
        Assert.assertTrue(o != null);
        Assert.assertTrue("Failure message to keep", o != null);
        Assert.assertFalse(o != null);
        Assert.assertFalse("Failure message to keep", o != null);
        Assert.assertFalse(o == null);
        Assert.assertFalse("Failure message to keep", o == null);

        assertTrue(o == null);
        assertTrue("Failure message to keep", o == null);
        assertTrue(o != null);
        assertTrue("Failure message to keep", o != null);
        assertFalse(o != null);
        assertFalse("Failure message to keep", o != null);
        assertFalse(o == null);
        assertFalse("Failure message to keep", o == null);
    }

    public void shouldRefactorNullCheckFirstArgWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertEquals(null, o);
        Assert.assertEquals("Failure message to keep", null, o);

        assertEquals(null, o);
        assertEquals("Failure message to keep", null, o);
    }

    public void shouldRefactorNullCheckSecondArgWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertEquals(o, null);
        Assert.assertEquals("Failure message to keep", o, null);

        assertEquals(o, null);
        assertEquals("Failure message to keep", o, null);
    }

    public void shouldMoveLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertEquals(o, 42);
        Assert.assertEquals("Failure message to keep", o, 42);

        assertEquals(o, 42);
        assertEquals("Failure message to keep", o, 42);
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(42, o);
        Assert.assertEquals("Failure message to keep", 42, o);

        assertEquals(42, o);
        assertEquals("Failure message to keep", 42, o);
    }

    public void shouldMoveConstantAsExpectedArgInWithEquals(Object o) throws Exception {
        // Keep this comment
        Assert.assertEquals(o, FOURTYTWO);
        Assert.assertEquals("Failure message to keep", o, FOURTYTWO);

        assertEquals(o, FOURTYTWO);
        assertEquals("Failure message to keep", o, FOURTYTWO);
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) throws Exception {
        Assert.assertEquals(FOURTYTWO, o);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, o);

        assertEquals(FOURTYTWO, o);
        assertEquals("Failure message to keep", FOURTYTWO, o);
    }

    public void shouldMoveExpectedObjectAsExpectedArgWithEquals(Object o, int expected) throws Exception {
        // Keep this comment
        Assert.assertEquals(o, expected);
        Assert.assertEquals("Failure message to keep", o, expected);

        assertEquals(o, expected);
        assertEquals("Failure message to keep", o, expected);

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(o, expceted);
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
        Assert.assertEquals(l, expected);
        Assert.assertEquals("Failure message to keep", l, expected);

        assertEquals(l, expected);
        assertEquals("Failure message to keep", l, expected);

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(l, expceted);
    }

    public void shouldMoveExpectedDoubleAsExpectedArgWithEquals(double d, double expected) throws Exception {
        // Keep this comment
        Assert.assertEquals(d, expected);
        Assert.assertEquals("Failure message to keep", d, expected);

        assertEquals(d, expected);
        assertEquals("Failure message to keep", d, expected);

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(d, expceted);
    }

    public void shouldRefactorIfOnBoolean(boolean b) throws Exception {
        // Keep this comment
        if (b) {
            Assert.fail();
        }
        if (b) {
            Assert.fail("Failure message to keep");
        }

        if (!b) {
            fail();
        }
        if (!b) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfPrimitiveThenFail(int i1, int i2) throws Exception {
        // Keep this comment
        if (i1 != i2) {
            Assert.fail();
        }
        if (i1 != i2) {
            Assert.fail("Failure message to keep");
        }

        if (i1 != i2) {
            fail();
        }
        if (i1 != i2) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfSameObjectThenFail(Object o1, Object o2) throws Exception {
        // Keep this comment
        if (o1 == o2) {
            Assert.fail();
        }
        if (o1 == o2) {
            Assert.fail("Failure message to keep");
        }
        if (o1 != o2) {
            Assert.fail();
        }
        if (o1 != o2) {
            Assert.fail("Failure message to keep");
        }

        if (o1 == o2) {
            fail();
        }
        if (o1 == o2) {
            fail("Failure message to keep");
        }
        if (o1 != o2) {
            fail();
        }
        if (o1 != o2) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfNullThenFail(Object o1) throws Exception {
        // Keep this comment
        if (o1 == null) {
            Assert.fail();
        }
        if (o1 == null) {
            Assert.fail("Failure message to keep");
        }
        if (o1 != null) {
            Assert.fail();
        }
        if (o1 != null) {
            Assert.fail("Failure message to keep");
        }
        if (null == o1) {
            Assert.fail();
        }
        if (null == o1) {
            Assert.fail("Failure message to keep");
        }
        if (null != o1) {
            Assert.fail();
        }
        if (null != o1) {
            Assert.fail("Failure message to keep");
        }

        if (o1 == null) {
            fail();
        }
        if (o1 == null) {
            fail("Failure message to keep");
        }
        if (o1 != null) {
            fail();
        }
        if (o1 != null) {
            fail("Failure message to keep");
        }
        if (null == o1) {
            fail();
        }
        if (null == o1) {
            fail("Failure message to keep");
        }
        if (null != o1) {
            fail();
        }
        if (null != o1) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfObjectThenFail(Object o1, Object o2) throws Exception {
        // Keep this comment
        if (!o1.equals(o2)) {
            Assert.fail();
        }
        if (!o1.equals(o2)) {
            Assert.fail("Failure message to keep");
        }

        if (!o1.equals(o2)) {
            fail();
        }
        if (!o1.equals(o2)) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfLiteralThenFail(int i) throws Exception {
        // Keep this comment
        if (i == 42) {
            Assert.fail();
        }
        if (i == 42) {
            Assert.fail("Failure message to keep");
        }
        if (i != 42) {
            Assert.fail();
        }
        if (i != 42) {
            Assert.fail("Failure message to keep");
        }
        if (42 == i) {
            Assert.fail();
        }
        if (42 == i) {
            Assert.fail("Failure message to keep");
        }
        if (42 != i) {
            Assert.fail();
        }
        if (42 != i) {
            Assert.fail("Failure message to keep");
        }

        if (i == 42) {
            fail();
        }
        if (i == 42) {
            fail("Failure message to keep");
        }
        if (i != 42) {
            fail();
        }
        if (i != 42) {
            fail("Failure message to keep");
        }
        if (42 == i) {
            fail();
        }
        if (42 == i) {
            fail("Failure message to keep");
        }
        if (42 != i) {
            fail();
        }
        if (42 != i) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfConstantThenFail(int i) throws Exception {
        // Keep this comment
        if (i == FOURTYTWO) {
            Assert.fail();
        }
        if (i == FOURTYTWO) {
            Assert.fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            Assert.fail();
        }
        if (i != FOURTYTWO) {
            Assert.fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            Assert.fail();
        }
        if (FOURTYTWO == i) {
            Assert.fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            Assert.fail();
        }
        if (FOURTYTWO != i) {
            Assert.fail("Failure message to keep");
        }

        if (i == FOURTYTWO) {
            fail();
        }
        if (i == FOURTYTWO) {
            fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            fail();
        }
        if (i != FOURTYTWO) {
            fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            fail();
        }
        if (FOURTYTWO == i) {
            fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            fail();
        }
        if (FOURTYTWO != i) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfExpectedThenFail(int i, int expected) throws Exception {
        // Keep this comment
        if (i == expected) {
            Assert.fail();
        }
        if (i == expected) {
            Assert.fail("Failure message to keep");
        }
        if (i != expected) {
            Assert.fail();
        }
        if (i != expected) {
            Assert.fail("Failure message to keep");
        }
        if (expected == i) {
            Assert.fail();
        }
        if (expected == i) {
            Assert.fail("Failure message to keep");
        }
        if (expected != i) {
            Assert.fail();
        }
        if (expected != i) {
            Assert.fail("Failure message to keep");
        }

        if (i == expected) {
            fail();
        }
        if (i == expected) {
            fail("Failure message to keep");
        }
        if (i != expected) {
            fail();
        }
        if (i != expected) {
            fail("Failure message to keep");
        }
        if (expected == i) {
            fail();
        }
        if (expected == i) {
            fail("Failure message to keep");
        }
        if (expected != i) {
            fail();
        }
        if (expected != i) {
            fail("Failure message to keep");
        }
    }

    public void shouldRefactorIfNearlyExpectedThenFail(int i, int expectedI) throws Exception {
        // Keep this comment
        if (i == expectedI) {
            Assert.fail();
        }
        if (i == expectedI) {
            Assert.fail("Failure message to keep");
        }
        if (i != expectedI) {
            Assert.fail();
        }
        if (i != expectedI) {
            Assert.fail("Failure message to keep");
        }
        if (expectedI == i) {
            Assert.fail();
        }
        if (expectedI == i) {
            Assert.fail("Failure message to keep");
        }
        if (expectedI != i) {
            Assert.fail();
        }
        if (expectedI != i) {
            Assert.fail("Failure message to keep");
        }

        if (i == expectedI) {
            fail();
        }
        if (i == expectedI) {
            fail("Failure message to keep");
        }
        if (i != expectedI) {
            fail();
        }
        if (i != expectedI) {
            fail("Failure message to keep");
        }
        if (expectedI == i) {
            fail();
        }
        if (expectedI == i) {
            fail("Failure message to keep");
        }
        if (expectedI != i) {
            fail();
        }
        if (expectedI != i) {
            fail("Failure message to keep");
        }
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
