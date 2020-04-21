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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import static org.junit.Assert.*;

import org.junit.Assert;

public class JUnitAssertSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
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

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assert.assertTrue(d1 == d2);
        Assert.assertTrue("Failure message to keep", d1 == d2);
        Assert.assertFalse(d1 != d2);
        Assert.assertFalse("Failure message to keep", d1 != d2);

        assertTrue(d1 == d2);
        assertTrue("Failure message to keep", d1 == d2);
        assertFalse(d1 != d2);
        assertFalse("Failure message to keep", d1 != d2);
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assert.assertTrue(f1 == f2);
        Assert.assertTrue("Failure message to keep", f1 == f2);
        Assert.assertFalse(f1 != f2);
        Assert.assertFalse("Failure message to keep", f1 != f2);

        assertTrue(f1 == f2);
        assertTrue("Failure message to keep", f1 == f2);
        assertFalse(f1 != f2);
        assertFalse("Failure message to keep", f1 != f2);
    }

    public void refactorFailures() {
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

    public void removeDeadChecks() {
        Assert.assertTrue(true);
        Assert.assertTrue("Useless message", true);
        Assert.assertFalse(false);
        Assert.assertFalse("Useless message", false);

        assertTrue(true);
        assertTrue("Useless message", true);
        assertFalse(false);
        assertFalse("Useless message", false);
    }

    public void replaceAssertByBlocks(boolean b) {
        if (b)
            Assert.assertTrue(true);
        if (b)
            Assert.assertTrue("Useless message", true);
        if (b)
            Assert.assertFalse(false);
        if (b)
            Assert.assertFalse("Useless message", false);

        if (b)
            assertTrue(true);
        if (b)
            assertTrue("Useless message", true);
        if (b)
            assertFalse(false);
        if (b)
            assertFalse("Useless message", false);
    }

    public void removeElse(boolean b) {
        if (b)
            System.out.println("foo");
        else
            Assert.assertTrue(true);
    }

    public void refactorNegatedConditions(boolean b) {
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

    public void refactorWithObjectReferences(Object o1, Object o2) {
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

    public void refactorWithObjects(Object o1, Object o2) {
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

    public void refactorNullCheckFirstArg(Object o) {
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

    public void refactorNullCheckSecondArg(Object o) {
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

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(null, o);
        Assert.assertEquals("Failure message to keep", null, o);
        Assert.assertNotEquals(null, o);
        Assert.assertNotEquals("Failure message to keep", null, o);

        assertEquals(null, o);
        assertEquals("Failure message to keep", null, o);
        assertNotEquals(null, o);
        assertNotEquals("Failure message to keep", null, o);
    }

    public void refactorNullCheckSecondArgWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(o, null);
        Assert.assertEquals("Failure message to keep", o, null);
        Assert.assertNotEquals(o, null);
        Assert.assertNotEquals("Failure message to keep", o, null);

        assertEquals(o, null);
        assertEquals("Failure message to keep", o, null);
        assertNotEquals(o, null);
        assertNotEquals("Failure message to keep", o, null);
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(o, 42);
        Assert.assertEquals("Failure message to keep", o, 42);
        Assert.assertNotEquals(o, 42);
        Assert.assertNotEquals("Failure message to keep", o, 42);

        assertEquals(o, 42);
        assertEquals("Failure message to keep", o, 42);
        assertNotEquals(o, 42);
        assertNotEquals("Failure message to keep", o, 42);
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assert.assertEquals(l, 42L);
        Assert.assertEquals("Failure message to keep", l, 42L);
        Assert.assertNotEquals(l, 42L);
        Assert.assertNotEquals("Failure message to keep", l, 42L);

        assertEquals(l, 42L);
        assertEquals("Failure message to keep", l, 42L);
        assertNotEquals(l, 42L);
        assertNotEquals("Failure message to keep", l, 42L);
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assert.assertEquals(i, 42);
        Assert.assertEquals("Failure message to keep", i, 42);
        Assert.assertNotEquals(i, 42);
        Assert.assertNotEquals("Failure message to keep", i, 42);

        assertEquals(i, 42);
        assertEquals("Failure message to keep", i, 42);
        assertNotEquals(i, 42);
        assertNotEquals("Failure message to keep", i, 42);
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assert.assertEquals(c, 'a');
        Assert.assertEquals("Failure message to keep", c, 'a');
        Assert.assertNotEquals(c, 'a');
        Assert.assertNotEquals("Failure message to keep", c, 'a');

        assertEquals(c, 'a');
        assertEquals("Failure message to keep", c, 'a');
        assertNotEquals(c, 'a');
        assertNotEquals("Failure message to keep", c, 'a');
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assert.assertEquals(i, 1 + 2 + 3);
        Assert.assertEquals("Failure message to keep", i, 1 + 2 + 3);
        Assert.assertNotEquals(i, 1 + 2 + 3);
        Assert.assertNotEquals("Failure message to keep", i, 1 + 2 + 3);

        assertEquals(i, 1 + 2 + 3);
        assertEquals("Failure message to keep", i, 1 + 2 + 3);
        assertNotEquals(i, 1 + 2 + 3);
        assertNotEquals("Failure message to keep", i, 1 + 2 + 3);
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) {
        Assert.assertEquals(42, o);
        Assert.assertEquals("Failure message to keep", 42, o);
        Assert.assertNotEquals(42, o);
        Assert.assertNotEquals("Failure message to keep", 42, o);

        assertEquals(42, o);
        assertEquals("Failure message to keep", 42, o);
        assertNotEquals(42, o);
        assertNotEquals("Failure message to keep", 42, o);
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(o, FOURTYTWO);
        Assert.assertEquals("Failure message to keep", o, FOURTYTWO);
        Assert.assertNotEquals(o, FOURTYTWO);
        Assert.assertNotEquals("Failure message to keep", o, FOURTYTWO);

        assertEquals(o, FOURTYTWO);
        assertEquals("Failure message to keep", o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals("Failure message to keep", o, FOURTYTWO);
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) {
        Assert.assertEquals(FOURTYTWO, o);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, o);
        Assert.assertNotEquals(FOURTYTWO, o);
        Assert.assertNotEquals("Failure message to keep", FOURTYTWO, o);

        assertEquals(FOURTYTWO, o);
        assertEquals("Failure message to keep", FOURTYTWO, o);
        assertNotEquals(FOURTYTWO, o);
        assertNotEquals("Failure message to keep", FOURTYTWO, o);
    }

    public void moveExpectedObjectAsExpectedArgWithEquals(Object o, int expected) {
        // Keep this comment
        Assert.assertEquals(o, expected);
        Assert.assertEquals("Failure message to keep", o, expected);
        Assert.assertNotEquals(o, expected);
        Assert.assertNotEquals("Failure message to keep", o, expected);

        assertEquals(o, expected);
        assertEquals("Failure message to keep", o, expected);
        assertNotEquals(o, expected);
        assertNotEquals("Failure message to keep", o, expected);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(o, expceted);
        assertNotEquals(o, expceted);
    }

    public void doNotRefactorExpectedObjectAsExpectedArgWithEquals(Object o, int expected) {
        Assert.assertEquals(expected, o);
        Assert.assertEquals("Failure message to keep", expected, o);
        Assert.assertNotEquals(expected, o);
        Assert.assertNotEquals("Failure message to keep", expected, o);

        assertEquals(expected, o);
        assertEquals("Failure message to keep", expected, o);
        assertNotEquals(expected, o);
        assertNotEquals("Failure message to keep", expected, o);

        int expceted = 0;
        assertEquals(expceted, o);
        assertNotEquals(expceted, o);
    }

    public void moveExpectedLongAsExpectedArgWithEquals(long l, long expected) {
        // Keep this comment
        Assert.assertEquals(l, expected);
        Assert.assertEquals("Failure message to keep", l, expected);
        Assert.assertNotEquals(l, expected);
        Assert.assertNotEquals("Failure message to keep", l, expected);

        assertEquals(l, expected);
        assertEquals("Failure message to keep", l, expected);
        assertNotEquals(l, expected);
        assertNotEquals("Failure message to keep", l, expected);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(l, expceted);
        assertNotEquals(l, expceted);
    }

    public void moveExpectedDoubleAsExpectedArgWithEquals(double d, double expected) {
        // Keep this comment
        Assert.assertEquals(d, expected);
        Assert.assertEquals("Failure message to keep", d, expected);
        Assert.assertNotEquals(d, expected);
        Assert.assertNotEquals("Failure message to keep", d, expected);

        assertEquals(d, expected);
        assertEquals("Failure message to keep", d, expected);
        assertNotEquals(d, expected);
        assertNotEquals("Failure message to keep", d, expected);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(d, expceted);
        assertNotEquals(d, expceted);
    }

    public void refactorIfOnBoolean(boolean b) {
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

    public void refactorIfOnExpression(boolean b1, boolean b2) {
        // Keep this comment
        if (b1 && b2) {
            Assert.fail();
        }
        if (b1 || b2) {
            Assert.fail("Failure message to keep");
        }

        if (!b1 && !b2) {
            fail();
        }
        if (!b1 || !b2) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
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

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
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

    public void refactorIfNullThenFail(Object o1) {
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

    public void refactorIfObjectThenFail(Object o1, Object o2) {
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

    public void refactorIfLiteralThenFail(int i) {
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

    public void refactorIfConstantThenFail(int i) {
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

    public void refactorIfExpectedThenFail(int i, int expected) {
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

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
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

    public void doNotRefactorBecauseOfElseStatement(int i1, int i2, Object o1) {
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
