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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import static org.junit.Assert.*;

import org.junit.Assert;

public class JUnitAssertSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
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

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assert.assertEquals(d1, d2, .0);
        Assert.assertEquals("Failure message to keep", d1, d2, .0);
        Assert.assertEquals(d1, d2, .0);
        Assert.assertEquals("Failure message to keep", d1, d2, .0);

        assertEquals(d1, d2, .0);
        assertEquals("Failure message to keep", d1, d2, .0);
        assertEquals(d1, d2, .0);
        assertEquals("Failure message to keep", d1, d2, .0);
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assert.assertEquals(f1, f2, .0F);
        Assert.assertEquals("Failure message to keep", f1, f2, .0F);
        Assert.assertEquals(f1, f2, .0F);
        Assert.assertEquals("Failure message to keep", f1, f2, .0F);

        assertEquals(f1, f2, .0F);
        assertEquals("Failure message to keep", f1, f2, .0F);
        assertEquals(f1, f2, .0F);
        assertEquals("Failure message to keep", f1, f2, .0F);
    }

    public void refactorFailures() {
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

    public void removeDeadChecks() {
    }

    public void replaceAssertByBlocks(boolean b) {
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }

        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
    }

    public void refactorNegatedConditions(boolean b) {
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

    public void refactorWithObjectReferences(Object o1, Object o2) {
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

    public void refactorWithObjects(Object o1, Object o2) {
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

    public void refactorNullCheckFirstArg(Object o) {
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

    public void refactorNullCheckSecondArg(Object o) {
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

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);
        Assert.assertNotNull(o);
        Assert.assertNotNull("Failure message to keep", o);

        assertNull(o);
        assertNull("Failure message to keep", o);
        assertNotNull(o);
        assertNotNull("Failure message to keep", o);
    }

    public void refactorNullCheckSecondArgWithEquals(Object o) {
        // Keep this comment
        Assert.assertNull(o);
        Assert.assertNull("Failure message to keep", o);
        Assert.assertNotNull(o);
        Assert.assertNotNull("Failure message to keep", o);

        assertNull(o);
        assertNull("Failure message to keep", o);
        assertNotNull(o);
        assertNotNull("Failure message to keep", o);
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(42, o);
        Assert.assertEquals("Failure message to keep", 42, o);
        Assert.assertNotEquals(42, o);
        Assert.assertNotEquals("Failure message to keep", 42, o);

        assertEquals(42, o);
        assertEquals("Failure message to keep", 42, o);
        assertNotEquals(42, o);
        assertNotEquals("Failure message to keep", 42, o);
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assert.assertEquals(42L, l);
        Assert.assertEquals("Failure message to keep", 42L, l);
        Assert.assertNotEquals(42L, l);
        Assert.assertNotEquals("Failure message to keep", 42L, l);

        assertEquals(42L, l);
        assertEquals("Failure message to keep", 42L, l);
        assertNotEquals(42L, l);
        assertNotEquals("Failure message to keep", 42L, l);
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assert.assertEquals(42, i);
        Assert.assertEquals("Failure message to keep", 42, i);
        Assert.assertNotEquals(42, i);
        Assert.assertNotEquals("Failure message to keep", 42, i);

        assertEquals(42, i);
        assertEquals("Failure message to keep", 42, i);
        assertNotEquals(42, i);
        assertNotEquals("Failure message to keep", 42, i);
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assert.assertEquals('a', c);
        Assert.assertEquals("Failure message to keep", 'a', c);
        Assert.assertNotEquals('a', c);
        Assert.assertNotEquals("Failure message to keep", 'a', c);

        assertEquals('a', c);
        assertEquals("Failure message to keep", 'a', c);
        assertNotEquals('a', c);
        assertNotEquals("Failure message to keep", 'a', c);
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
        Assert.assertEquals(FOURTYTWO, o);
        Assert.assertEquals("Failure message to keep", FOURTYTWO, o);
        Assert.assertNotEquals(FOURTYTWO, o);
        Assert.assertNotEquals("Failure message to keep", FOURTYTWO, o);

        assertEquals(FOURTYTWO, o);
        assertEquals("Failure message to keep", FOURTYTWO, o);
        assertNotEquals(FOURTYTWO, o);
        assertNotEquals("Failure message to keep", FOURTYTWO, o);
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
        Assert.assertEquals(expected, o);
        Assert.assertEquals("Failure message to keep", expected, o);
        Assert.assertNotEquals(expected, o);
        Assert.assertNotEquals("Failure message to keep", expected, o);

        assertEquals(expected, o);
        assertEquals("Failure message to keep", expected, o);
        assertNotEquals(expected, o);
        assertNotEquals("Failure message to keep", expected, o);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, o);
        assertNotEquals(expceted, o);
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
        Assert.assertEquals(expected, l);
        Assert.assertEquals("Failure message to keep", expected, l);
        Assert.assertNotEquals(expected, l);
        Assert.assertNotEquals("Failure message to keep", expected, l);

        assertEquals(expected, l);
        assertEquals("Failure message to keep", expected, l);
        assertNotEquals(expected, l);
        assertNotEquals("Failure message to keep", expected, l);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, l);
        assertNotEquals(expceted, l);
    }

    public void moveExpectedDoubleAsExpectedArgWithEquals(double d, double expected) {
        // Keep this comment
        Assert.assertEquals(expected, d, .0);
        Assert.assertEquals("Failure message to keep", expected, d, .0);
        Assert.assertNotEquals(expected, d, .0);
        Assert.assertNotEquals("Failure message to keep", expected, d, .0);

        assertEquals(expected, d, .0);
        assertEquals("Failure message to keep", expected, d, .0);
        assertNotEquals(expected, d, .0);
        assertNotEquals("Failure message to keep", expected, d, .0);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, d);
        assertNotEquals(expceted, d);
    }

    public void refactorIfOnBoolean(boolean b) {
        // Keep this comment
        Assert.assertFalse(b);
        Assert.assertFalse("Failure message to keep", b);

        assertTrue(b);
        assertTrue("Failure message to keep", b);
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
        // Keep this comment
        Assert.assertEquals(i1, i2);
        Assert.assertEquals("Failure message to keep", i1, i2);

        assertEquals(i1, i2);
        assertEquals("Failure message to keep", i1, i2);
    }

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
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

    public void refactorIfNullThenFail(Object o1) {
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

    public void refactorIfObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        Assert.assertEquals(o1, o2);
        Assert.assertEquals("Failure message to keep", o1, o2);

        assertEquals(o1, o2);
        assertEquals("Failure message to keep", o1, o2);
    }

    public void refactorIfLiteralThenFail(int i) {
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

    public void refactorIfConstantThenFail(int i) {
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

    public void refactorIfExpectedThenFail(int i, int expected) {
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

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
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
