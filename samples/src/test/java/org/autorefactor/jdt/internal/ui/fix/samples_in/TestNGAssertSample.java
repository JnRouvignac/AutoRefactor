/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - include more cases
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

import static org.testng.Assert.*;

import org.testng.Assert;

public class TestNGAssertSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
        // Keep this comment
        Assert.assertTrue(i1 == i2);
        Assert.assertTrue(i1 == i2, "Failure message to keep");
        Assert.assertTrue(i1 != i2);
        Assert.assertTrue(i1 != i2, "Failure message to keep");
        Assert.assertFalse(i1 != i2);
        Assert.assertFalse(i1 != i2, "Failure message to keep");
        Assert.assertFalse(i1 == i2);
        Assert.assertFalse(i1 == i2, "Failure message to keep");

        assertTrue(i1 == i2);
        assertTrue(i1 == i2, "Failure message to keep");
        assertTrue(i1 != i2);
        assertTrue(i1 != i2, "Failure message to keep");
        assertFalse(i1 != i2);
        assertFalse(i1 != i2, "Failure message to keep");
        assertFalse(i1 == i2);
        assertFalse(i1 == i2, "Failure message to keep");
    }

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assert.assertTrue(d1 == d2);
        Assert.assertTrue(d1 == d2, "Failure message to keep");
        Assert.assertTrue(d1 != d2);
        Assert.assertTrue(d1 != d2, "Failure message to keep");
        Assert.assertFalse(d1 != d2);
        Assert.assertFalse(d1 != d2, "Failure message to keep");
        Assert.assertFalse(d1 == d2);
        Assert.assertFalse(d1 == d2, "Failure message to keep");

        assertTrue(d1 == d2);
        assertTrue(d1 == d2, "Failure message to keep");
        assertTrue(d1 != d2);
        assertTrue(d1 != d2, "Failure message to keep");
        assertFalse(d1 != d2);
        assertFalse(d1 != d2, "Failure message to keep");
        assertFalse(d1 == d2);
        assertFalse(d1 == d2, "Failure message to keep");
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assert.assertTrue(f1 == f2);
        Assert.assertTrue(f1 == f2, "Failure message to keep");
        Assert.assertTrue(f1 != f2);
        Assert.assertTrue(f1 != f2, "Failure message to keep");
        Assert.assertFalse(f1 != f2);
        Assert.assertFalse(f1 != f2, "Failure message to keep");
        Assert.assertFalse(f1 == f2);
        Assert.assertFalse(f1 == f2, "Failure message to keep");

        assertTrue(f1 == f2);
        assertTrue(f1 == f2, "Failure message to keep");
        assertTrue(f1 != f2);
        assertTrue(f1 != f2, "Failure message to keep");
        assertFalse(f1 != f2);
        assertFalse(f1 != f2, "Failure message to keep");
        assertFalse(f1 == f2);
        assertFalse(f1 == f2, "Failure message to keep");
    }

    public void refactorFailures() {
        // Keep this comment
        Assert.assertTrue(false);
        Assert.assertTrue(false, "Failure message to keep");
        Assert.assertFalse(true);
        Assert.assertFalse(true, "Failure message to keep");

        assertTrue(false);
        assertTrue(false, "Failure message to keep");
        assertFalse(true);
        assertFalse(true, "Failure message to keep");
    }

    public void removeDeadChecks() {
        Assert.assertTrue(true);
        Assert.assertTrue(true, "Failure message to keep");
        Assert.assertFalse(false);
        Assert.assertFalse(false, "Failure message to keep");

        assertTrue(true);
        assertTrue(true, "Failure message to keep");
        assertFalse(false);
        assertFalse(false, "Failure message to keep");
    }

    public void refactorNegatedConditions(boolean b) {
        // Keep this comment
        Assert.assertTrue(!b);
        Assert.assertTrue(!b, "Failure message to keep");
        Assert.assertFalse(!b);
        Assert.assertFalse(!b, "Failure message to keep");

        assertTrue(!b);
        assertTrue(!b, "Failure message to keep");
        assertFalse(!b);
        assertFalse(!b, "Failure message to keep");
    }

    public void refactorWithObjectReferences(Object o1, Object o2) {
        // Keep this comment
        Assert.assertTrue(o1 == o2);
        Assert.assertTrue(o1 == o2, "Failure message to keep");
        Assert.assertTrue(o1 != o2);
        Assert.assertTrue(o1 != o2, "Failure message to keep");
        Assert.assertFalse(o1 != o2);
        Assert.assertFalse(o1 != o2, "Failure message to keep");
        Assert.assertFalse(o1 == o2);
        Assert.assertFalse(o1 == o2, "Failure message to keep");

        assertTrue(o1 == o2);
        assertTrue(o1 == o2, "Failure message to keep");
        assertTrue(o1 != o2);
        assertTrue(o1 != o2, "Failure message to keep");
        assertFalse(o1 != o2);
        assertFalse(o1 != o2, "Failure message to keep");
        assertFalse(o1 == o2);
        assertFalse(o1 == o2, "Failure message to keep");
    }

    public void refactorWithObjects(Object o1, Object o2) {
        // Keep this comment
        Assert.assertTrue(o1.equals(o2));
        Assert.assertTrue(o1.equals(o2), "Failure message to keep");
        Assert.assertTrue(!(o1.equals(o2)));
        Assert.assertTrue(!(o1.equals(o2)), "Failure message to keep");
        Assert.assertFalse(!(o1.equals(o2)));
        Assert.assertFalse(!(o1.equals(o2)), "Failure message to keep");
        Assert.assertFalse(o1.equals(o2));
        Assert.assertFalse(o1.equals(o2), "Failure message to keep");

        assertTrue(o1.equals(o2));
        assertTrue(o1.equals(o2), "Failure message to keep");
        assertTrue(!(o1.equals(o2)));
        assertTrue(!(o1.equals(o2)), "Failure message to keep");
        assertFalse(!(o1.equals(o2)));
        assertFalse(!(o1.equals(o2)), "Failure message to keep");
        assertFalse(o1.equals(o2));
        assertFalse(o1.equals(o2), "Failure message to keep");
    }

    public void refactorNullCheckFirstArg(Object o) {
        // Keep this comment
        Assert.assertTrue(null == o);
        Assert.assertTrue(null == o, "Failure message to keep");
        Assert.assertTrue(null != o);
        Assert.assertTrue(null != o, "Failure message to keep");
        Assert.assertFalse(null != o);
        Assert.assertFalse(null != o, "Failure message to keep");
        Assert.assertFalse(null == o);
        Assert.assertFalse(null == o, "Failure message to keep");

        assertTrue(null == o);
        assertTrue(null == o, "Failure message to keep");
        assertTrue(null != o);
        assertTrue(null != o, "Failure message to keep");
        assertFalse(null != o);
        assertFalse(null != o, "Failure message to keep");
        assertFalse(null == o);
        assertFalse(null == o, "Failure message to keep");
    }

    public void refactorNullCheckSecondArg(Object o) {
        // Keep this comment
        Assert.assertTrue(o == null);
        Assert.assertTrue(o == null, "Failure message to keep");
        Assert.assertTrue(o != null);
        Assert.assertTrue(o != null, "Failure message to keep");
        Assert.assertFalse(o != null);
        Assert.assertFalse(o != null, "Failure message to keep");
        Assert.assertFalse(o == null);
        Assert.assertFalse(o == null, "Failure message to keep");

        assertTrue(o == null);
        assertTrue(o == null, "Failure message to keep");
        assertTrue(o != null);
        assertTrue(o != null, "Failure message to keep");
        assertFalse(o != null);
        assertFalse(o != null, "Failure message to keep");
        assertFalse(o == null);
        assertFalse(o == null, "Failure message to keep");
    }

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(null, o);
        Assert.assertEquals(null, o, "Failure message to keep");
        Assert.assertNotEquals(null, o);
        Assert.assertNotEquals(null, o, "Failure message to keep");

        assertEquals(null, o);
        assertEquals(null, o, "Failure message to keep");
        assertNotEquals(null, o);
        assertNotEquals(null, o, "Failure message to keep");
    }

    public void refactorNullCheckSecondArgWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(o, null);
        Assert.assertEquals(o, null, "Failure message to keep");
        Assert.assertNotEquals(o, null);
        Assert.assertNotEquals(o, null, "Failure message to keep");

        assertEquals(o, null);
        assertEquals(o, null, "Failure message to keep");
        assertNotEquals(o, null);
        assertNotEquals(o, null, "Failure message to keep");
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(42, o);
        Assert.assertEquals(42, o, "Failure message to keep");
        Assert.assertNotEquals(42, o);
        Assert.assertNotEquals(42, o, "Failure message to keep");

        assertEquals(42, o);
        assertEquals(42, o, "Failure message to keep");
        assertNotEquals(42, o);
        assertNotEquals(42, o, "Failure message to keep");
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assert.assertEquals(42L, l);
        Assert.assertEquals(42L, l, "Failure message to keep");
        Assert.assertNotEquals(42L, l);
        Assert.assertNotEquals(42L, l, "Failure message to keep");

        assertEquals(42L, l);
        assertEquals(42L, l, "Failure message to keep");
        assertNotEquals(42L, l);
        assertNotEquals(42L, l, "Failure message to keep");
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assert.assertEquals(42, i);
        Assert.assertEquals(42, i, "Failure message to keep");
        Assert.assertNotEquals(42, i);
        Assert.assertNotEquals(42, i, "Failure message to keep");

        assertEquals(42, i);
        assertEquals(42, i, "Failure message to keep");
        assertNotEquals(42, i);
        assertNotEquals(42, i, "Failure message to keep");
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assert.assertEquals('a', c);
        Assert.assertEquals('a', c, "Failure message to keep");
        Assert.assertNotEquals('a', c);
        Assert.assertNotEquals('a', c, "Failure message to keep");

        assertEquals('a', c);
        assertEquals('a', c, "Failure message to keep");
        assertNotEquals('a', c);
        assertNotEquals('a', c, "Failure message to keep");
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assert.assertEquals(1 + 2 + 3, i);
        Assert.assertEquals(1 + 2 + 3, i, "Failure message to keep");
        Assert.assertNotEquals(1 + 2 + 3, i);
        Assert.assertNotEquals(1 + 2 + 3, i, "Failure message to keep");

        assertEquals(1 + 2 + 3, i);
        assertEquals(1 + 2 + 3, i, "Failure message to keep");
        assertNotEquals(1 + 2 + 3, i);
        assertNotEquals(1 + 2 + 3, i, "Failure message to keep");
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) {
        Assert.assertEquals(o, 42);
        Assert.assertEquals(o, 42, "Failure message to keep");
        Assert.assertNotEquals(o, 42);
        Assert.assertNotEquals(o, 42, "Failure message to keep");

        assertEquals(o, 42);
        assertEquals(o, 42, "Failure message to keep");
        assertNotEquals(o, 42);
        assertNotEquals(o, 42, "Failure message to keep");
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assert.assertEquals(FOURTYTWO, o);
        Assert.assertEquals(FOURTYTWO, o, "Failure message to keep");
        Assert.assertNotEquals(FOURTYTWO, o);
        Assert.assertNotEquals(FOURTYTWO, o, "Failure message to keep");

        assertEquals(FOURTYTWO, o);
        assertEquals(FOURTYTWO, o, "Failure message to keep");
        assertNotEquals(FOURTYTWO, o);
        assertNotEquals(FOURTYTWO, o, "Failure message to keep");
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) {
        Assert.assertEquals(o, FOURTYTWO);
        Assert.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assert.assertNotEquals(o, FOURTYTWO);
        Assert.assertNotEquals(o, FOURTYTWO, "Failure message to keep");

        assertEquals(o, FOURTYTWO);
        assertEquals(o, FOURTYTWO, "Failure message to keep");
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO, "Failure message to keep");
    }

    public void moveExpectedVariableAsExpectedArgWithEquals(Object o, int expected) {
        // Keep this comment
        Assert.assertEquals(expected, o);
        Assert.assertEquals(expected, o, "Failure message to keep");
        Assert.assertNotEquals(expected, o);
        Assert.assertNotEquals(expected, o, "Failure message to keep");

        assertEquals(expected, o);
        assertEquals(expected, o, "Failure message to keep");
        assertNotEquals(expected, o);
        assertNotEquals(expected, o, "Failure message to keep");

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, o);
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
        // Keep this comment
        if (i1 == i2) {
            Assert.fail();
        }
        if (i1 == i2) {
            Assert.fail("Failure message to keep");
        }
        if (i1 != i2) {
            Assert.fail();
        }
        if (i1 != i2) {
            Assert.fail("Failure message to keep");
        }

        if (i1 == i2) {
            fail();
        }
        if (i1 == i2) {
            fail("Failure message to keep");
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
        if (o1.equals(o2)) {
            Assert.fail();
        }
        if (o1.equals(o2)) {
            Assert.fail("Failure message to keep");
        }
        if (!o1.equals(o2)) {
            Assert.fail();
        }
        if (!o1.equals(o2)) {
            Assert.fail("Failure message to keep");
        }

        if (o1.equals(o2)) {
            fail();
        }
        if (o1.equals(o2)) {
            fail("Failure message to keep");
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
