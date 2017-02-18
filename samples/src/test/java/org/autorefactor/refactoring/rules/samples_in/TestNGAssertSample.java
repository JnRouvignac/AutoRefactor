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
package org.autorefactor.refactoring.rules.samples_in;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.fail;
import static org.testng.Assert.*;

import org.testng.Assert;

public class TestNGAssertSample {

    private static final int FOURTYTWO = 42;

    public void shouldRefactorWithPrimitives(int i1, int i2) throws Exception {
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

    public void shouldRefactorFailures() throws Exception {
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

    public void shouldRemoveDeadChecks() throws Exception {
        Assert.assertTrue(true);
        Assert.assertTrue(true, "Failure message to keep");
        Assert.assertFalse(false);
        Assert.assertFalse(false, "Failure message to keep");

        assertTrue(true);
        assertTrue(true, "Failure message to keep");
        assertFalse(false);
        assertFalse(false, "Failure message to keep");
    }

    public void shouldRefactorNegatedConditions(boolean b) throws Exception {
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

    public void shouldRefactorWithObjectReferences(Object o1, Object o2) throws Exception {
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

    public void shouldRefactorWithObjects(Object o1, Object o2) throws Exception {
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

    public void shouldRefactorNullCheckFirstArg(Object o) throws Exception {
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

    public void shouldRefactorNullCheckSecondArg(Object o) throws Exception {
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

    public void shouldRefactorNullCheckFirstArgWithEquals(Object o) throws Exception {
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

    public void shouldRefactorNullCheckSecondArgWithEquals(Object o) throws Exception {
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

    public void shouldMoveLiteralAsExpectedArgInWithEquals(Object o) throws Exception {
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
        // Keep this comment
        Assert.assertEquals(expected, o);
        Assert.assertEquals(expected, o, "Failure message to keep");
        Assert.assertNotEquals(expected, o);
        Assert.assertNotEquals(expected, o, "Failure message to keep");

        assertEquals(expected, o);
        assertEquals(expected, o, "Failure message to keep");
        assertNotEquals(expected, o);
        assertNotEquals(expected, o, "Failure message to keep");

        // tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, o);
    }

    public void shouldRefactorIfPrimitiveThenFail(int i1, int i2) throws Exception {
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
