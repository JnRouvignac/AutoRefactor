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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Assertions;

public class JupiterAssertSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
        // Keep this comment
        Assertions.assertEquals(i1, i2);
        Assertions.assertEquals(i1, i2, "Failure message to keep");
        Assertions.assertNotEquals(i1, i2);
        Assertions.assertNotEquals(i1, i2, "Failure message to keep");
        Assertions.assertEquals(i1, i2);
        Assertions.assertEquals(i1, i2, "Failure message to keep");
        Assertions.assertNotEquals(i1, i2);
        Assertions.assertNotEquals(i1, i2, "Failure message to keep");

        assertEquals(i1, i2);
        assertEquals(i1, i2, "Failure message to keep");
        assertNotEquals(i1, i2);
        assertNotEquals(i1, i2, "Failure message to keep");
        assertEquals(i1, i2);
        assertEquals(i1, i2, "Failure message to keep");
        assertNotEquals(i1, i2);
        assertNotEquals(i1, i2, "Failure message to keep");
    }

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assertions.assertEquals(d1, d2, .0);
        Assertions.assertEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertNotEquals(d1, d2, .0);
        Assertions.assertNotEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertEquals(d1, d2, .0);
        Assertions.assertEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertNotEquals(d1, d2, .0);
        Assertions.assertNotEquals(d1, d2, .0, "Failure message to keep");

        assertEquals(d1, d2, .0);
        assertEquals(d1, d2, .0, "Failure message to keep");
        assertNotEquals(d1, d2, .0);
        assertNotEquals(d1, d2, .0, "Failure message to keep");
        assertEquals(d1, d2, .0);
        assertEquals(d1, d2, .0, "Failure message to keep");
        assertNotEquals(d1, d2, .0);
        assertNotEquals(d1, d2, .0, "Failure message to keep");
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assertions.assertEquals(f1, f2, .0F);
        Assertions.assertEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertNotEquals(f1, f2, .0F);
        Assertions.assertNotEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertEquals(f1, f2, .0F);
        Assertions.assertEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertNotEquals(f1, f2, .0F);
        Assertions.assertNotEquals(f1, f2, .0F, "Failure message to keep");

        assertEquals(f1, f2, .0F);
        assertEquals(f1, f2, .0F, "Failure message to keep");
        assertNotEquals(f1, f2, .0F);
        assertNotEquals(f1, f2, .0F, "Failure message to keep");
        assertEquals(f1, f2, .0F);
        assertEquals(f1, f2, .0F, "Failure message to keep");
        assertNotEquals(f1, f2, .0F);
        assertNotEquals(f1, f2, .0F, "Failure message to keep");
    }

    public void refactorFailures() {
        // Keep this comment
        Assertions.fail();
        Assertions.fail("Failure message to keep");
        Assertions.fail();
        Assertions.fail("Failure message to keep");

        fail();
        fail("Failure message to keep");
        fail();
        fail("Failure message to keep");
    }

    public void removeDeadChecks() {
    }

    public void refactorNegatedConditions(boolean b) {
        // Keep this comment
        Assertions.assertFalse(b);
        Assertions.assertFalse(b, "Failure message to keep");
        Assertions.assertTrue(b);
        Assertions.assertTrue(b, "Failure message to keep");

        assertFalse(b);
        assertFalse(b, "Failure message to keep");
        assertTrue(b);
        assertTrue(b, "Failure message to keep");
    }

    public void refactorWithObjectReferences(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertSame(o1, o2);
        Assertions.assertSame(o1, o2, "Failure message to keep");
        Assertions.assertNotSame(o1, o2);
        Assertions.assertNotSame(o1, o2, "Failure message to keep");
        Assertions.assertSame(o1, o2);
        Assertions.assertSame(o1, o2, "Failure message to keep");
        Assertions.assertNotSame(o1, o2);
        Assertions.assertNotSame(o1, o2, "Failure message to keep");

        assertSame(o1, o2);
        assertSame(o1, o2, "Failure message to keep");
        assertNotSame(o1, o2);
        assertNotSame(o1, o2, "Failure message to keep");
        assertSame(o1, o2);
        assertSame(o1, o2, "Failure message to keep");
        assertNotSame(o1, o2);
        assertNotSame(o1, o2, "Failure message to keep");
    }

    public void refactorWithObjects(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertEquals(o1, o2);
        Assertions.assertEquals(o1, o2, "Failure message to keep");
        Assertions.assertNotEquals(o1, o2);
        Assertions.assertNotEquals(o1, o2, "Failure message to keep");
        Assertions.assertEquals(o1, o2);
        Assertions.assertEquals(o1, o2, "Failure message to keep");
        Assertions.assertNotEquals(o1, o2);
        Assertions.assertNotEquals(o1, o2, "Failure message to keep");

        assertEquals(o1, o2);
        assertEquals(o1, o2, "Failure message to keep");
        assertNotEquals(o1, o2);
        assertNotEquals(o1, o2, "Failure message to keep");
        assertEquals(o1, o2);
        assertEquals(o1, o2, "Failure message to keep");
        assertNotEquals(o1, o2);
        assertNotEquals(o1, o2, "Failure message to keep");
    }

    public void refactorNullCheckFirstArg(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
    }

    public void refactorNullCheckSecondArg(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
    }

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
    }

    public void refactorNullCheckSecondArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(o, 42);
        Assertions.assertEquals(o, 42, "Failure message to keep");
        Assertions.assertNotEquals(o, 42);
        Assertions.assertNotEquals(o, 42, "Failure message to keep");

        assertEquals(o, 42);
        assertEquals(o, 42, "Failure message to keep");
        assertNotEquals(o, 42);
        assertNotEquals(o, 42, "Failure message to keep");
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assertions.assertEquals(l, 42L);
        Assertions.assertEquals(l, 42L, "Failure message to keep");
        Assertions.assertNotEquals(l, 42L);
        Assertions.assertNotEquals(l, 42L, "Failure message to keep");

        assertEquals(l, 42L);
        assertEquals(l, 42L, "Failure message to keep");
        assertNotEquals(l, 42L);
        assertNotEquals(l, 42L, "Failure message to keep");
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assertions.assertEquals(i, 42);
        Assertions.assertEquals(i, 42, "Failure message to keep");
        Assertions.assertNotEquals(i, 42);
        Assertions.assertNotEquals(i, 42, "Failure message to keep");

        assertEquals(i, 42);
        assertEquals(i, 42, "Failure message to keep");
        assertNotEquals(i, 42);
        assertNotEquals(i, 42, "Failure message to keep");
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assertions.assertEquals(c, 'a');
        Assertions.assertEquals(c, 'a', "Failure message to keep");
        Assertions.assertNotEquals(c, 'a');
        Assertions.assertNotEquals(c, 'a', "Failure message to keep");

        assertEquals(c, 'a');
        assertEquals(c, 'a', "Failure message to keep");
        assertNotEquals(c, 'a');
        assertNotEquals(c, 'a', "Failure message to keep");
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assertions.assertEquals(i, 1 + 2 + 3);
        Assertions.assertEquals(i, 1 + 2 + 3, "Failure message to keep");
        Assertions.assertNotEquals(i, 1 + 2 + 3);
        Assertions.assertNotEquals(i, 1 + 2 + 3, "Failure message to keep");

        assertEquals(i, 1 + 2 + 3);
        assertEquals(i, 1 + 2 + 3, "Failure message to keep");
        assertNotEquals(i, 1 + 2 + 3);
        assertNotEquals(i, 1 + 2 + 3, "Failure message to keep");
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) {
        Assertions.assertEquals(o, 42);
        Assertions.assertEquals(o, 42, "Failure message to keep");
        Assertions.assertNotEquals(o, 42);
        Assertions.assertNotEquals(o, 42, "Failure message to keep");

        assertEquals(o, 42);
        assertEquals(o, 42, "Failure message to keep");
        assertNotEquals(o, 42);
        assertNotEquals(o, 42, "Failure message to keep");
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(o, FOURTYTWO);
        Assertions.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assertions.assertNotEquals(o, FOURTYTWO);
        Assertions.assertNotEquals(o, FOURTYTWO, "Failure message to keep");

        assertEquals(o, FOURTYTWO);
        assertEquals(o, FOURTYTWO, "Failure message to keep");
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO, "Failure message to keep");
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) {
        Assertions.assertEquals(o, FOURTYTWO);
        Assertions.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assertions.assertNotEquals(o, FOURTYTWO);
        Assertions.assertNotEquals(o, FOURTYTWO, "Failure message to keep");

        assertEquals(o, FOURTYTWO);
        assertEquals(o, FOURTYTWO, "Failure message to keep");
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO, "Failure message to keep");
    }

    public void moveExpectedVariableAsExpectedArgWithEquals(Object o, int expected) {
        // Keep this comment
        Assertions.assertEquals(o, expected);
        Assertions.assertEquals(o, expected, "Failure message to keep");
        Assertions.assertNotEquals(o, expected);
        Assertions.assertNotEquals(o, expected, "Failure message to keep");

        assertEquals(o, expected);
        assertEquals(o, expected, "Failure message to keep");
        assertNotEquals(o, expected);
        assertNotEquals(o, expected, "Failure message to keep");

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(o, expceted);
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
        // Keep this comment
        Assertions.assertNotEquals(i1, i2);
        Assertions.assertNotEquals(i1, i2, "Failure message to keep");
        Assertions.assertEquals(i1, i2);
        Assertions.assertEquals(i1, i2, "Failure message to keep");

        assertNotEquals(i1, i2);
        assertNotEquals(i1, i2, "Failure message to keep");
        assertEquals(i1, i2);
        assertEquals(i1, i2, "Failure message to keep");
    }

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertNotSame(o1, o2);
        Assertions.assertNotSame(o1, o2, "Failure message to keep");
        Assertions.assertSame(o1, o2);
        Assertions.assertSame(o1, o2, "Failure message to keep");

        assertNotSame(o1, o2);
        assertNotSame(o1, o2, "Failure message to keep");
        assertSame(o1, o2);
        assertSame(o1, o2, "Failure message to keep");
    }

    public void refactorIfNullThenFail(Object o1) {
        // Keep this comment
        Assertions.assertNotNull(o1);
        Assertions.assertNotNull(o1, "Failure message to keep");
        Assertions.assertNull(o1);
        Assertions.assertNull(o1, "Failure message to keep");
        Assertions.assertNotNull(o1);
        Assertions.assertNotNull(o1, "Failure message to keep");
        Assertions.assertNull(o1);
        Assertions.assertNull(o1, "Failure message to keep");

        assertNotNull(o1);
        assertNotNull(o1, "Failure message to keep");
        assertNull(o1);
        assertNull(o1, "Failure message to keep");
        assertNotNull(o1);
        assertNotNull(o1, "Failure message to keep");
        assertNull(o1);
        assertNull(o1, "Failure message to keep");
    }

    public void refactorIfObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertNotEquals(o1, o2);
        Assertions.assertNotEquals(o1, o2, "Failure message to keep");
        Assertions.assertEquals(o1, o2);
        Assertions.assertEquals(o1, o2, "Failure message to keep");

        assertNotEquals(o1, o2);
        assertNotEquals(o1, o2, "Failure message to keep");
        assertEquals(o1, o2);
        assertEquals(o1, o2, "Failure message to keep");
    }

    public void refactorIfLiteralThenFail(int i) {
        // Keep this comment
        Assertions.assertNotEquals(i, 42);
        Assertions.assertNotEquals(i, 42, "Failure message to keep");
        Assertions.assertEquals(i, 42);
        Assertions.assertEquals(i, 42, "Failure message to keep");
        Assertions.assertNotEquals(i, 42);
        Assertions.assertNotEquals(i, 42, "Failure message to keep");
        Assertions.assertEquals(i, 42);
        Assertions.assertEquals(i, 42, "Failure message to keep");

        assertNotEquals(i, 42);
        assertNotEquals(i, 42, "Failure message to keep");
        assertEquals(i, 42);
        assertEquals(i, 42, "Failure message to keep");
        assertNotEquals(i, 42);
        assertNotEquals(i, 42, "Failure message to keep");
        assertEquals(i, 42);
        assertEquals(i, 42, "Failure message to keep");
    }

    public void refactorIfConstantThenFail(int i) {
        // Keep this comment
        Assertions.assertNotEquals(i, FOURTYTWO);
        Assertions.assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        Assertions.assertEquals(i, FOURTYTWO);
        Assertions.assertEquals(i, FOURTYTWO, "Failure message to keep");
        Assertions.assertNotEquals(i, FOURTYTWO);
        Assertions.assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        Assertions.assertEquals(i, FOURTYTWO);
        Assertions.assertEquals(i, FOURTYTWO, "Failure message to keep");

        assertNotEquals(i, FOURTYTWO);
        assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        assertEquals(i, FOURTYTWO);
        assertEquals(i, FOURTYTWO, "Failure message to keep");
        assertNotEquals(i, FOURTYTWO);
        assertNotEquals(i, FOURTYTWO, "Failure message to keep");
        assertEquals(i, FOURTYTWO);
        assertEquals(i, FOURTYTWO, "Failure message to keep");
    }

    public void refactorIfExpectedThenFail(int i, int expected) {
        // Keep this comment
        Assertions.assertNotEquals(i, expected);
        Assertions.assertNotEquals(i, expected, "Failure message to keep");
        Assertions.assertEquals(i, expected);
        Assertions.assertEquals(i, expected, "Failure message to keep");
        Assertions.assertNotEquals(i, expected);
        Assertions.assertNotEquals(i, expected, "Failure message to keep");
        Assertions.assertEquals(i, expected);
        Assertions.assertEquals(i, expected, "Failure message to keep");

        assertNotEquals(i, expected);
        assertNotEquals(i, expected, "Failure message to keep");
        assertEquals(i, expected);
        assertEquals(i, expected, "Failure message to keep");
        assertNotEquals(i, expected);
        assertNotEquals(i, expected, "Failure message to keep");
        assertEquals(i, expected);
        assertEquals(i, expected, "Failure message to keep");
    }

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
        // Keep this comment
        Assertions.assertNotEquals(i, expectedI);
        Assertions.assertNotEquals(i, expectedI, "Failure message to keep");
        Assertions.assertEquals(i, expectedI);
        Assertions.assertEquals(i, expectedI, "Failure message to keep");
        Assertions.assertNotEquals(i, expectedI);
        Assertions.assertNotEquals(i, expectedI, "Failure message to keep");
        Assertions.assertEquals(i, expectedI);
        Assertions.assertEquals(i, expectedI, "Failure message to keep");

        assertNotEquals(i, expectedI);
        assertNotEquals(i, expectedI, "Failure message to keep");
        assertEquals(i, expectedI);
        assertEquals(i, expectedI, "Failure message to keep");
        assertNotEquals(i, expectedI);
        assertNotEquals(i, expectedI, "Failure message to keep");
        assertEquals(i, expectedI);
        assertEquals(i, expectedI, "Failure message to keep");
    }

    public void doNotRefactorBecauseOfElseStatement(int i1, int i2, Object o1) {
        if (i1 == i2) {
            Assertions.fail();
        } else {
            System.out.println("keep me!");
        }
        if (o1 == null) {
            Assertions.fail();
        } else {
            System.out.println("keep me!");
        }
        Object o2 = i2;
        if (o1 == o2) {
            Assertions.fail();
        } else {
            System.out.println("keep me!");
        }
        if (o1.equals(o2)) {
            Assertions.fail();
        } else {
            System.out.println("keep me!");
        }
    }
}
