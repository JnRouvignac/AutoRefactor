/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.Date;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;

public class JupiterAssertSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
        // Keep this comment
        Assertions.assertEquals(i1, i2);
        Assertions.assertEquals(i1, i2, "Failure message to keep");
        Assertions.assertEquals(i1, i2, () -> "Failure message to keep");
        Assertions.assertNotEquals(i1, i2);
        Assertions.assertNotEquals(i1, i2, "Failure message to keep");
        Assertions.assertNotEquals(i1, i2, () -> "Failure message to keep");
        Assertions.assertEquals(i1, i2);
        Assertions.assertEquals(i1, i2, "Failure message to keep");
        Assertions.assertEquals(i1, i2, () -> "Failure message to keep");
        Assertions.assertNotEquals(i1, i2);
        Assertions.assertNotEquals(i1, i2, "Failure message to keep");
        Assertions.assertNotEquals(i1, i2, () -> "Failure message to keep");

        assertEquals(i1, i2);
        assertEquals(i1, i2, "Failure message to keep");
        assertEquals(i1, i2, () -> "Failure message to keep");
        assertNotEquals(i1, i2);
        assertNotEquals(i1, i2, "Failure message to keep");
        assertNotEquals(i1, i2, () -> "Failure message to keep");
        assertEquals(i1, i2);
        assertEquals(i1, i2, "Failure message to keep");
        assertEquals(i1, i2, () -> "Failure message to keep");
        assertNotEquals(i1, i2);
        assertNotEquals(i1, i2, "Failure message to keep");
        assertNotEquals(i1, i2, () -> "Failure message to keep");
    }

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assertions.assertEquals(d1, d2, .0);
        Assertions.assertEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertEquals(d1, d2, .0, () -> "Failure message to keep");
        Assertions.assertNotEquals(d1, d2, .0);
        Assertions.assertNotEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertNotEquals(d1, d2, .0, () -> "Failure message to keep");
        Assertions.assertEquals(d1, d2, .0);
        Assertions.assertEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertEquals(d1, d2, .0, () -> "Failure message to keep");
        Assertions.assertNotEquals(d1, d2, .0);
        Assertions.assertNotEquals(d1, d2, .0, "Failure message to keep");
        Assertions.assertNotEquals(d1, d2, .0, () -> "Failure message to keep");

        assertEquals(d1, d2, .0);
        assertEquals(d1, d2, .0, "Failure message to keep");
        assertEquals(d1, d2, .0, () -> "Failure message to keep");
        assertNotEquals(d1, d2, .0);
        assertNotEquals(d1, d2, .0, "Failure message to keep");
        assertNotEquals(d1, d2, .0, () -> "Failure message to keep");
        assertEquals(d1, d2, .0);
        assertEquals(d1, d2, .0, "Failure message to keep");
        assertEquals(d1, d2, .0, () -> "Failure message to keep");
        assertNotEquals(d1, d2, .0);
        assertNotEquals(d1, d2, .0, "Failure message to keep");
        assertNotEquals(d1, d2, .0, () -> "Failure message to keep");
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assertions.assertEquals(f1, f2, .0F);
        Assertions.assertEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertEquals(f1, f2, .0F, () -> "Failure message to keep");
        Assertions.assertNotEquals(f1, f2, .0F);
        Assertions.assertNotEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertNotEquals(f1, f2, .0F, () -> "Failure message to keep");
        Assertions.assertEquals(f1, f2, .0F);
        Assertions.assertEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertEquals(f1, f2, .0F, () -> "Failure message to keep");
        Assertions.assertNotEquals(f1, f2, .0F);
        Assertions.assertNotEquals(f1, f2, .0F, "Failure message to keep");
        Assertions.assertNotEquals(f1, f2, .0F, () -> "Failure message to keep");

        assertEquals(f1, f2, .0F);
        assertEquals(f1, f2, .0F, "Failure message to keep");
        assertEquals(f1, f2, .0F, () -> "Failure message to keep");
        assertNotEquals(f1, f2, .0F);
        assertNotEquals(f1, f2, .0F, "Failure message to keep");
        assertNotEquals(f1, f2, .0F, () -> "Failure message to keep");
        assertEquals(f1, f2, .0F);
        assertEquals(f1, f2, .0F, "Failure message to keep");
        assertEquals(f1, f2, .0F, () -> "Failure message to keep");
        assertNotEquals(f1, f2, .0F);
        assertNotEquals(f1, f2, .0F, "Failure message to keep");
        assertNotEquals(f1, f2, .0F, () -> "Failure message to keep");
    }

    public void refactorFailures() {
        // Keep this comment
        Assertions.fail();
        Assertions.fail("Failure message to keep");
        Assertions.fail(() -> "Failure message to keep");
        Assertions.fail();
        Assertions.fail("Failure message to keep");
        Assertions.fail(() -> "Failure message to keep");

        fail();
        fail("Failure message to keep");
        fail(() -> "Failure message to keep");
        fail();
        fail("Failure message to keep");
        fail(() -> "Failure message to keep");
    }

    public void removeDeadChecks() {
    }

    public void refactorNegatedConditions(boolean b) {
        // Keep this comment
        Assertions.assertFalse(b);
        Assertions.assertFalse(b, "Failure message to keep");
        Assertions.assertFalse(b, () -> "Failure message to keep");
        Assertions.assertTrue(b);
        Assertions.assertTrue(b, "Failure message to keep");
        Assertions.assertTrue(b, () -> "Failure message to keep");

        assertFalse(b);
        assertFalse(b, "Failure message to keep");
        assertFalse(b, () -> "Failure message to keep");
        assertTrue(b);
        assertTrue(b, "Failure message to keep");
        assertTrue(b, () -> "Failure message to keep");
    }

    public void refactorWithObjectReferences(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertSame(o1, o2);
        Assertions.assertSame(o1, o2, "Failure message to keep");
        Assertions.assertSame(o1, o2, () -> "Failure message to keep");
        Assertions.assertNotSame(o1, o2);
        Assertions.assertNotSame(o1, o2, "Failure message to keep");
        Assertions.assertNotSame(o1, o2, () -> "Failure message to keep");
        Assertions.assertSame(o1, o2);
        Assertions.assertSame(o1, o2, "Failure message to keep");
        Assertions.assertSame(o1, o2, () -> "Failure message to keep");
        Assertions.assertNotSame(o1, o2);
        Assertions.assertNotSame(o1, o2, "Failure message to keep");
        Assertions.assertNotSame(o1, o2, () -> "Failure message to keep");

        assertSame(o1, o2);
        assertSame(o1, o2, "Failure message to keep");
        assertSame(o1, o2, () -> "Failure message to keep");
        assertNotSame(o1, o2);
        assertNotSame(o1, o2, "Failure message to keep");
        assertNotSame(o1, o2, () -> "Failure message to keep");
        assertSame(o1, o2);
        assertSame(o1, o2, "Failure message to keep");
        assertSame(o1, o2, () -> "Failure message to keep");
        assertNotSame(o1, o2);
        assertNotSame(o1, o2, "Failure message to keep");
        assertNotSame(o1, o2, () -> "Failure message to keep");
    }

    public void refactorWithObjects(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertEquals(o1, o2);
        Assertions.assertEquals(o1, o2, "Failure message to keep");
        Assertions.assertEquals(o1, o2, () -> "Failure message to keep");
        Assertions.assertNotEquals(o1, o2);
        Assertions.assertNotEquals(o1, o2, "Failure message to keep");
        Assertions.assertNotEquals(o1, o2, () -> "Failure message to keep");
        Assertions.assertEquals(o1, o2);
        Assertions.assertEquals(o1, o2, "Failure message to keep");
        Assertions.assertEquals(o1, o2, () -> "Failure message to keep");
        Assertions.assertNotEquals(o1, o2);
        Assertions.assertNotEquals(o1, o2, "Failure message to keep");
        Assertions.assertNotEquals(o1, o2, () -> "Failure message to keep");

        assertEquals(o1, o2);
        assertEquals(o1, o2, "Failure message to keep");
        assertEquals(o1, o2, () -> "Failure message to keep");
        assertNotEquals(o1, o2);
        assertNotEquals(o1, o2, "Failure message to keep");
        assertNotEquals(o1, o2, () -> "Failure message to keep");
        assertEquals(o1, o2);
        assertEquals(o1, o2, "Failure message to keep");
        assertEquals(o1, o2, () -> "Failure message to keep");
        assertNotEquals(o1, o2);
        assertNotEquals(o1, o2, "Failure message to keep");
        assertNotEquals(o1, o2, () -> "Failure message to keep");
    }

    public void refactorNullCheckFirstArg(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNull(o, () -> "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNotNull(o, () -> "Failure message to keep");
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNull(o, () -> "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNotNull(o, () -> "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNull(o, () -> "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNotNull(o, () -> "Failure message to keep");
        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNull(o, () -> "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNotNull(o, () -> "Failure message to keep");
    }

    public void refactorNullCheckSecondArg(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNull(o, () -> "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNotNull(o, () -> "Failure message to keep");
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNull(o, () -> "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNotNull(o, () -> "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNull(o, () -> "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNotNull(o, () -> "Failure message to keep");
        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNull(o, () -> "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNotNull(o, () -> "Failure message to keep");
    }

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNull(o, () -> "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNotNull(o, () -> "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNull(o, () -> "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNotNull(o, () -> "Failure message to keep");
    }

    public void refactorNullCheckSecondArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertNull(o);
        Assertions.assertNull(o, "Failure message to keep");
        Assertions.assertNull(o, () -> "Failure message to keep");
        Assertions.assertNotNull(o);
        Assertions.assertNotNull(o, "Failure message to keep");
        Assertions.assertNotNull(o, () -> "Failure message to keep");

        assertNull(o);
        assertNull(o, "Failure message to keep");
        assertNull(o, () -> "Failure message to keep");
        assertNotNull(o);
        assertNotNull(o, "Failure message to keep");
        assertNotNull(o, () -> "Failure message to keep");
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(42, o);
        Assertions.assertEquals(42, o, "Failure message to keep");
        Assertions.assertEquals(42, o, () -> "Failure message to keep");
        Assertions.assertNotEquals(42, o);
        Assertions.assertNotEquals(42, o, "Failure message to keep");
        Assertions.assertNotEquals(42, o, () -> "Failure message to keep");

        assertEquals(42, o);
        assertEquals(42, o, "Failure message to keep");
        assertEquals(42, o, () -> "Failure message to keep");
        assertNotEquals(42, o);
        assertNotEquals(42, o, "Failure message to keep");
        assertNotEquals(42, o, () -> "Failure message to keep");
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assertions.assertEquals(42L, l);
        Assertions.assertEquals(42L, l, "Failure message to keep");
        Assertions.assertEquals(42L, l, () -> "Failure message to keep");
        Assertions.assertNotEquals(42L, l);
        Assertions.assertNotEquals(42L, l, "Failure message to keep");
        Assertions.assertNotEquals(42L, l, () -> "Failure message to keep");

        assertEquals(42L, l);
        assertEquals(42L, l, "Failure message to keep");
        assertEquals(42L, l, () -> "Failure message to keep");
        assertNotEquals(42L, l);
        assertNotEquals(42L, l, "Failure message to keep");
        assertNotEquals(42L, l, () -> "Failure message to keep");
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assertions.assertEquals(42, i);
        Assertions.assertEquals(42, i, "Failure message to keep");
        Assertions.assertEquals(42, i, () -> "Failure message to keep");
        Assertions.assertNotEquals(42, i);
        Assertions.assertNotEquals(42, i, "Failure message to keep");
        Assertions.assertNotEquals(42, i, () -> "Failure message to keep");

        assertEquals(42, i);
        assertEquals(42, i, "Failure message to keep");
        assertEquals(42, i, () -> "Failure message to keep");
        assertNotEquals(42, i);
        assertNotEquals(42, i, "Failure message to keep");
        assertNotEquals(42, i, () -> "Failure message to keep");
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assertions.assertEquals('a', c);
        Assertions.assertEquals('a', c, "Failure message to keep");
        Assertions.assertEquals('a', c, () -> "Failure message to keep");
        Assertions.assertNotEquals('a', c);
        Assertions.assertNotEquals('a', c, "Failure message to keep");
        Assertions.assertNotEquals('a', c, () -> "Failure message to keep");

        assertEquals('a', c);
        assertEquals('a', c, "Failure message to keep");
        assertEquals('a', c, () -> "Failure message to keep");
        assertNotEquals('a', c);
        assertNotEquals('a', c, "Failure message to keep");
        assertNotEquals('a', c, () -> "Failure message to keep");
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assertions.assertEquals(1 + 2 + 3, i);
        Assertions.assertEquals(1 + 2 + 3, i, "Failure message to keep");
        Assertions.assertEquals(1 + 2 + 3, i, () -> "Failure message to keep");
        Assertions.assertNotEquals(1 + 2 + 3, i);
        Assertions.assertNotEquals(1 + 2 + 3, i, "Failure message to keep");
        Assertions.assertNotEquals(1 + 2 + 3, i, () -> "Failure message to keep");

        assertEquals(1 + 2 + 3, i);
        assertEquals(1 + 2 + 3, i, "Failure message to keep");
        assertEquals(1 + 2 + 3, i, () -> "Failure message to keep");
        assertNotEquals(1 + 2 + 3, i);
        assertNotEquals(1 + 2 + 3, i, "Failure message to keep");
        assertNotEquals(1 + 2 + 3, i, () -> "Failure message to keep");
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) {
        Assertions.assertEquals(42, o);
        Assertions.assertEquals(42, o, "Failure message to keep");
        Assertions.assertEquals(42, o, () -> "Failure message to keep");
        Assertions.assertNotEquals(42, o);
        Assertions.assertNotEquals(42, o, "Failure message to keep");
        Assertions.assertNotEquals(42, o, () -> "Failure message to keep");

        assertEquals(42, o);
        assertEquals(42, o, "Failure message to keep");
        assertEquals(42, o, () -> "Failure message to keep");
        assertNotEquals(42, o);
        assertNotEquals(42, o, "Failure message to keep");
        assertNotEquals(42, o, () -> "Failure message to keep");
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(FOURTYTWO, o);
        Assertions.assertEquals(FOURTYTWO, o, "Failure message to keep");
        Assertions.assertEquals(FOURTYTWO, o, () -> "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, o);
        Assertions.assertNotEquals(FOURTYTWO, o, "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, o, () -> "Failure message to keep");

        assertEquals(FOURTYTWO, o);
        assertEquals(FOURTYTWO, o, "Failure message to keep");
        assertEquals(FOURTYTWO, o, () -> "Failure message to keep");
        assertNotEquals(FOURTYTWO, o);
        assertNotEquals(FOURTYTWO, o, "Failure message to keep");
        assertNotEquals(FOURTYTWO, o, () -> "Failure message to keep");
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) {
        Assertions.assertEquals(FOURTYTWO, o);
        Assertions.assertEquals(FOURTYTWO, o, "Failure message to keep");
        Assertions.assertEquals(FOURTYTWO, o, () -> "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, o);
        Assertions.assertNotEquals(FOURTYTWO, o, "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, o, () -> "Failure message to keep");

        assertEquals(FOURTYTWO, o);
        assertEquals(FOURTYTWO, o, "Failure message to keep");
        assertEquals(FOURTYTWO, o, () -> "Failure message to keep");
        assertNotEquals(FOURTYTWO, o);
        assertNotEquals(FOURTYTWO, o, "Failure message to keep");
        assertNotEquals(FOURTYTWO, o, () -> "Failure message to keep");
    }

    public void moveExpectedVariableAsExpectedArgWithEquals(Object o, int expected) {
        // Keep this comment
        Assertions.assertEquals(expected, o);
        Assertions.assertEquals(expected, o, "Failure message to keep");
        Assertions.assertEquals(expected, o, () -> "Failure message to keep");
        Assertions.assertNotEquals(expected, o);
        Assertions.assertNotEquals(expected, o, "Failure message to keep");
        Assertions.assertNotEquals(expected, o, () -> "Failure message to keep");

        assertEquals(expected, o);
        assertEquals(expected, o, "Failure message to keep");
        assertEquals(expected, o, () -> "Failure message to keep");
        assertNotEquals(expected, o);
        assertNotEquals(expected, o, "Failure message to keep");
        assertNotEquals(expected, o, () -> "Failure message to keep");

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(expceted, o);
    }

    public void refactorIfOnBoolean(boolean b) {
        // Keep this comment
        Assertions.assertFalse(b);
        Assertions.assertFalse(b, "Failure message to keep");

        assertTrue(b);
        assertTrue(b, "Failure message to keep");
    }

    public void refactorSuppliedObject(Date nullDate) {
        // Keep this comment
        Assertions.assertNull(nullDate, () -> "The date should be null: " + nullDate.getTime());

        assertNull(nullDate, () -> "The date should be null: " + nullDate.getTime());
    }

    public void doNotRefactorUsedObject(Date nullDate) {
        if (nullDate != null) {
            Assertions.fail("The date should be null: " + nullDate.getTime());
        }

        if (nullDate != null) {
            fail("The date should be null: " + nullDate.getTime());
        }
    }

    public void refactorNotUsedObject(Date nullDate, Date notNullDate) {
        // Keep this comment
        Assertions.assertNull(nullDate, "The date should be null, not like: " + notNullDate.getTime());

        assertNull(nullDate, "The date should be null, not like: " + notNullDate.getTime());
    }

    public void refactorIfOnExpression(boolean b1, boolean b2) {
        // Keep this comment
        Assertions.assertFalse(b1 && b2);
        Assertions.assertFalse(b1 || b2, "Failure message to keep");

        assertFalse(!b1 && !b2);
        assertFalse(!b1 || !b2, "Failure message to keep");
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
        // Keep this comment
        Assertions.assertNotEquals(i1, i2);
        Assertions.assertNotEquals(i1, i2, "Failure message to keep");
        Assertions.assertNotEquals(i1, i2, () -> "Failure message to keep");
        Assertions.assertEquals(i1, i2);
        Assertions.assertEquals(i1, i2, "Failure message to keep");
        Assertions.assertEquals(i1, i2, () -> "Failure message to keep");

        assertNotEquals(i1, i2);
        assertNotEquals(i1, i2, "Failure message to keep");
        assertNotEquals(i1, i2, () -> "Failure message to keep");
        assertEquals(i1, i2);
        assertEquals(i1, i2, "Failure message to keep");
        assertEquals(i1, i2, () -> "Failure message to keep");
    }

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertNotSame(o1, o2);
        Assertions.assertNotSame(o1, o2, "Failure message to keep");
        Assertions.assertNotSame(o1, o2, () -> "Failure message to keep");
        Assertions.assertSame(o1, o2);
        Assertions.assertSame(o1, o2, "Failure message to keep");
        Assertions.assertSame(o1, o2, () -> "Failure message to keep");

        assertNotSame(o1, o2);
        assertNotSame(o1, o2, "Failure message to keep");
        assertNotSame(o1, o2, () -> "Failure message to keep");
        assertSame(o1, o2);
        assertSame(o1, o2, "Failure message to keep");
        assertSame(o1, o2, () -> "Failure message to keep");
    }

    public void refactorIfNullThenFail(Object o1) {
        // Keep this comment
        Assertions.assertNotNull(o1);
        Assertions.assertNotNull(o1, "Failure message to keep");
        Assertions.assertNotNull(o1, () -> "Failure message to keep");
        Assertions.assertNull(o1);
        Assertions.assertNull(o1, "Failure message to keep");
        Assertions.assertNull(o1, () -> "Failure message to keep");
        Assertions.assertNotNull(o1);
        Assertions.assertNotNull(o1, "Failure message to keep");
        Assertions.assertNotNull(o1, () -> "Failure message to keep");
        Assertions.assertNull(o1);
        Assertions.assertNull(o1, "Failure message to keep");
        Assertions.assertNull(o1, () -> "Failure message to keep");

        assertNotNull(o1);
        assertNotNull(o1, "Failure message to keep");
        assertNotNull(o1, () -> "Failure message to keep");
        assertNull(o1);
        assertNull(o1, "Failure message to keep");
        assertNull(o1, () -> "Failure message to keep");
        assertNotNull(o1);
        assertNotNull(o1, "Failure message to keep");
        assertNotNull(o1, () -> "Failure message to keep");
        assertNull(o1);
        assertNull(o1, "Failure message to keep");
        assertNull(o1, () -> "Failure message to keep");
    }

    public void refactorIfObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertNotEquals(o1, o2);
        Assertions.assertNotEquals(o1, o2, "Failure message to keep");
        Assertions.assertNotEquals(o1, o2, () -> "Failure message to keep");
        Assertions.assertEquals(o1, o2);
        Assertions.assertEquals(o1, o2, "Failure message to keep");
        Assertions.assertEquals(o1, o2, () -> "Failure message to keep");

        assertNotEquals(o1, o2);
        assertNotEquals(o1, o2, "Failure message to keep");
        assertNotEquals(o1, o2, () -> "Failure message to keep");
        assertEquals(o1, o2);
        assertEquals(o1, o2, "Failure message to keep");
        assertEquals(o1, o2, () -> "Failure message to keep");
    }

    public void refactorIfLiteralThenFail(int i) {
        // Keep this comment
        Assertions.assertNotEquals(42, i);
        Assertions.assertNotEquals(42, i, "Failure message to keep");
        Assertions.assertNotEquals(42, i, () -> "Failure message to keep");
        Assertions.assertEquals(42, i);
        Assertions.assertEquals(42, i, "Failure message to keep");
        Assertions.assertEquals(42, i, () -> "Failure message to keep");
        Assertions.assertNotEquals(42, i);
        Assertions.assertNotEquals(42, i, "Failure message to keep");
        Assertions.assertNotEquals(42, i, () -> "Failure message to keep");
        Assertions.assertEquals(42, i);
        Assertions.assertEquals(42, i, "Failure message to keep");
        Assertions.assertEquals(42, i, () -> "Failure message to keep");

        assertNotEquals(42, i);
        assertNotEquals(42, i, "Failure message to keep");
        assertNotEquals(42, i, () -> "Failure message to keep");
        assertEquals(42, i);
        assertEquals(42, i, "Failure message to keep");
        assertEquals(42, i, () -> "Failure message to keep");
        assertNotEquals(42, i);
        assertNotEquals(42, i, "Failure message to keep");
        assertNotEquals(42, i, () -> "Failure message to keep");
        assertEquals(42, i);
        assertEquals(42, i, "Failure message to keep");
        assertEquals(42, i, () -> "Failure message to keep");
    }

    public void refactorIfConstantThenFail(int i) {
        // Keep this comment
        Assertions.assertNotEquals(FOURTYTWO, i);
        Assertions.assertNotEquals(FOURTYTWO, i, "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, i, () -> "Failure message to keep");
        Assertions.assertEquals(FOURTYTWO, i);
        Assertions.assertEquals(FOURTYTWO, i, "Failure message to keep");
        Assertions.assertEquals(FOURTYTWO, i, () -> "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, i);
        Assertions.assertNotEquals(FOURTYTWO, i, "Failure message to keep");
        Assertions.assertNotEquals(FOURTYTWO, i, () -> "Failure message to keep");
        Assertions.assertEquals(FOURTYTWO, i);
        Assertions.assertEquals(FOURTYTWO, i, "Failure message to keep");
        Assertions.assertEquals(FOURTYTWO, i, () -> "Failure message to keep");

        assertNotEquals(FOURTYTWO, i);
        assertNotEquals(FOURTYTWO, i, "Failure message to keep");
        assertNotEquals(FOURTYTWO, i, () -> "Failure message to keep");
        assertEquals(FOURTYTWO, i);
        assertEquals(FOURTYTWO, i, "Failure message to keep");
        assertEquals(FOURTYTWO, i, () -> "Failure message to keep");
        assertNotEquals(FOURTYTWO, i);
        assertNotEquals(FOURTYTWO, i, "Failure message to keep");
        assertNotEquals(FOURTYTWO, i, () -> "Failure message to keep");
        assertEquals(FOURTYTWO, i);
        assertEquals(FOURTYTWO, i, "Failure message to keep");
        assertEquals(FOURTYTWO, i, () -> "Failure message to keep");
    }

    public void refactorIfExpectedThenFail(int i, int expected) {
        // Keep this comment
        Assertions.assertNotEquals(expected, i);
        Assertions.assertNotEquals(expected, i, "Failure message to keep");
        Assertions.assertNotEquals(expected, i, () -> "Failure message to keep");
        Assertions.assertEquals(expected, i);
        Assertions.assertEquals(expected, i, "Failure message to keep");
        Assertions.assertEquals(expected, i, () -> "Failure message to keep");
        Assertions.assertNotEquals(expected, i);
        Assertions.assertNotEquals(expected, i, "Failure message to keep");
        Assertions.assertNotEquals(expected, i, () -> "Failure message to keep");
        Assertions.assertEquals(expected, i);
        Assertions.assertEquals(expected, i, "Failure message to keep");
        Assertions.assertEquals(expected, i, () -> "Failure message to keep");

        assertNotEquals(expected, i);
        assertNotEquals(expected, i, "Failure message to keep");
        assertNotEquals(expected, i, () -> "Failure message to keep");
        assertEquals(expected, i);
        assertEquals(expected, i, "Failure message to keep");
        assertEquals(expected, i, () -> "Failure message to keep");
        assertNotEquals(expected, i);
        assertNotEquals(expected, i, "Failure message to keep");
        assertNotEquals(expected, i, () -> "Failure message to keep");
        assertEquals(expected, i);
        assertEquals(expected, i, "Failure message to keep");
        assertEquals(expected, i, () -> "Failure message to keep");
    }

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
        // Keep this comment
        Assertions.assertNotEquals(expectedI, i);
        Assertions.assertNotEquals(expectedI, i, "Failure message to keep");
        Assertions.assertNotEquals(expectedI, i, () -> "Failure message to keep");
        Assertions.assertEquals(expectedI, i);
        Assertions.assertEquals(expectedI, i, "Failure message to keep");
        Assertions.assertEquals(expectedI, i, () -> "Failure message to keep");
        Assertions.assertNotEquals(expectedI, i);
        Assertions.assertNotEquals(expectedI, i, "Failure message to keep");
        Assertions.assertNotEquals(expectedI, i, () -> "Failure message to keep");
        Assertions.assertEquals(expectedI, i);
        Assertions.assertEquals(expectedI, i, "Failure message to keep");
        Assertions.assertEquals(expectedI, i, () -> "Failure message to keep");

        assertNotEquals(expectedI, i);
        assertNotEquals(expectedI, i, "Failure message to keep");
        assertNotEquals(expectedI, i, () -> "Failure message to keep");
        assertEquals(expectedI, i);
        assertEquals(expectedI, i, "Failure message to keep");
        assertEquals(expectedI, i, () -> "Failure message to keep");
        assertNotEquals(expectedI, i);
        assertNotEquals(expectedI, i, "Failure message to keep");
        assertNotEquals(expectedI, i, () -> "Failure message to keep");
        assertEquals(expectedI, i);
        assertEquals(expectedI, i, "Failure message to keep");
        assertEquals(expectedI, i, () -> "Failure message to keep");
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
