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

import static org.junit.jupiter.api.Assertions.*;

import java.util.Date;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;

public class JupiterAssertSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
        // Keep this comment
        Assertions.assertTrue(i1 == i2);
        Assertions.assertTrue(i1 == i2, "Failure message to keep");
        Assertions.assertTrue(i1 == i2, () -> "Failure message to keep");
        Assertions.assertTrue(i1 != i2);
        Assertions.assertTrue(i1 != i2, "Failure message to keep");
        Assertions.assertTrue(i1 != i2, () -> "Failure message to keep");
        Assertions.assertFalse(i1 != i2);
        Assertions.assertFalse(i1 != i2, "Failure message to keep");
        Assertions.assertFalse(i1 != i2, () -> "Failure message to keep");
        Assertions.assertFalse(i1 == i2);
        Assertions.assertFalse(i1 == i2, "Failure message to keep");
        Assertions.assertFalse(i1 == i2, () -> "Failure message to keep");

        assertTrue(i1 == i2);
        assertTrue(i1 == i2, "Failure message to keep");
        assertTrue(i1 == i2, () -> "Failure message to keep");
        assertTrue(i1 != i2);
        assertTrue(i1 != i2, "Failure message to keep");
        assertTrue(i1 != i2, () -> "Failure message to keep");
        assertFalse(i1 != i2);
        assertFalse(i1 != i2, "Failure message to keep");
        assertFalse(i1 != i2, () -> "Failure message to keep");
        assertFalse(i1 == i2);
        assertFalse(i1 == i2, "Failure message to keep");
        assertFalse(i1 == i2, () -> "Failure message to keep");
    }

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assertions.assertTrue(d1 == d2);
        Assertions.assertTrue(d1 == d2, "Failure message to keep");
        Assertions.assertTrue(d1 == d2, () -> "Failure message to keep");
        Assertions.assertTrue(d1 != d2);
        Assertions.assertTrue(d1 != d2, "Failure message to keep");
        Assertions.assertTrue(d1 != d2, () -> "Failure message to keep");
        Assertions.assertFalse(d1 != d2);
        Assertions.assertFalse(d1 != d2, "Failure message to keep");
        Assertions.assertFalse(d1 != d2, () -> "Failure message to keep");
        Assertions.assertFalse(d1 == d2);
        Assertions.assertFalse(d1 == d2, "Failure message to keep");
        Assertions.assertFalse(d1 == d2, () -> "Failure message to keep");

        assertTrue(d1 == d2);
        assertTrue(d1 == d2, "Failure message to keep");
        assertTrue(d1 == d2, () -> "Failure message to keep");
        assertTrue(d1 != d2);
        assertTrue(d1 != d2, "Failure message to keep");
        assertTrue(d1 != d2, () -> "Failure message to keep");
        assertFalse(d1 != d2);
        assertFalse(d1 != d2, "Failure message to keep");
        assertFalse(d1 != d2, () -> "Failure message to keep");
        assertFalse(d1 == d2);
        assertFalse(d1 == d2, "Failure message to keep");
        assertFalse(d1 == d2, () -> "Failure message to keep");
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assertions.assertTrue(f1 == f2);
        Assertions.assertTrue(f1 == f2, "Failure message to keep");
        Assertions.assertTrue(f1 == f2, () -> "Failure message to keep");
        Assertions.assertTrue(f1 != f2);
        Assertions.assertTrue(f1 != f2, "Failure message to keep");
        Assertions.assertTrue(f1 != f2, () -> "Failure message to keep");
        Assertions.assertFalse(f1 != f2);
        Assertions.assertFalse(f1 != f2, "Failure message to keep");
        Assertions.assertFalse(f1 != f2, () -> "Failure message to keep");
        Assertions.assertFalse(f1 == f2);
        Assertions.assertFalse(f1 == f2, "Failure message to keep");
        Assertions.assertFalse(f1 == f2, () -> "Failure message to keep");

        assertTrue(f1 == f2);
        assertTrue(f1 == f2, "Failure message to keep");
        assertTrue(f1 == f2, () -> "Failure message to keep");
        assertTrue(f1 != f2);
        assertTrue(f1 != f2, "Failure message to keep");
        assertTrue(f1 != f2, () -> "Failure message to keep");
        assertFalse(f1 != f2);
        assertFalse(f1 != f2, "Failure message to keep");
        assertFalse(f1 != f2, () -> "Failure message to keep");
        assertFalse(f1 == f2);
        assertFalse(f1 == f2, "Failure message to keep");
        assertFalse(f1 == f2, () -> "Failure message to keep");
    }

    public void refactorFailures() {
        // Keep this comment
        Assertions.assertTrue(false);
        Assertions.assertTrue(false, "Failure message to keep");
        Assertions.assertTrue(false, () -> "Failure message to keep");
        Assertions.assertFalse(true);
        Assertions.assertFalse(true, "Failure message to keep");
        Assertions.assertFalse(true, () -> "Failure message to keep");

        assertTrue(false);
        assertTrue(false, "Failure message to keep");
        assertTrue(false, () -> "Failure message to keep");
        assertFalse(true);
        assertFalse(true, "Failure message to keep");
        assertFalse(true, () -> "Failure message to keep");
    }

    public void removeDeadChecks() {
        Assertions.assertTrue(true);
        Assertions.assertTrue(true, "Failure message to keep");
        Assertions.assertTrue(true, () -> "Failure message to keep");
        Assertions.assertFalse(false);
        Assertions.assertFalse(false, "Failure message to keep");
        Assertions.assertFalse(false, () -> "Failure message to keep");

        assertTrue(true);
        assertTrue(true, "Failure message to keep");
        assertTrue(true, () -> "Failure message to keep");
        assertFalse(false);
        assertFalse(false, "Failure message to keep");
        assertFalse(false, () -> "Failure message to keep");
    }

    public void refactorNegatedConditions(boolean b) {
        // Keep this comment
        Assertions.assertTrue(!b);
        Assertions.assertTrue(!b, "Failure message to keep");
        Assertions.assertTrue(!b, () -> "Failure message to keep");
        Assertions.assertFalse(!b);
        Assertions.assertFalse(!b, "Failure message to keep");
        Assertions.assertFalse(!b, () -> "Failure message to keep");

        assertTrue(!b);
        assertTrue(!b, "Failure message to keep");
        assertTrue(!b, () -> "Failure message to keep");
        assertFalse(!b);
        assertFalse(!b, "Failure message to keep");
        assertFalse(!b, () -> "Failure message to keep");
    }

    public void refactorWithObjectReferences(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertTrue(o1 == o2);
        Assertions.assertTrue(o1 == o2, "Failure message to keep");
        Assertions.assertTrue(o1 == o2, () -> "Failure message to keep");
        Assertions.assertTrue(o1 != o2);
        Assertions.assertTrue(o1 != o2, "Failure message to keep");
        Assertions.assertTrue(o1 != o2, () -> "Failure message to keep");
        Assertions.assertFalse(o1 != o2);
        Assertions.assertFalse(o1 != o2, "Failure message to keep");
        Assertions.assertFalse(o1 != o2, () -> "Failure message to keep");
        Assertions.assertFalse(o1 == o2);
        Assertions.assertFalse(o1 == o2, "Failure message to keep");
        Assertions.assertFalse(o1 == o2, () -> "Failure message to keep");

        assertTrue(o1 == o2);
        assertTrue(o1 == o2, "Failure message to keep");
        assertTrue(o1 == o2, () -> "Failure message to keep");
        assertTrue(o1 != o2);
        assertTrue(o1 != o2, "Failure message to keep");
        assertTrue(o1 != o2, () -> "Failure message to keep");
        assertFalse(o1 != o2);
        assertFalse(o1 != o2, "Failure message to keep");
        assertFalse(o1 != o2, () -> "Failure message to keep");
        assertFalse(o1 == o2);
        assertFalse(o1 == o2, "Failure message to keep");
        assertFalse(o1 == o2, () -> "Failure message to keep");
    }

    public void refactorWithObjects(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertTrue(o1.equals(o2));
        Assertions.assertTrue(o1.equals(o2), "Failure message to keep");
        Assertions.assertTrue(o1.equals(o2), () -> "Failure message to keep");
        Assertions.assertTrue(!(o1.equals(o2)));
        Assertions.assertTrue(!(o1.equals(o2)), "Failure message to keep");
        Assertions.assertTrue(!(o1.equals(o2)), () -> "Failure message to keep");
        Assertions.assertFalse(!(o1.equals(o2)));
        Assertions.assertFalse(!(o1.equals(o2)), "Failure message to keep");
        Assertions.assertFalse(!(o1.equals(o2)), () -> "Failure message to keep");
        Assertions.assertFalse(o1.equals(o2));
        Assertions.assertFalse(o1.equals(o2), "Failure message to keep");
        Assertions.assertFalse(o1.equals(o2), () -> "Failure message to keep");

        assertTrue(o1.equals(o2));
        assertTrue(o1.equals(o2), "Failure message to keep");
        assertTrue(o1.equals(o2), () -> "Failure message to keep");
        assertTrue(!(o1.equals(o2)));
        assertTrue(!(o1.equals(o2)), "Failure message to keep");
        assertTrue(!(o1.equals(o2)), () -> "Failure message to keep");
        assertFalse(!(o1.equals(o2)));
        assertFalse(!(o1.equals(o2)), "Failure message to keep");
        assertFalse(!(o1.equals(o2)), () -> "Failure message to keep");
        assertFalse(o1.equals(o2));
        assertFalse(o1.equals(o2), "Failure message to keep");
        assertFalse(o1.equals(o2), () -> "Failure message to keep");
    }

    public void refactorNullCheckFirstArg(Object o) {
        // Keep this comment
        Assertions.assertTrue(null == o);
        Assertions.assertTrue(null == o, "Failure message to keep");
        Assertions.assertTrue(null == o, () -> "Failure message to keep");
        Assertions.assertTrue(null != o);
        Assertions.assertTrue(null != o, "Failure message to keep");
        Assertions.assertTrue(null != o, () -> "Failure message to keep");
        Assertions.assertFalse(null != o);
        Assertions.assertFalse(null != o, "Failure message to keep");
        Assertions.assertFalse(null != o, () -> "Failure message to keep");
        Assertions.assertFalse(null == o);
        Assertions.assertFalse(null == o, "Failure message to keep");
        Assertions.assertFalse(null == o, () -> "Failure message to keep");

        assertTrue(null == o);
        assertTrue(null == o, "Failure message to keep");
        assertTrue(null == o, () -> "Failure message to keep");
        assertTrue(null != o);
        assertTrue(null != o, "Failure message to keep");
        assertTrue(null != o, () -> "Failure message to keep");
        assertFalse(null != o);
        assertFalse(null != o, "Failure message to keep");
        assertFalse(null != o, () -> "Failure message to keep");
        assertFalse(null == o);
        assertFalse(null == o, "Failure message to keep");
        assertFalse(null == o, () -> "Failure message to keep");
    }

    public void refactorNullCheckSecondArg(Object o) {
        // Keep this comment
        Assertions.assertTrue(o == null);
        Assertions.assertTrue(o == null, "Failure message to keep");
        Assertions.assertTrue(o == null, () -> "Failure message to keep");
        Assertions.assertTrue(o != null);
        Assertions.assertTrue(o != null, "Failure message to keep");
        Assertions.assertTrue(o != null, () -> "Failure message to keep");
        Assertions.assertFalse(o != null);
        Assertions.assertFalse(o != null, "Failure message to keep");
        Assertions.assertFalse(o != null, () -> "Failure message to keep");
        Assertions.assertFalse(o == null);
        Assertions.assertFalse(o == null, "Failure message to keep");
        Assertions.assertFalse(o == null, () -> "Failure message to keep");

        assertTrue(o == null);
        assertTrue(o == null, "Failure message to keep");
        assertTrue(o == null, () -> "Failure message to keep");
        assertTrue(o != null);
        assertTrue(o != null, "Failure message to keep");
        assertTrue(o != null, () -> "Failure message to keep");
        assertFalse(o != null);
        assertFalse(o != null, "Failure message to keep");
        assertFalse(o != null, () -> "Failure message to keep");
        assertFalse(o == null);
        assertFalse(o == null, "Failure message to keep");
        assertFalse(o == null, () -> "Failure message to keep");
    }

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(null, o);
        Assertions.assertEquals(null, o, "Failure message to keep");
        Assertions.assertEquals(null, o, () -> "Failure message to keep");
        Assertions.assertNotEquals(null, o);
        Assertions.assertNotEquals(null, o, "Failure message to keep");
        Assertions.assertNotEquals(null, o, () -> "Failure message to keep");

        assertEquals(null, o);
        assertEquals(null, o, "Failure message to keep");
        assertEquals(null, o, () -> "Failure message to keep");
        assertNotEquals(null, o);
        assertNotEquals(null, o, "Failure message to keep");
        assertNotEquals(null, o, () -> "Failure message to keep");
    }

    public void refactorNullCheckSecondArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(o, null);
        Assertions.assertEquals(o, null, "Failure message to keep");
        Assertions.assertEquals(o, null, () -> "Failure message to keep");
        Assertions.assertNotEquals(o, null);
        Assertions.assertNotEquals(o, null, "Failure message to keep");
        Assertions.assertNotEquals(o, null, () -> "Failure message to keep");

        assertEquals(o, null);
        assertEquals(o, null, "Failure message to keep");
        assertEquals(o, null, () -> "Failure message to keep");
        assertNotEquals(o, null);
        assertNotEquals(o, null, "Failure message to keep");
        assertNotEquals(o, null, () -> "Failure message to keep");
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(o, 42);
        Assertions.assertEquals(o, 42, "Failure message to keep");
        Assertions.assertEquals(o, 42, () -> "Failure message to keep");
        Assertions.assertNotEquals(o, 42);
        Assertions.assertNotEquals(o, 42, "Failure message to keep");
        Assertions.assertNotEquals(o, 42, () -> "Failure message to keep");

        assertEquals(o, 42);
        assertEquals(o, 42, "Failure message to keep");
        assertEquals(o, 42, () -> "Failure message to keep");
        assertNotEquals(o, 42);
        assertNotEquals(o, 42, "Failure message to keep");
        assertNotEquals(o, 42, () -> "Failure message to keep");
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assertions.assertEquals(l, 42L);
        Assertions.assertEquals(l, 42L, "Failure message to keep");
        Assertions.assertEquals(l, 42L, () -> "Failure message to keep");
        Assertions.assertNotEquals(l, 42L);
        Assertions.assertNotEquals(l, 42L, "Failure message to keep");
        Assertions.assertNotEquals(l, 42L, () -> "Failure message to keep");

        assertEquals(l, 42L);
        assertEquals(l, 42L, "Failure message to keep");
        assertEquals(l, 42L, () -> "Failure message to keep");
        assertNotEquals(l, 42L);
        assertNotEquals(l, 42L, "Failure message to keep");
        assertNotEquals(l, 42L, () -> "Failure message to keep");
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assertions.assertEquals(i, 42);
        Assertions.assertEquals(i, 42, "Failure message to keep");
        Assertions.assertEquals(i, 42, () -> "Failure message to keep");
        Assertions.assertNotEquals(i, 42);
        Assertions.assertNotEquals(i, 42, "Failure message to keep");
        Assertions.assertNotEquals(i, 42, () -> "Failure message to keep");

        assertEquals(i, 42);
        assertEquals(i, 42, "Failure message to keep");
        assertEquals(i, 42, () -> "Failure message to keep");
        assertNotEquals(i, 42);
        assertNotEquals(i, 42, "Failure message to keep");
        assertNotEquals(i, 42, () -> "Failure message to keep");
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assertions.assertEquals(c, 'a');
        Assertions.assertEquals(c, 'a', "Failure message to keep");
        Assertions.assertEquals(c, 'a', () -> "Failure message to keep");
        Assertions.assertNotEquals(c, 'a');
        Assertions.assertNotEquals(c, 'a', "Failure message to keep");
        Assertions.assertNotEquals(c, 'a', () -> "Failure message to keep");

        assertEquals(c, 'a');
        assertEquals(c, 'a', "Failure message to keep");
        assertEquals(c, 'a', () -> "Failure message to keep");
        assertNotEquals(c, 'a');
        assertNotEquals(c, 'a', "Failure message to keep");
        assertNotEquals(c, 'a', () -> "Failure message to keep");
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assertions.assertEquals(i, 1 + 2 + 3);
        Assertions.assertEquals(i, 1 + 2 + 3, "Failure message to keep");
        Assertions.assertEquals(i, 1 + 2 + 3, () -> "Failure message to keep");
        Assertions.assertNotEquals(i, 1 + 2 + 3);
        Assertions.assertNotEquals(i, 1 + 2 + 3, "Failure message to keep");
        Assertions.assertNotEquals(i, 1 + 2 + 3, () -> "Failure message to keep");

        assertEquals(i, 1 + 2 + 3);
        assertEquals(i, 1 + 2 + 3, "Failure message to keep");
        assertEquals(i, 1 + 2 + 3, () -> "Failure message to keep");
        assertNotEquals(i, 1 + 2 + 3);
        assertNotEquals(i, 1 + 2 + 3, "Failure message to keep");
        assertNotEquals(i, 1 + 2 + 3, () -> "Failure message to keep");
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) {
        Assertions.assertEquals(o, 42);
        Assertions.assertEquals(o, 42, "Failure message to keep");
        Assertions.assertEquals(o, 42, () -> "Failure message to keep");
        Assertions.assertNotEquals(o, 42);
        Assertions.assertNotEquals(o, 42, "Failure message to keep");
        Assertions.assertNotEquals(o, 42, () -> "Failure message to keep");

        assertEquals(o, 42);
        assertEquals(o, 42, "Failure message to keep");
        assertEquals(o, 42, () -> "Failure message to keep");
        assertNotEquals(o, 42);
        assertNotEquals(o, 42, "Failure message to keep");
        assertNotEquals(o, 42, () -> "Failure message to keep");
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertEquals(o, FOURTYTWO);
        Assertions.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assertions.assertEquals(o, FOURTYTWO, () -> "Failure message to keep");
        Assertions.assertNotEquals(o, FOURTYTWO);
        Assertions.assertNotEquals(o, FOURTYTWO, "Failure message to keep");
        Assertions.assertNotEquals(o, FOURTYTWO, () -> "Failure message to keep");

        assertEquals(o, FOURTYTWO);
        assertEquals(o, FOURTYTWO, "Failure message to keep");
        assertEquals(o, FOURTYTWO, () -> "Failure message to keep");
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO, "Failure message to keep");
        assertNotEquals(o, FOURTYTWO, () -> "Failure message to keep");
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) {
        Assertions.assertEquals(o, FOURTYTWO);
        Assertions.assertEquals(o, FOURTYTWO, "Failure message to keep");
        Assertions.assertEquals(o, FOURTYTWO, () -> "Failure message to keep");
        Assertions.assertNotEquals(o, FOURTYTWO);
        Assertions.assertNotEquals(o, FOURTYTWO, "Failure message to keep");
        Assertions.assertNotEquals(o, FOURTYTWO, () -> "Failure message to keep");

        assertEquals(o, FOURTYTWO);
        assertEquals(o, FOURTYTWO, "Failure message to keep");
        assertEquals(o, FOURTYTWO, () -> "Failure message to keep");
        assertNotEquals(o, FOURTYTWO);
        assertNotEquals(o, FOURTYTWO, "Failure message to keep");
        assertNotEquals(o, FOURTYTWO, () -> "Failure message to keep");
    }

    public void moveExpectedVariableAsExpectedArgWithEquals(Object o, int expected) {
        // Keep this comment
        Assertions.assertEquals(o, expected);
        Assertions.assertEquals(o, expected, "Failure message to keep");
        Assertions.assertEquals(o, expected, () -> "Failure message to keep");
        Assertions.assertNotEquals(o, expected);
        Assertions.assertNotEquals(o, expected, "Failure message to keep");
        Assertions.assertNotEquals(o, expected, () -> "Failure message to keep");

        assertEquals(o, expected);
        assertEquals(o, expected, "Failure message to keep");
        assertEquals(o, expected, () -> "Failure message to keep");
        assertNotEquals(o, expected);
        assertNotEquals(o, expected, "Failure message to keep");
        assertNotEquals(o, expected, () -> "Failure message to keep");

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        assertEquals(o, expceted);
    }

    public void refactorIfOnBoolean(boolean b) {
        // Keep this comment
        if (b) {
            Assertions.fail();
        }
        if (b) {
            Assertions.fail("Failure message to keep");
        }

        if (!b) {
            fail();
        }
        if (!b) {
            fail("Failure message to keep");
        }
    }

    public void refactorSuppliedObject(Date nullDate) {
        // Keep this comment
        if (nullDate != null) {
            Assertions.fail(() -> "The date should be null: " + nullDate.getTime());
        }

        if (nullDate != null) {
            fail(() -> "The date should be null: " + nullDate.getTime());
        }
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
        if (nullDate != null) {
            Assertions.fail("The date should be null, not like: " + notNullDate.getTime());
        }

        if (nullDate != null) {
            fail("The date should be null, not like: " + notNullDate.getTime());
        }
    }

    public void refactorIfOnExpression(boolean b1, boolean b2) {
        // Keep this comment
        if (b1 && b2) {
            Assertions.fail();
        }
        if (b1 || b2) {
            Assertions.fail("Failure message to keep");
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
        if (i1 == i2) {
            Assertions.fail();
        }
        if (i1 == i2) {
            Assertions.fail("Failure message to keep");
        }
        if (i1 == i2) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (i1 != i2) {
            Assertions.fail();
        }
        if (i1 != i2) {
            Assertions.fail("Failure message to keep");
        }
        if (i1 != i2) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (i1 == i2) {
            fail();
        }
        if (i1 == i2) {
            fail("Failure message to keep");
        }
        if (i1 == i2) {
            fail(() -> "Failure message to keep");
        }
        if (i1 != i2) {
            fail();
        }
        if (i1 != i2) {
            fail("Failure message to keep");
        }
        if (i1 != i2) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        if (o1 == o2) {
            Assertions.fail();
        }
        if (o1 == o2) {
            Assertions.fail("Failure message to keep");
        }
        if (o1 == o2) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (o1 != o2) {
            Assertions.fail();
        }
        if (o1 != o2) {
            Assertions.fail("Failure message to keep");
        }
        if (o1 != o2) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (o1 == o2) {
            fail();
        }
        if (o1 == o2) {
            fail("Failure message to keep");
        }
        if (o1 == o2) {
            fail(() -> "Failure message to keep");
        }
        if (o1 != o2) {
            fail();
        }
        if (o1 != o2) {
            fail("Failure message to keep");
        }
        if (o1 != o2) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfNullThenFail(Object o1) {
        // Keep this comment
        if (o1 == null) {
            Assertions.fail();
        }
        if (o1 == null) {
            Assertions.fail("Failure message to keep");
        }
        if (o1 == null) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (o1 != null) {
            Assertions.fail();
        }
        if (o1 != null) {
            Assertions.fail("Failure message to keep");
        }
        if (o1 != null) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (null == o1) {
            Assertions.fail();
        }
        if (null == o1) {
            Assertions.fail("Failure message to keep");
        }
        if (null == o1) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (null != o1) {
            Assertions.fail();
        }
        if (null != o1) {
            Assertions.fail("Failure message to keep");
        }
        if (null != o1) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (o1 == null) {
            fail();
        }
        if (o1 == null) {
            fail("Failure message to keep");
        }
        if (o1 == null) {
            fail(() -> "Failure message to keep");
        }
        if (o1 != null) {
            fail();
        }
        if (o1 != null) {
            fail("Failure message to keep");
        }
        if (o1 != null) {
            fail(() -> "Failure message to keep");
        }
        if (null == o1) {
            fail();
        }
        if (null == o1) {
            fail("Failure message to keep");
        }
        if (null == o1) {
            fail(() -> "Failure message to keep");
        }
        if (null != o1) {
            fail();
        }
        if (null != o1) {
            fail("Failure message to keep");
        }
        if (null != o1) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        if (o1.equals(o2)) {
            Assertions.fail();
        }
        if (o1.equals(o2)) {
            Assertions.fail("Failure message to keep");
        }
        if (o1.equals(o2)) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (!o1.equals(o2)) {
            Assertions.fail();
        }
        if (!o1.equals(o2)) {
            Assertions.fail("Failure message to keep");
        }
        if (!o1.equals(o2)) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (o1.equals(o2)) {
            fail();
        }
        if (o1.equals(o2)) {
            fail("Failure message to keep");
        }
        if (o1.equals(o2)) {
            fail(() -> "Failure message to keep");
        }
        if (!o1.equals(o2)) {
            fail();
        }
        if (!o1.equals(o2)) {
            fail("Failure message to keep");
        }
        if (!o1.equals(o2)) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfLiteralThenFail(int i) {
        // Keep this comment
        if (i == 42) {
            Assertions.fail();
        }
        if (i == 42) {
            Assertions.fail("Failure message to keep");
        }
        if (i == 42) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (i != 42) {
            Assertions.fail();
        }
        if (i != 42) {
            Assertions.fail("Failure message to keep");
        }
        if (i != 42) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (42 == i) {
            Assertions.fail();
        }
        if (42 == i) {
            Assertions.fail("Failure message to keep");
        }
        if (42 == i) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (42 != i) {
            Assertions.fail();
        }
        if (42 != i) {
            Assertions.fail("Failure message to keep");
        }
        if (42 != i) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (i == 42) {
            fail();
        }
        if (i == 42) {
            fail("Failure message to keep");
        }
        if (i == 42) {
            fail(() -> "Failure message to keep");
        }
        if (i != 42) {
            fail();
        }
        if (i != 42) {
            fail("Failure message to keep");
        }
        if (i != 42) {
            fail(() -> "Failure message to keep");
        }
        if (42 == i) {
            fail();
        }
        if (42 == i) {
            fail("Failure message to keep");
        }
        if (42 == i) {
            fail(() -> "Failure message to keep");
        }
        if (42 != i) {
            fail();
        }
        if (42 != i) {
            fail("Failure message to keep");
        }
        if (42 != i) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfConstantThenFail(int i) {
        // Keep this comment
        if (i == FOURTYTWO) {
            Assertions.fail();
        }
        if (i == FOURTYTWO) {
            Assertions.fail("Failure message to keep");
        }
        if (i == FOURTYTWO) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (i != FOURTYTWO) {
            Assertions.fail();
        }
        if (i != FOURTYTWO) {
            Assertions.fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (FOURTYTWO == i) {
            Assertions.fail();
        }
        if (FOURTYTWO == i) {
            Assertions.fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (FOURTYTWO != i) {
            Assertions.fail();
        }
        if (FOURTYTWO != i) {
            Assertions.fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (i == FOURTYTWO) {
            fail();
        }
        if (i == FOURTYTWO) {
            fail("Failure message to keep");
        }
        if (i == FOURTYTWO) {
            fail(() -> "Failure message to keep");
        }
        if (i != FOURTYTWO) {
            fail();
        }
        if (i != FOURTYTWO) {
            fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            fail(() -> "Failure message to keep");
        }
        if (FOURTYTWO == i) {
            fail();
        }
        if (FOURTYTWO == i) {
            fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            fail(() -> "Failure message to keep");
        }
        if (FOURTYTWO != i) {
            fail();
        }
        if (FOURTYTWO != i) {
            fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfExpectedThenFail(int i, int expected) {
        // Keep this comment
        if (i == expected) {
            Assertions.fail();
        }
        if (i == expected) {
            Assertions.fail("Failure message to keep");
        }
        if (i == expected) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (i != expected) {
            Assertions.fail();
        }
        if (i != expected) {
            Assertions.fail("Failure message to keep");
        }
        if (i != expected) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (expected == i) {
            Assertions.fail();
        }
        if (expected == i) {
            Assertions.fail("Failure message to keep");
        }
        if (expected == i) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (expected != i) {
            Assertions.fail();
        }
        if (expected != i) {
            Assertions.fail("Failure message to keep");
        }
        if (expected != i) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (i == expected) {
            fail();
        }
        if (i == expected) {
            fail("Failure message to keep");
        }
        if (i == expected) {
            fail(() -> "Failure message to keep");
        }
        if (i != expected) {
            fail();
        }
        if (i != expected) {
            fail("Failure message to keep");
        }
        if (i != expected) {
            fail(() -> "Failure message to keep");
        }
        if (expected == i) {
            fail();
        }
        if (expected == i) {
            fail("Failure message to keep");
        }
        if (expected == i) {
            fail(() -> "Failure message to keep");
        }
        if (expected != i) {
            fail();
        }
        if (expected != i) {
            fail("Failure message to keep");
        }
        if (expected != i) {
            fail(() -> "Failure message to keep");
        }
    }

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
        // Keep this comment
        if (i == expectedI) {
            Assertions.fail();
        }
        if (i == expectedI) {
            Assertions.fail("Failure message to keep");
        }
        if (i == expectedI) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (i != expectedI) {
            Assertions.fail();
        }
        if (i != expectedI) {
            Assertions.fail("Failure message to keep");
        }
        if (i != expectedI) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (expectedI == i) {
            Assertions.fail();
        }
        if (expectedI == i) {
            Assertions.fail("Failure message to keep");
        }
        if (expectedI == i) {
            Assertions.fail(() -> "Failure message to keep");
        }
        if (expectedI != i) {
            Assertions.fail();
        }
        if (expectedI != i) {
            Assertions.fail("Failure message to keep");
        }
        if (expectedI != i) {
            Assertions.fail(() -> "Failure message to keep");
        }

        if (i == expectedI) {
            fail();
        }
        if (i == expectedI) {
            fail("Failure message to keep");
        }
        if (i == expectedI) {
            fail(() -> "Failure message to keep");
        }
        if (i != expectedI) {
            fail();
        }
        if (i != expectedI) {
            fail("Failure message to keep");
        }
        if (i != expectedI) {
            fail(() -> "Failure message to keep");
        }
        if (expectedI == i) {
            fail();
        }
        if (expectedI == i) {
            fail("Failure message to keep");
        }
        if (expectedI == i) {
            fail(() -> "Failure message to keep");
        }
        if (expectedI != i) {
            fail();
        }
        if (expectedI != i) {
            fail("Failure message to keep");
        }
        if (expectedI != i) {
            fail(() -> "Failure message to keep");
        }
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
