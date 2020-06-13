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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import static org.junit.Assert.*;

import org.junit.Assert;

public class SeparateAssertionsRatherThanBooleanExpressionSample {
    public void refactorInfixExpression(boolean b1, boolean b2) {
        // Keep this comment
        Assert.assertTrue(b1 && b2);
        Assert.assertTrue("Either b1 or b2 is wrong, I don't know", b1 && b2);
        Assert.assertFalse(b1 || b2);
        Assert.assertFalse("Either b1 or b2 is wrong, I don't know", b1 || b2);

        // Keep this comment too
        assertTrue(b1 && b2);
        assertTrue("Either b1 or b2 is wrong, I don't know", b1 && b2);
        assertFalse(b1 || b2);
        assertFalse("Either b1 or b2 is wrong, I don't know", b1 || b2);
    }

    public void doNotRefactorEagerOperator(boolean b1, boolean b2) {
        Assert.assertTrue(b1 & b2);
        Assert.assertTrue("Either b1 or b2 is wrong, I don't know", b1 & b2);
        Assert.assertFalse(b1 | b2);
        Assert.assertFalse("Either b1 or b2 is wrong, I don't know", b1 | b2);

        assertTrue(b1 & b2);
        assertTrue("Either b1 or b2 is wrong, I don't know", b1 & b2);
        assertFalse(b1 | b2);
        assertFalse("Either b1 or b2 is wrong, I don't know", b1 | b2);
    }

    public void refactorFormerJUnit(boolean b1, boolean b2) {
        // Keep this comment
        junit.framework.Assert.assertTrue(b1 && b2);
        junit.framework.Assert.assertTrue("Either b1 or b2 is wrong, I don't know", b1 && b2);
        junit.framework.Assert.assertFalse(b1 || b2);
        junit.framework.Assert.assertFalse("Either b1 or b2 is wrong, I don't know", b1 || b2);
    }

    public void refactorTestNg(boolean b1, boolean b2) {
        // Keep this comment
        org.testng.Assert.assertTrue(b1 && b2);
        org.testng.Assert.assertTrue(b1 && b2, "Either b1 or b2 is wrong, I don't know");
        org.testng.Assert.assertFalse(b1 || b2);
        org.testng.Assert.assertFalse(b1 || b2, "Either b1 or b2 is wrong, I don't know");
    }

    public void refactorJupiter(boolean b1, boolean b2) {
        // Keep this comment
        org.junit.jupiter.api.Assertions.assertTrue(b1 && b2);
        org.junit.jupiter.api.Assertions.assertTrue(b1 && b2, "Either b1 or b2 is wrong, I don't know");
        org.junit.jupiter.api.Assertions.assertFalse(b1 || b2);
        org.junit.jupiter.api.Assertions.assertFalse(b1 || b2, "Either b1 or b2 is wrong, I don't know");
    }

    public void doNotRefactorOppositeInfixExpression(boolean b1, boolean b2) {
        Assert.assertTrue(b1 || b2);
        Assert.assertTrue("Either b1 or b2 is wrong, I don't know", b1 || b2);
        Assert.assertFalse(b1 && b2);
        Assert.assertFalse("Either b1 or b2 is wrong, I don't know", b1 && b2);

        assertTrue(b1 || b2);
        assertTrue("Either b1 or b2 is wrong, I don't know", b1 || b2);
        assertFalse(b1 && b2);
        assertFalse("Either b1 or b2 is wrong, I don't know", b1 && b2);
    }

    public void doNotRefactorXOR(boolean b1, boolean b2) {
        Assert.assertTrue(b1 ^ b2);
        Assert.assertTrue("Either b1 or b2 is wrong, I don't know", b1 ^ b2);
        Assert.assertFalse(b1 ^ b2);
        Assert.assertFalse("Either b1 or b2 is wrong, I don't know", b1 ^ b2);

        assertTrue(b1 ^ b2);
        assertTrue("Either b1 or b2 is wrong, I don't know", b1 ^ b2);
        assertFalse(b1 ^ b2);
        assertFalse("Either b1 or b2 is wrong, I don't know", b1 ^ b2);
    }

    public void doNotRefactorField(boolean b) {
        Assert.assertTrue(b);
        Assert.assertTrue("Either b1 or b2 is wrong, I don't know", b);
        Assert.assertFalse(b);
        Assert.assertFalse("Either b1 or b2 is wrong, I don't know", b);

        assertTrue(b);
        assertTrue("Either b1 or b2 is wrong, I don't know", b);
        assertFalse(b);
        assertFalse("Either b1 or b2 is wrong, I don't know", b);
    }

    public void refactorExtendedExpression(boolean b1, boolean b2, boolean b3) {
        // Keep this comment
        Assert.assertTrue(b1 && b2 && b3);
        Assert.assertTrue("Either b1, b2 or b3 is wrong, I don't know", b1 && b2 && b3);
        Assert.assertFalse(b1 || b2 || b3);
        Assert.assertFalse("Either b1, b2 or b3 is wrong, I don't know", b1 || b2 || b3);

        // Keep this comment too
        assertTrue(b1 && b2 && b3);
        assertTrue("Either b1, b2 or b3 is wrong, I don't know", b1 && b2 && b3);
        assertFalse(b1 || b2 || b3);
        assertFalse("Either b1, b2 or b3 is wrong, I don't know", b1 || b2 || b3);
    }

    public void refactorLoneStatement(boolean b1, boolean b2, boolean isEnabled) {
        if (isEnabled)
            assertTrue(b1 && b2);
    }

    public void refactorStatementInBlock(boolean b1, boolean b2, boolean isEnabled) {
        if (isEnabled) {
            // Keep this comment
            Assert.assertTrue(b1 && b2);
        }
    }

    public void refactorComplexExpression(int i) {
        // Keep this comment
        Assert.assertTrue(0 < i && i < 100);
        Assert.assertTrue("Interval", 0 < i && i < 100);
        Assert.assertFalse(i < 0 || 100 < i);
        Assert.assertFalse("Interval", i < 0 || 100 < i);

        // Keep this comment too
        assertTrue(0 < i && i < 100);
        assertTrue("Interval", 0 < i && i < 100);
        assertFalse(i < 0 || 100 < i);
        assertFalse("Interval", i < 0 || 100 < i);
    }
}
