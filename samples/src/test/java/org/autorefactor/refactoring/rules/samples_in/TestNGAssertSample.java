/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

import static org.testng.Assert.*;

import org.testng.Assert;

public class TestNGAssertSample {

    public void shouldRefactorWithPrimitives(int i1, int i2) throws Exception {
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
        Assert.assertTrue(false);
        Assert.assertTrue(false, "Failure message to keep");
        Assert.assertFalse(true);
        Assert.assertFalse(true, "Failure message to keep");

        assertTrue(false);
        assertTrue(false, "Failure message to keep");
        assertFalse(true);
        assertFalse(true, "Failure message to keep");
    }

    public void shouldRefactorWithObjectReferences(Object o1, Object o2) throws Exception {
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
}
