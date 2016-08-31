/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules.samples.test;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

@SuppressWarnings("javadoc")
public class RemoveUnnecessaryCastSampleTest {

    private org.autorefactor.refactoring.rules.samples_in.RemoveUnnecessaryCastSample sampleIn;
    private org.autorefactor.refactoring.rules.samples_out.RemoveUnnecessaryCastSample sampleOut;

    @Before
    public void setUp() {
        sampleIn = new org.autorefactor.refactoring.rules.samples_in.RemoveUnnecessaryCastSample();
        sampleOut = new org.autorefactor.refactoring.rules.samples_out.RemoveUnnecessaryCastSample();
    }

    @Test
    public void removeCasts() throws Exception {
        Integer oi = 1;
        int pi = 2;
        long l = 3;
        assertEquals(sampleIn.removeCasts(oi, pi, l), sampleOut.removeCasts(oi, pi, l));
    }

    @Test
    public void removeCastToIntegerWrapper() throws Exception {
        int pi = 0;
        assertEquals(sampleIn.removeCastToIntegerWrapper(pi), sampleOut.removeCastToIntegerWrapper(pi));
    }

    @Test
    public void removeCastToSameType() throws Exception {
        Integer oi = 0;
        assertEquals(sampleIn.removeCastToSameType(oi), sampleOut.removeCastToSameType(oi));
    }

    @Test
    public void doNotRemoveNarrowingCast1() throws Exception {
        Integer oi = 0;
        assertEquals(sampleIn.doNotRemoveNarrowingCast1(oi), sampleOut.doNotRemoveNarrowingCast1(oi));
    }

    @Test
    public void doNotRemoveNarrowingCast2() throws Exception {
        Integer oi = 0;
        assertEquals(sampleIn.doNotRemoveNarrowingCast2(oi), sampleOut.doNotRemoveNarrowingCast2(oi));
    }

    @Test
    public void removeWideningPrimitiveCast() throws Exception {
        int pi = 0;
        assertEquals(sampleIn.removeWideningPrimitiveCast(pi), sampleOut.removeWideningPrimitiveCast(pi));
    }

    @Test
    public void doNotRemoveWideningCast() {
        int i = Integer.MAX_VALUE;
        int j = Integer.MAX_VALUE;
        assertEquals(sampleIn.doNotRemoveWideningCast(i, j), sampleOut.doNotRemoveWideningCast(i, j));
    }

    @Test
    public void doNotRemoveWideningCastsWithExtendedOperands() {
        int i = Integer.MAX_VALUE;
        int j = Integer.MAX_VALUE;
        int k = Integer.MAX_VALUE;
        assertEquals(sampleIn.doNotRemoveWideningCastsWithExtendedOperands(i, j, k), sampleOut.doNotRemoveWideningCastsWithExtendedOperands(i, j, k));
    }

    @Test
    public void doNotRemovePrimitiveNarrowingCast() throws Exception {
        int pi = 0;
        assertEquals(sampleIn.doNotRemovePrimitiveNarrowingCast(pi), sampleOut.doNotRemovePrimitiveNarrowingCast(pi));
    }

    @Test
    public void doNotRemoveCasts() throws Exception {
        Integer oi = 1;
        int pi = 2;
        long l = 3;
        Object o = 4;
        assertEquals(sampleIn.doNotRemoveCasts(oi, pi, l, o), sampleOut.doNotRemoveCasts(oi, pi, l, o));
    }

    @Test
    public void doNotRemovePrimitiveNarrowingCastsWithComparison() throws Exception {
        assertDoNotRemovePrimitiveNarrowingCastsWithComparison(0);
        assertDoNotRemovePrimitiveNarrowingCastsWithComparison(1);
        final int i = 2 << 8;
        assertEquals((byte) 0, (byte) i);
        assertDoNotRemovePrimitiveNarrowingCastsWithComparison(i);
    }

    private void assertDoNotRemovePrimitiveNarrowingCastsWithComparison(int i) {
        assertEquals(sampleIn.doNotRemovePrimitiveNarrowingCastsWithComparison(i),
                sampleOut.doNotRemovePrimitiveNarrowingCastsWithComparison(i));
    }

    @Test
    public void doNotRemovePrimitiveNarrowingCasts() {
        assertDoNotRemovePrimitiveNarrowingCasts(1, 1);
        assertDoNotRemovePrimitiveNarrowingCasts(256, 1);
    }

    private void assertDoNotRemovePrimitiveNarrowingCasts(int i, int j) {
        assertEquals(sampleIn.doNotRemovePrimitiveNarrowingCasts(i, j),
                sampleOut.doNotRemovePrimitiveNarrowingCasts(i, j));
    }

    @Test
    public void removeCastsFromStringAppend() throws Exception {
        Integer oi = 1;
        int pi = 2;
        Object o = "";
        assertEquals(sampleIn.removeCastsFromStringAppend(oi, pi, o), sampleOut.removeCastsFromStringAppend(oi, pi, o));
    }

    @Test
    public void doNotRemoveCastsFromStringAppend() throws Exception {
        int pi = 1;
        long l = 2;
        float f = 3;
        assertEquals(sampleIn.doNotRemoveCastsFromStringAppend(pi, l, f),
                sampleOut.doNotRemoveCastsFromStringAppend(pi, l, f));
    }

    @Test
    public void removeSomeCastsFromDivisionWithDifferentTypes() throws Exception {
        int i = 1;
        long l = 2;
        assertEquals(sampleIn.removeSomeCastsFromDivisionWithDifferentTypes(i, l),
                sampleOut.removeSomeCastsFromDivisionWithDifferentTypes(i, l), 0.0);
    }

    @Test
    public void removeSomeCastsFromDivisionWithDifferentTypesWithExtendedOperands() throws Exception {
        int i = 1;
        long l = 2;
        short s = 3;
        assertEquals(sampleIn.removeSomeCastsFromDivisionWithDifferentTypesWithExtendedOperands(i, l, s),
                sampleOut.removeSomeCastsFromDivisionWithDifferentTypesWithExtendedOperands(i, l, s), 0.0);
    }

    @Test
    public void doNotRemoveCastsFromAdditionWhenResultIsWidened() {
        Integer oi = Integer.MAX_VALUE;
        int pi = Integer.MAX_VALUE;
        long l = Long.MAX_VALUE;
        assertEquals(sampleIn.doNotRemoveCastsFromAdditionWhenResultIsWidened(oi, pi, l),
                sampleOut.doNotRemoveCastsFromAdditionWhenResultIsWidened(oi, pi, l), 0.0);
    }

    @Test
    public void doNotRemoveCastsFromSoustractionWhenResultIsWidened() {
        Integer oi = Integer.MIN_VALUE;
        int pi = Integer.MAX_VALUE;
        long l = Long.MIN_VALUE;
        assertEquals(sampleIn.doNotRemoveCastsFromSoustractionWhenResultIsWidened(oi, pi, l),
                sampleOut.doNotRemoveCastsFromSoustractionWhenResultIsWidened(oi, pi, l), 0.0);
    }
}
