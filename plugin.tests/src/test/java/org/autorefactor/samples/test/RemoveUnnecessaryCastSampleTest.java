/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.samples.test;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

@SuppressWarnings("javadoc")
public class RemoveUnnecessaryCastSampleTest {

    private org.autorefactor.samples_in.RemoveUnnecessaryCastSample sampleIn;
    private org.autorefactor.samples_out.RemoveUnnecessaryCastSample sampleOut;

    @Before
    public void setUp() {
        sampleIn = new org.autorefactor.samples_in.RemoveUnnecessaryCastSample();
        sampleOut = new org.autorefactor.samples_out.RemoveUnnecessaryCastSample();
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
    public void doNotRemovePrimitiveNarrowingCast() throws Exception {
        int pi = 0;
        assertEquals(sampleIn.doNotRemovePrimitiveNarrowingCast(pi), sampleOut.doNotRemovePrimitiveNarrowingCast(pi));
    }
}
