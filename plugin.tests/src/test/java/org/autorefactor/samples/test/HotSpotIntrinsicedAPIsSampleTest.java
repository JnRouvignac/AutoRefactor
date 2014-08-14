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

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class HotSpotIntrinsicedAPIsSampleTest {

    public org.autorefactor.samples_in.HotSpotIntrinsicedAPIsSample sampleIn;
    public org.autorefactor.samples_out.HotSpotIntrinsicedAPIsSample sampleOut;
    private int[] src;
    private int[] dest1;
    private int[] dest2;

    @Before
    public void setup() {
        sampleIn = new org.autorefactor.samples_in.HotSpotIntrinsicedAPIsSample();
        sampleOut = new org.autorefactor.samples_out.HotSpotIntrinsicedAPIsSample();
        src = new int[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
        dest1 = new int[src.length];
        dest2 = new int[src.length];
    }

    @After
    public void teardown() {
        sampleIn = null;
        sampleOut = null;
        src = dest1 = dest2 = null;
    }

    @Test
    public void replaceBySystemArrayCopyBasic() {
        sampleIn.replaceBySystemArrayCopyBasic(src, dest1);
        sampleOut.replaceBySystemArrayCopyBasic(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyComplexUpperBound() {
        sampleIn.replaceBySystemArrayCopyComplexUpperBound(src, dest1);
        sampleOut.replaceBySystemArrayCopyComplexUpperBound(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyAssignIndexVariable() {
        sampleIn.replaceBySystemArrayCopyAssignIndexVariable(src, dest1, 1);
        sampleOut.replaceBySystemArrayCopyAssignIndexVariable(src, dest2, 1);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyWithSrcPos() {
        sampleIn.replaceBySystemArrayCopyWithSrcPos(src, dest1);
        sampleOut.replaceBySystemArrayCopyWithSrcPos(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyWithDestPos() {
        sampleIn.replaceBySystemArrayCopyWithDestPos(src, dest1);
        sampleOut.replaceBySystemArrayCopyWithDestPos(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyWithLowerBound() {
        sampleIn.replaceBySystemArrayCopyWithLowerBound(src, dest1);
        sampleOut.replaceBySystemArrayCopyWithLowerBound(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyRevertedCondition() {
        sampleIn.replaceBySystemArrayCopyRevertedCondition(src, dest1);
        sampleOut.replaceBySystemArrayCopyRevertedCondition(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

    @Test
    public void replaceBySystemArrayCopyRevertedCondition2() {
        sampleIn.replaceBySystemArrayCopyRevertedCondition2(src, dest1);
        sampleOut.replaceBySystemArrayCopyRevertedCondition2(src, dest2);
        assertArrayEquals(dest1, dest2);
    }

}
