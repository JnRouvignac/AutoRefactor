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
package org.autorefactor.refactoring.rules.samples_in;

public class HotSpotIntrinsicedAPIsSample {

    public static void replaceBySystemArrayCopyBasic(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 0; i < 3; i++) {
            dest[i] = src[i];
        }
    }

    public static void doNotRefactorWrongOperator(int[] a1, int[] a2) {
        for (int i = 0; i < 3; i++) {
            a2[i] += a1[i];
        }
    }

    public static void doNotReplaceBySystemArrayCopyWhenTypeDiffers(byte[] src, int[] dest) {
        for (int i = 0; i < 3; i++) {
            dest[i] = src[i];
        }
    }

    public static void replaceBySystemArrayCopyComplexUpperBound(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 0; i < src.length; i++) {
            dest[i] = src[i];
        }
    }

    public static void replaceBySystemArrayCopyAssignIndexVariable(int[] src, int[] dest, int i) {
        // Keep this comment
        for (i = 0; i < 3; i++) {
            dest[i] = src[i];
        }
    }

    public static void replaceBySystemArrayCopyWithConstantSrcPos(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 0; i < src.length - 1; i++) {
            dest[i] = src[i + 1];
        }
    }

    public static void replaceBySystemArrayCopyWithArgumentSrcPos(int[] src, int startPos, int[] dest) {
        // Keep this comment
        for (int j = 0; j < dest.length; j++) {
            dest[j] = src[startPos + j];
        }
    }

    public static void replaceBySystemArrayCopyWithDestPos(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 0; i < src.length - 1; i++) {
            dest[i + 1] = src[i];
        }
    }

    public static void replaceBySystemArrayCopyWithLowerBound(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 1; i <= 3; i++) {
            dest[i + 2] = src[i + 1];
        }
    }

    public static int[] replaceBySystemArrayCopyWithOffset(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 2; i < 7; i++) {
            dest[i] = src[i];
        }
        return dest;
    }

    public static int[] replaceBySystemArrayCopyWithBoundaryVariable(int[] src, int[] dest, int boundary) {
        // Keep this comment
        for (int i = 2; i < boundary; i++) {
            dest[i] = src[i];
        }
        return dest;
    }

    public static int[] replaceBySystemArrayCopyWithOffsetAndBoundaryVariable(int[] src, int[] dest, int offset,
            int boundary) {
        // Keep this comment
        for (int i = offset; i < boundary; i++) {
            dest[i] = src[i];
        }
        return dest;
    }

    public static void replaceBySystemArrayCopyRevertedCondition(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 0; 3 > i; i++) {
            dest[i] = src[i];
        }
    }

    public static void replaceBySystemArrayCopyRevertedCondition2(int[] src, int[] dest) {
        // Keep this comment
        for (int i = 0; 3 >= i; i++) {
            dest[i] = src[i];
        }
    }

    public static void replaceWithArraysCopyOf(int[] src, int[] dest) {
        // FIXME Should use java.util.Arrays.copyOf()
    }

    public static void replaceWithArraysCopyOfRange(int[] src, int[] dest) {
        // FIXME Should use java.util.Arrays.copyOfRange()
    }

    public static boolean replaceWithArraysEquals(int[] src, int[] dest) {
        if (dest.length != src.length) {
            return false;
        }
        for (int i = 0; i < src.length; i++) {
            if (dest[i] != src[i]) {
                return false;
            }
        }
        return true;
        // FIXME Should use java.util.Arrays.equals()
    }

    public static boolean replaceWithArraysEquals2(int[] src, int[] dest) {
        if (dest.length == src.length) {
            for (int i = 0; i < src.length; i++) {
                if (dest[i] != src[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
        // FIXME Should use java.util.Arrays.equals()
    }

    public static void replaceWithStringIndexOf(String s) {
        // FIXME Should use java.lang.String.indexOf()
    }
}
