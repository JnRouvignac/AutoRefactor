/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.Arrays;

public class FillRatherThanLoopSample {
    private boolean[] booleanArray = new boolean[10];

    public boolean[] refactorBooleanArray() {
        boolean[] array = new boolean[10];

        // Keep this comment
        Arrays.fill(array, true);
        // Keep this comment too
        Arrays.fill(array, false);

        return array;
    }

    public int[] refactorNumberArray() {
        int[] array = new int[10];

        // Keep this comment
        Arrays.fill(array, 123);
        // Keep this comment too
        Arrays.fill(array, Integer.MAX_VALUE);

        return array;
    }

    public char[] refactorCharacterArray() {
        char[] array = new char[10];

        // Keep this comment
        Arrays.fill(array, '!');
        // Keep this comment too
        Arrays.fill(array, '\\');

        return array;
    }

    public String[] refactorStringArray() {
        String[] array = new String[10];

        // Keep this comment
        Arrays.fill(array, "foo");
        // Keep this comment too
        Arrays.fill(array, null);

        return array;
    }

    public void refactorExternalArray() {
        // Keep this comment
        Arrays.fill(booleanArray, true);
        // Keep this comment too
        Arrays.fill(booleanArray, false);
    }

    public boolean[] doNotReplaceNonForEachLoop() {
        boolean[] array = new boolean[10];

        for (int i = 1; i < array.length; i++) {
            array[i] = true;
        }
        for (int i = 0; i < array.length - 1; i++) {
            array[i] = false;
        }

        return array;
    }

    public int[] doNotRefactorInitWithoutConstant(int j) {
        int[] array = new int[10];

        for (int i = 0; i < array.length; i++) {
            array[i] = i*i;
        }
        for (int i = array.length - 1; i >= 0; i--) {
            array[i] = j++;
        }

        return array;
    }

    public int[] doNotRefactorWithAnotherStatement() {
        int[] array = new int[10];

        for (int i = 0; i < array.length; i++) {
            array[i] = 123;
            System.out.println("Do not forget me!");
        }
        for (int i = array.length - 1; i >= 0; i--) {
            System.out.println("Do not forget me!");
            array[i] = 123;
        }

        return array;
    }

    public int[] doNotRefactorWithSpecificIndex() {
        int[] array = new int[10];

        for (int i = 0; i < array.length; i++) {
            array[0] = 123;
        }
        for (int i = 0; i < array.length; i++) {
            array[array.length - i] = 123;
        }

        return array;
    }

    public int[] doNotRefactorAnotherArray(int[] array3) {
        int[] array = new int[10];
        int[] array2 = new int[10];

        for (int i = 0; i < array.length; i++) {
            array2[i] = 123;
        }
        for (int i = 0; i < array.length; i++) {
            array3[i] = 123;
        }

        return array;
    }
}
