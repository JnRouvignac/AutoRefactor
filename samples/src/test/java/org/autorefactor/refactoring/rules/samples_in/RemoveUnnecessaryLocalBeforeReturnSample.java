/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

public class RemoveUnnecessaryLocalBeforeReturnSample {

    private int i;

    private double[] arrayField = new double[] { 42.42 };

    public int inlineLocalVariableDeclaration() {
        int i = 0;
        return i;
    }

    public int inlineLocalVariableAssignment(int i) {
        i = 0;
        return i;
    }

    /**
     * No need to check for array variable assignments.
     * <p>
     * Trying to use it reports compile error "Array constants can only be used in initializers"
     * <p>
     */
    public String[] inlineStringArrayConstants() {
        String[] array = { "test" };
        return array;
    }

    public boolean[] inlineBooleanArrayConstants() {
        boolean[] array = { true };
        return array;
    }

    public char[] inlineCharArrayConstants() {
        char[] array = { 'a' };
        return array;
    }

    public byte[] inlineByteArrayConstants() {
        byte[] array = { 42 };
        return array;
    }

    public short[] inlineShortArrayConstants() {
        short[] array = { 42 };
        return array;
    }

    public int[] inlineIntArrayConstants() {
        int[] array = { 42 };
        return array;
    }

    public long[] inlineLongArrayConstants() {
        long[] array = { 42 };
        return array;
    }

    public float[] inlineFloatArrayConstants() {
        float[] array = { 42.42f };
        return array;
    }

    public double[] inlineDoubleArrayConstants() {
        double[] array = { 42.42 };
        return array;
    }

    public double[] inlineDoubleArrayCreation() {
        double[] array = new double[] { 42.42 };
        return array;
    }

    public double[] inlineDoubleArrayVariableDeclaration() {
        double[] array = arrayField;
        return array;
    }

    public double[] inlineDoubleArrayAssignment() {
        double[] array = null;
        array = arrayField;
        return array;
    }

    public int notInlineFieldAssignment1() {
        i = 0;
        return i;
    }

    public int notInlineFieldAssignment2() {
        this.i = 0;
        return i;
    }

    public Throwable[] inlineStatementWithEmptyArray() {
        Throwable[] t = {};
        return t;
    }

    public Throwable[] inlineExpressionWithEmptyArray(Throwable[] t) {
        t = new Throwable[] {};
        return t;
    }

    public char[] refactorMethodCall(String s) {
        char[] res = s.toCharArray();
        return res;
    }

    public int inlineSeveralReturns(int i1, int i2) {
        if (i1 == 0) {
            i1 = 10;
            return i1;
        } else {
            i2 = 11;
            return i2;
        }
    }

    public int doNotInlineVariableInFinally() {
        int i = 0;
        try {
            i = 1;
            return i;
        } finally {
            System.out.println(i);
        }
    }

    public int doNotInlineCatchVariableInFinally() {
        int i = 0;
        try {
            return 1;
        } catch (Exception e) {
            i = 1;
            return 2;
        } finally {
            System.out.println(i);
        }
    }

    public int inlineUnusedVariableInFinally() {
        int i = 0;
        try {
            i = 1;
            return i;
        } finally {
            System.out.println("Finished");
        }
    }

    public int doNotInlineVariableInFarAwayFinally() {
        int i = 0;
        try {
            try {
                i = 1;
                return i;
            } finally {
                System.out.println("Finished");
            }
        } finally {
            System.out.println(i);
        }
    }
}
