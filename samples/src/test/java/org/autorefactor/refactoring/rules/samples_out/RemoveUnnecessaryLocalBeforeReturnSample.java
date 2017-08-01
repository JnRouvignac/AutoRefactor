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
package org.autorefactor.refactoring.rules.samples_out;

public class RemoveUnnecessaryLocalBeforeReturnSample {

    private int i;

    private double[] arrayField = new double[] { 42.42 };

    public int inlineLocalVariableDeclaration() {
        return 0;
    }

    public int inlineLocalVariableAssignment(int i) {
        return 0;
    }

    /**
     * No need to check for array variable assignments.
     * <p>
     * Trying to use it reports compile error "Array constants can only be used in initializers"
     * <p>
     */
    public String[] inlineStringArrayConstants() {
        return new String[] { "test" };
    }

    public boolean[] inlineBooleanArrayConstants() {
        return new boolean[] { true };
    }

    public char[] inlineCharArrayConstants() {
        return new char[] { 'a' };
    }

    public byte[] inlineByteArrayConstants() {
        return new byte[] { 42 };
    }

    public short[] inlineShortArrayConstants() {
        return new short[] { 42 };
    }

    public int[] inlineIntArrayConstants() {
        return new int[] { 42 };
    }

    public long[] inlineLongArrayConstants() {
        return new long[] { 42 };
    }

    public float[] inlineFloatArrayConstants() {
        return new float[] { 42.42f };
    }

    public double[] inlineDoubleArrayConstants() {
        return new double[] { 42.42 };
    }

    public double[] inlineDoubleArrayCreation() {
        return new double[] { 42.42 };
    }

    public double[] inlineDoubleArrayVariableDeclaration() {
        return arrayField;
    }

    public double[] inlineDoubleArrayAssignment() {
        double[] array = null;
        return arrayField;
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
        return new Throwable[] {};
    }

    public Throwable[] inlineExpressionWithEmptyArray(Throwable[] t) {
        return new Throwable[] {};
    }

    public char[] refactorMethodCall(String s) {
        return s.toCharArray();
    }

    public int inlineSeveralReturns(int i1, int i2) {
        if (i1 == 0) {
            return 10;
        } else {
            return 11;
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
            return 1;
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
