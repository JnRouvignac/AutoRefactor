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
     * FIXME little bug: the return type should be String instead of java.lang.String
     */
    public String[] inlineStringArrayVariableDeclaration() {
        String[] array = { "test" };
        return array;
    }

    public boolean[] inlineBooleanArrayVariableDeclaration() {
        boolean[] array = { true };
        return array;
    }

    public char[] inlineCharArrayVariableDeclaration() {
        char[] array = { 'a' };
        return array;
    }

    public byte[] inlineByteArrayVariableDeclaration() {
        byte[] array = { 42 };
        return array;
    }

    public short[] inlineShortArrayVariableDeclaration() {
        short[] array = { 42 };
        return array;
    }

    public int[] inlineIntArrayVariableDeclaration() {
        int[] array = { 42 };
        return array;
    }

    public long[] inlineLongArrayVariableDeclaration() {
        long[] array = { 42 };
        return array;
    }

    public float[] inlineFloatArrayVariableDeclaration() {
        float[] array = { 42.42f };
        return array;
    }

    public double[] inlineDoubleArrayVariableDeclaration() {
        double[] array = { 42.42 };
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
}
