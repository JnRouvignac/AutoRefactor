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
     * FIXME little bug: the return type should be String instead of java.lang.String
     */
    public String[] inlineStringArrayVariableDeclaration() {
        return new String[] { "test" };
    }

    public boolean[] inlineBooleanArrayVariableDeclaration() {
        return new boolean[] { true };
    }

    public char[] inlineCharArrayVariableDeclaration() {
        return new char[] { 'a' };
    }

    public byte[] inlineByteArrayVariableDeclaration() {
        return new byte[] { 42 };
    }

    public short[] inlineShortArrayVariableDeclaration() {
        return new short[] { 42 };
    }

    public int[] inlineIntArrayVariableDeclaration() {
        return new int[] { 42 };
    }

    public long[] inlineLongArrayVariableDeclaration() {
        return new long[] { 42 };
    }

    public float[] inlineFloatArrayVariableDeclaration() {
        return new float[] { 42.42f };
    }

    public double[] inlineDoubleArrayVariableDeclaration() {
        return new double[] { 42.42 };
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
}
