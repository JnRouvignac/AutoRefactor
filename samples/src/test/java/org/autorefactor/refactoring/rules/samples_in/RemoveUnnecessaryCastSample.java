/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2018 Fabrice Tiercelin - Adds 'L', 'f' or 'd' to type literals.
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

public class RemoveUnnecessaryCastSample {

    private static final int ONE = 1;
    private byte b;
    private char c;
    private short s;

    private Long usual = (long) 101;
    private Long octal = (long) 0121;
    private Long hex = (long) 0xdafdaf;
    private Long binary = (long) 0b1110010111;
    private Float usualFloat = (float) 101;
    private Double usualDouble = (double) 101;
    private Double floatingDouble = (double) 101.01;

    private Long doNotChangeLong = 101L;
    private Float doNotChangeFloat = 101f;
    private Double doNotChangeDouble = 101d;

    public void refactorIt() {
        Long localUsual = (long) 101;
        Long localOctal = (long) 0121;
        Long localHex = (long) 0xdafdaf;
        Long localBinary = (long) 0b1110010111;
        Float localUsualFloat = (float) 101;
        Double localUsualDouble = (double) 101;
        Double localFloatingDouble = (double) 101.01;
    }

    public void doNotRefactor() {
        Long doNotChangeLocalLong = 101L;
        Float doNotChangeLocalFloat = 101f;
        Double doNotChangeLocalDouble = 101d;
    }

    public Object removeCastToSameType(Integer i) {
        // Keep this comment
        return (Integer) i;
    }

    public Object removeCastToIntegerWrapper(int i) {
        // Keep this comment
        return (Integer) i;
    }

    public Integer doNotRemoveNarrowingCast1(Object o) {
        return (Integer) o;
    }

    public int doNotRemoveNarrowingCast2(Object o) {
        return (Integer) o;
    }

    public long removeWideningPrimitiveCast(int i) {
        // Keep this comment
        return (long) i;
    }

    public Object[] doNotRemoveCastToArray(Object o) {
        Object array[] = (Object[]) o;
        return array;
    }

    public int doNotRemovePrimitiveNarrowingCast(long l) {
        return (int) l;
    }

    public void removeAssignmentCasts(Integer oi, int pi, Object o) {
        Object o1;
        long l;
        int i;
        // Keep this comment
        o1 = (Integer) oi;
        o1 = (Integer) pi;
        l = (long) pi;
        i = (int) pi;
        l += (long) pi;
        i += (int) pi;
    }

    public void doNotRemoveAssignmentCasts(Object o, long l) {
        Integer oi;
        int i;
        oi = (Integer) o;
        i = (Integer) o;
        i = (int) l;
        i += (int) l;
    }

    public void removeVariableDeclarationFragmentCasts(Integer oi, int pi, Object o) {
        // Keep this comment
        Object o1 = (Integer) oi;
        Object o2 = (Integer) pi;
        long l = (long) pi;
    }

    public void doNotRemoveVariableDeclarationFragmentCasts(Object o, long l) {
        Integer oi1 = (Integer) o;
        int pi2 = (Integer) o;
        int pi3 = (int) l;
    }

    public boolean removeCasts(Integer oi, int pi, long l) {
        // Keep this comment
        return oi != (Integer) pi
            && (Integer) pi != oi
            && (int) oi != pi
            && pi != (int) oi
            && l != (long) pi
            && (long) pi != l;
    }

    public boolean doNotRemoveCasts(Integer oi, int pi, long l, Object o) {
        return (int) l != pi
                && pi != (int) l
                && (int) o != pi
                && pi != (int) o
                && (int) o != oi
                && oi != (int) o
                && (Integer) o != pi
                && pi != (Integer) o
                && (Integer) o != oi
                && oi != (Integer) o;
    }

    public boolean doNotRemovePrimitiveNarrowingCastsWithComparison(int i) {
        return (byte) i == 0
            && 0 == (byte) i
            && (char) i == 0
            && 0 == (char) i
            && (short) i == 0
            && 0 == (short) i;
    }

    public long doNotRemoveWideningCast(int i, int j) {
        return (long) i + j;
    }

    public long doNotRemoveWideningCastsWithExtendedOperands(int i, int j, int k) {
        // Keep this comment
        return (long) i + j + (long) k;
    }

    public int doNotRemovePrimitiveNarrowingCasts(int i, int j) {
        return (byte) i + j;
    }

    public int doNotRemovePrimitiveNarrowingCastsWithExtendedOperands(int i, int j) {
        return (byte) i + j + (byte) i;
    }

    public String removeCastsFromStringAppend(Integer oi, int pi, Object o) {
        // Keep this comment
        return "" + (Object) oi + (Number) oi + (int) oi + (Integer) pi + (long) pi + (String) o;
    }

    public String doNotRemoveCastsFromStringAppend(int pi, long l, float f) {
        return "" + (byte) pi + (int) f;
    }

    public double removeSomeCastsFromDivisionWithDifferentTypes(int i, long l) {
        // Keep this comment
        return (double) i / (double) l;
    }

    public double removeSomeCastsFromDivisionWithDifferentTypesWithExtendedOperands(int i, long l, short s) {
        // Keep this comment
        return (double) i / (double) l / (double) s;
    }

    public double doNotRemoveCastsFromAdditionWhenResultIsWidened(Integer oi, int pi, long l) {
        // Keep this comment
        return (long) oi + (Integer) pi + (double) l;
    }

    public double doNotRemoveCastsFromSoustractionWhenResultIsWidened(Integer oi, int pi, long l) {
        // Keep this comment
        return (long) oi - (Integer) pi - (double) l;
    }

    public boolean removeCastsFromBooleanOperations(boolean b1, boolean b2) {
        // Keep this comment
        return b1 && (Boolean) b2;
    }

    public double doNotRemoveCastInFloatingPointDivision(int i, int j) {
        return i / (double) j;
    }

    public double removeOneCastInFloatingPointDivision(int i, int j, int k) {
        // Keep this comment
        return i / (double) j / (double) k;
    }

    public long removeCastsInIntegralDivision(int i, int j, int k) {
        // Keep this comment
        return i / (long) j / (long) k;
    }

    public int removeCastsFromAssignmentsToByte() {
        // Keep this comment
        byte b1 = (byte) -1;
        byte b2 = (byte) 127;
        byte b3 = (byte) -128;
        byte b4 = (byte) ONE;
        Byte b5 = (byte) -1;
        Byte b6 = (byte) 127;
        Byte b7 = (byte) -128;
        Byte b8 = (byte) ONE;
        b = (byte) 1;
        return b1 + b2 + b3 + b4 + b5 + b6 + b7 + b8 + b;
    }

    public int removeCastsFromAssignmentsToChar() {
        // Keep this comment
        char c1 = (char) 0;
        char c2 = (char) 65535;
        char c3 = (char) ONE;
        Character c4 = (char) 0;
        Character c5 = (char) 65535;
        Character c6 = (char) ONE;
        c = (char) 1;
        return c1 + c2 + c3 + c4 + c5 + c6 + c;
    }

    public int removeCastsFromAssignmentsToShort() {
        // Keep this comment
        short s1 = (short) -1;
        short s2 = (short) 32767;
        short s3 = (short) -32768;
        short s4 = (short) ONE;
        Short s5 = (short) -1;
        Short s6 = (short) 32767;
        Short s7 = (short) -32768;
        Short s8 = (short) ONE;
        s = (short) 1;
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s;
    }

    public int doNotRemoveCastsFromPrimitiveAssignments() {
        byte b1 = (byte) 128;
        byte b2 = (byte) -129;
        char c1 = (char) -1;
        char c2 = (char) 65536;
        short s1 = (short) 32768;
        short s2 = (short) -32769;
        return b1 + b2 + c1 + c2 + s1 + s2;
    }
}
