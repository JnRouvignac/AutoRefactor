/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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

public class AutoBoxingRatherThanExplicitMethodSample {
    public static void useAutoBoxingOnDeclaration() {
        // Keep this comment
        Character c = '*';
        Byte by = (byte) 0;
        Boolean bo = true;
        Integer i = 42;
        Long l1 = 42L;
        Long l2 = (long) 42;
        Short s = (short) 42;
        Float f = 42.42F;
        Double d = 42.42;
    }

    public static void removeUnnecessaryValueOfCallsInPrimitiveDeclaration() {
        // Keep this comment
        char c = '*';
        byte by = (byte) 0;
        boolean bo = true;
        int i = 42;
        long l1 = 42L;
        long l2 = 42;
        short s = (short) 42;
        float f = 42.42F;
        double d = 42.42;
    }

    public static void doNotUseAutoBoxingWithObjectDeclaration() {
        Object c = Character.valueOf('*');
        Object by = Byte.valueOf((byte) 0);
        Object bo = Boolean.valueOf(true);
        Object i = Integer.valueOf(42);
        Object l1 = Long.valueOf(42L);
        Object l2 = Long.valueOf(42);
        Object s = Short.valueOf((short) 42);
        Object f = Float.valueOf(42.42F);
        Object d = Double.valueOf(42.42);
    }

    public static void directlyReturnWrapperParameter(Character c, Byte by, Boolean bo, Integer i, Long l, Short s,
            Float f, Double d) {
        Object myObject = null;

        // Keep this comment
        myObject = c;
        myObject = by;
        myObject = bo;
        myObject = i;
        myObject = l;
        myObject = s;
        myObject = f;
        myObject = d;
    }

    public static void useAutoBoxingOnAssignment() {
        // Keep this comment
        Character c;
        c = '*';
        Byte by;
        by = (byte) 0;
        Boolean bo1;
        bo1 = true;
        Integer i;
        i = 42;
        Long l1;
        l1 = 42L;
        Long l2;
        l2 = (long) 42;
        Short s;
        s = (short) 42;
        Float f;
        f = 42.42F;
        Double d;
        d = 42.42;
    }

    public static void removeUnnecessaryValueOfCallsInPrimitiveAssignment() {
        // Keep this comment
        char c;
        c = '*';
        byte by;
        by = (byte) 0;
        boolean bo1;
        bo1 = true;
        int i;
        i = 42;
        long l1;
        l1 = 42L;
        long l2;
        l2 = 42;
        short s;
        s = (short) 42;
        float f;
        f = 42.42F;
        double d;
        d = 42.42;
    }

    public static void doNotUseAutoBoxingWithObjectAssignment() {
        Object c;
        c = Character.valueOf('*');
        Object by;
        by = Byte.valueOf((byte) 0);
        Object bo1;
        bo1 = Boolean.valueOf(true);
        Object i;
        i = Integer.valueOf(42);
        Object l1;
        l1 = Long.valueOf(42L);
        Object l2;
        l2 = Long.valueOf(42);
        Object s;
        s = Short.valueOf((short) 42);
        Object f;
        f = Float.valueOf(42.42F);
        Object d;
        d = Double.valueOf(42.42);
    }

    public static Character removeUnnecessaryValueOfCallsInCharacterWrapper() {
        // Keep this comment
        return '*';
    }

    public static Byte removeUnnecessaryValueOfCallsInByteWrapper() {
        // Keep this comment
        return (byte) 0;
    }

    public static Boolean removeUnnecessaryValueOfCallsInBooleanWrapper() {
        // Keep this comment
        return true;
    }

    public static Integer removeUnnecessaryValueOfCallsInIntegerWrapper() {
        // Keep this comment
        return 42;
    }

    public static Long removeUnnecessaryValueOfCallsInLongWrapper() {
        // Keep this comment
        return 42L;
    }

    public static Short removeUnnecessaryValueOfCallsInShortWrapper() {
        // Keep this comment
        return (short) 42;
    }

    public static Float removeUnnecessaryValueOfCallsInFloatWrapper() {
        // Keep this comment
        return 42.42F;
    }

    public static Double removeUnnecessaryValueOfCallsInDoubleWrapper() {
        // Keep this comment
        return 42.42;
    }

    public static char removeUnnecessaryValueOfCallsInCharacterPrimitive() {
        // Keep this comment
        return '*';
    }

    public static byte removeUnnecessaryValueOfCallsInBytePrimitive() {
        // Keep this comment
        return (byte) 0;
    }

    public static boolean removeUnnecessaryValueOfCallsInBooleanPrimitive() {
        // Keep this comment
        return true;
    }

    public static int removeUnnecessaryValueOfCallsInIntegerPrimitive() {
        // Keep this comment
        return 42;
    }

    public static long removeUnnecessaryValueOfCallsInLongPrimitive() {
        // Keep this comment
        return 42L;
    }

    public static short removeUnnecessaryValueOfCallsInShortPrimitive() {
        // Keep this comment
        return (short) 42;
    }

    public static float removeUnnecessaryValueOfCallsInFloatPrimitive() {
        // Keep this comment
        return 42.42F;
    }

    public static double removeUnnecessaryValueOfCallsInDoublePrimitive() {
        // Keep this comment
        return 42.42;
    }

    public static Object doNotUseAutoBoxingReturningObject() {
        return Character.valueOf('a');
    }
}
