/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - #199 Replace unnecessary Boolean constant on boolean assignment
 *                                        #200 Compile error when Float myFloat = new Float(doubleObject);
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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

public class ParsingRatherThanValueOfSample {
    public static void convertValueOfCallsToParseCallsInPrimitiveContext() {
        // Keep this comment
        byte by1 = Byte.valueOf("0");
        byte by2 = Byte.valueOf("0", 10);
        boolean bo = Boolean.valueOf("true");
        int i1 = Integer.valueOf("42");
        int i2 = Integer.valueOf("42", 10);
        long l1 = Long.valueOf("42");
        long l2 = Long.valueOf("42", 10);
        short s1 = Short.valueOf("42");
        short s2 = Short.valueOf("42", 10);
        float f = Float.valueOf("42.42");
        double d = Double.valueOf("42.42");
    }

    public static void removeUnnecessaryValueOfCallsInPrimitiveDeclaration() {
        // Keep this comment
        char c = Character.valueOf('&');
        byte by = Byte.valueOf((byte) 0);
        boolean bo = Boolean.valueOf(true);
        int i = Integer.valueOf(42);
        long l = Long.valueOf(42);
        short s = Short.valueOf((short) 42);
        float f = Float.valueOf(42.42F);
        double d = Double.valueOf(42.42);
    }

    public static void removeUnnecessaryValueOfCallsInPrimitiveAssignment() {
        // Keep this comment
        char c;
        c = Character.valueOf('&');
        byte by;
        by = Byte.valueOf((byte) 0);
        boolean bo1;
        bo1 = Boolean.valueOf(true);
        int i;
        i = Integer.valueOf(42);
        long l;
        l = Long.valueOf(42);
        short s;
        s = Short.valueOf((short) 42);
        float f;
        f = Float.valueOf(42.42F);
        double d;
        d = Double.valueOf(42.42);
    }

    public static char removeUnnecessaryValueOfCallsInCharacterPrimitive() {
        // Keep this comment
        return Character.valueOf('&');
    }

    public static byte removeUnnecessaryValueOfCallsInBytePrimitive() {
        // Keep this comment
        return Byte.valueOf((byte) 0);
    }

    public static boolean removeUnnecessaryValueOfCallsInBooleanPrimitive() {
        // Keep this comment
        return Boolean.valueOf(true);
    }

    public static int removeUnnecessaryValueOfCallsInIntegerPrimitive() {
        // Keep this comment
        return Integer.valueOf(42);
    }

    public static long removeUnnecessaryValueOfCallsInLongPrimitive() {
        // Keep this comment
        return Long.valueOf(42);
    }

    public static short removeUnnecessaryValueOfCallsInShortPrimitive() {
        // Keep this comment
        return Short.valueOf((short) 42);
    }

    public static float removeUnnecessaryValueOfCallsInFloatPrimitive() {
        // Keep this comment
        return Float.valueOf(42.42F);
    }

    public static double removeUnnecessaryValueOfCallsInDoublePrimitive() {
        // Keep this comment
        return Double.valueOf(42.42);
    }

    public static void removeUnnecessaryObjectCreation() {
        // Keep this comment
        new Byte("0").byteValue();
        new Boolean("true").booleanValue();
        new Integer("42").intValue();
        new Short("42").shortValue();
        new Long("42").longValue();
        new Float("42.42").floatValue();
        new Double("42.42").doubleValue();
    }

    public static void removeUnnecessaryValueOfCalls() {
        // Keep this comment
        Byte.valueOf("0").byteValue();
        Boolean.valueOf("true").booleanValue();
        Integer.valueOf("42").intValue();
        Short.valueOf("42").shortValue();
        Long.valueOf("42").longValue();
        Float.valueOf("42.42").floatValue();
        Double.valueOf("42.42").doubleValue();
    }
}
