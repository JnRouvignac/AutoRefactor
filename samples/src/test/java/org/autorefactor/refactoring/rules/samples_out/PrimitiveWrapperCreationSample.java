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
package org.autorefactor.refactoring.rules.samples_out;

public class PrimitiveWrapperCreationSample {

    public static void replaceWrapperConstructorsWithValueOf() {
        // Replace all calls to wrapper constructors with calls to .valueOf() methods
        byte byPrimitive = 4;
        boolean boPrimitive = true;
        char cPrimitive = 'c';
        double dPrimitive = 1;
        Double dObject = Double.valueOf(1d);
        float fPrimitive = 1f;
        long lPrimitive = 1;
        short shPrimitive = 1;
        int iPrimitive = 1;

        // Primitive literals
        Byte by = Byte.valueOf((byte) 4);
        Boolean bo = Boolean.valueOf(true);
        Character c = Character.valueOf('c');
        Double d = Double.valueOf(1);
        Float f1 = Float.valueOf(1f);
        Float f2 = Float.valueOf((float) 1d);
        Long l = Long.valueOf(1);
        Short s = Short.valueOf((short) 1);
        Integer i = Integer.valueOf(1);

        // Primitive variables
        by = Byte.valueOf(byPrimitive);
        bo = Boolean.valueOf(boPrimitive);
        c = Character.valueOf(cPrimitive);
        d = Double.valueOf(dPrimitive);
        f1 = Float.valueOf(fPrimitive);
        f2 = Float.valueOf((float) dPrimitive);
        l = Long.valueOf(lPrimitive);
        s = Short.valueOf(shPrimitive);
        i = Integer.valueOf(iPrimitive);

        // Implicit object narrowing
        Float f3 = dObject.floatValue();
    }

    public static void removeUnnecessaryObjectCreation() {
        Byte.parseByte("0");
        Boolean.valueOf("true");
        Integer.parseInt("42");
        Short.parseShort("42");
        Long.parseLong("42");
        Float.parseFloat("42.42");
        Double.parseDouble("42.42");
    }

    public static void convertValueOfCallsToParseCallsInPrimitiveContext() {
        // Keep this comment
        byte by1 = Byte.parseByte("0");
        byte by2 = Byte.parseByte("0", 10);
        boolean bo = Boolean.parseBoolean("true");
        int i1 = Integer.parseInt("42");
        int i2 = Integer.parseInt("42", 10);
        long l1 = Long.parseLong("42");
        long l2 = Long.parseLong("42", 10);
        short s1 = Short.parseShort("42");
        short s2 = Short.parseShort("42", 10);
        float f = Float.parseFloat("42.42");
        double d = Double.parseDouble("42.42");
    }

    public static void removeUnnecessaryValueOfCallsInPrimitiveDeclaration() {
        // Keep this comment
        char c = '&';
        byte by = (byte) 0;
        boolean bo = true;
        int i = 42;
        long l = 42;
        short s = (short) 42;
        float f = 42.42F;
        double d = 42.42;
    }

    public static void removeUnnecessaryValueOfCallsInPrimitiveAssignment() {
        // Keep this comment
        char c;
        c = '&';
        byte by;
        by = (byte) 0;
        boolean bo1;
        bo1 = true;
        int i;
        i = 42;
        long l;
        l = 42;
        short s;
        s = (short) 42;
        float f;
        f = 42.42F;
        double d;
        d = 42.42;
    }

    public static char removeUnnecessaryValueOfCallsInCharacterPrimitive() {
        // Keep this comment
        return '&';
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
        return 42;
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

    public static void removeUnnecessaryConstructorInvocationsInPrimitiveContext() {
        // Keep this comment
        byte by = (byte) 0;
        boolean bo = true;
        int i = 42;
        long l = 42;
        short s = (short) 42;
        float f = 42.42F;
        double d = 42.42;
    }

    public static void removeUnnecessaryConstructorInvocationsInSwitch() {
        byte by = (byte) 4;
        char c = 'c';
        short s = (short) 1;
        int i = 1;

        // Keep this comment
        switch (by) {
        // Keep this comment too
        default:
        }
        switch (c) {
        default:
        }
        switch (s) {
        default:
        }
        switch (i) {
        default:
        }
    }

    public static String removeUnnecessaryConstructorInvocationsInArrayAccess(String[] strings, int i) {
        return strings[i];
    }
}
