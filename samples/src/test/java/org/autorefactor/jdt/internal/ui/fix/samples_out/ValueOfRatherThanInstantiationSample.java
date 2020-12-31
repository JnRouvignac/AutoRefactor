/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2021 Fabrice Tiercelin - #199 Replace unnecessary Boolean constant on boolean assignment
 *                                             #200 Compile error when Float myFloat = new Float(doubleObject);
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

public class ValueOfRatherThanInstantiationSample {
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
        // Keep this comment
        Byte.valueOf("0").byteValue();
        Boolean.valueOf("true").booleanValue();
        Integer.valueOf("42").intValue();
        Short.valueOf("42").shortValue();
        Long.valueOf("42").longValue();
        Float.valueOf("42.42").floatValue();
        Double.valueOf("42.42").doubleValue();
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
