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
package org.autorefactor.refactoring.rules.samples_in;

public class ObsoleteValueOfRatherThanInstantiationSample {
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
        Byte by = new Byte((byte) 4);
        Boolean bo = new Boolean(true);
        Character c = new Character('c');
        Double d = new Double(1);
        Float f1 = new Float(1f);
        Float f2 = new Float(1d);
        Long l = new Long(1);
        Short s = new Short((short) 1);
        Integer i = new Integer(1);

        // Primitive variables
        by = new Byte(byPrimitive);
        bo = new Boolean(boPrimitive);
        c = new Character(cPrimitive);
        d = new Double(dPrimitive);
        f1 = new Float(fPrimitive);
        f2 = new Float(dPrimitive);
        l = new Long(lPrimitive);
        s = new Short(shPrimitive);
        i = new Integer(iPrimitive);

        // Implicit object narrowing
        Float f3 = new Float(dObject);
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

    public static void removeUnnecessaryConstructorInvocationsInPrimitiveContext() {
        // Keep this comment
        byte by = new Byte((byte) 0);
        boolean bo = new Boolean(true);
        int i = new Integer(42);
        long l = new Long(42);
        short s = new Short((short) 42);
        float f = new Float(42.42F);
        double d = new Double(42.42);
    }

    public static void removeUnnecessaryConstructorInvocationsInSwitch() {
        byte by = (byte) 4;
        char c = 'c';
        short s = (short) 1;
        int i = 1;

        // Keep this comment
        switch (new Byte(by)) {
        // Keep this comment too
        default:
        }
        switch (new Character(c)) {
        default:
        }
        switch (new Short(s)) {
        default:
        }
        switch (new Integer(i)) {
        default:
        }
    }

    public static String removeUnnecessaryConstructorInvocationsInArrayAccess(String[] strings, int i) {
        return strings[new Integer(i)];
    }
}
