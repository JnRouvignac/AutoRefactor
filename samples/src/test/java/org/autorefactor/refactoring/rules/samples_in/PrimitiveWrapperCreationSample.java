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

public class PrimitiveWrapperCreationSample {

    public static void replaceWrapperConstructorsWithValueOf() {
        // Replace all calls to wrapper constructors with calls to .valueOf() methods
        byte b = 4;
        Byte by = new Byte(b);
        Boolean bo = new Boolean(true);
        Character c = new Character('c');
        Double d = new Double(1);
        Float f1 = new Float(1f);
        Float f2 = new Float(1d);
        Long l = new Long(1);
        short s = 1;
        Short sh = new Short(s);
        Integer i = new Integer(1);
    }

    public static void removeUnnecessaryObjectCreation() {
        new Byte("0").byteValue();
        new Boolean("true").booleanValue();
        new Integer("42").intValue();
        new Long("42").longValue();
        // nothing for Short?
        new Float("42.42").floatValue();
        new Double("42.42").doubleValue();
    }

    public static void convertValueOfCallsToParseCallsInPrimitiveContext() {
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

    public static void removeUnnecessaryValueOfCallsInPrimitiveContext() {
        byte by = Byte.valueOf((byte) 0);
        boolean bo1 = Boolean.valueOf(true);
        boolean bo2 = Boolean.TRUE;
        bo2 = Boolean.FALSE;
        int i = Integer.valueOf(42);
        long l = Long.valueOf(42);
        short s = Short.valueOf((short) 42);
        float f = Float.valueOf(42.42F);
        double d = Double.valueOf(42.42);
    }

    public static void removeUnnecessaryConstructorInvocationsInPrimitiveContext() {
        byte by = new Byte((byte) 0);
        boolean bo = new Boolean(true);
        int i = new Integer(42);
        long l = new Long(42);
        short s = new Short((short) 42);
        float f = new Float(42.42F);
        double d = new Double(42.42);
    }
}
