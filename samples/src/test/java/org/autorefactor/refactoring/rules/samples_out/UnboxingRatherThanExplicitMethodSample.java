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

public class UnboxingRatherThanExplicitMethodSample {
    public static void useUnboxingOnPrimitiveDeclaration(Character cObject, Byte byObject, Boolean boObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        // Keep this comment
        char c = cObject;
        byte by = byObject;
        boolean bo = boObject;
        int i = iObject;
        short s = sObject;
        long l = lObject;
        float f = fObject;
        double d = dObject;
    }

    public static void doNotUseUnboxingOnNarrowingType(Character cObject, Byte byObject,
            Integer iObject, Short sObject, Float fObject) {
        int c = cObject.charValue();
        int by = byObject.byteValue();
        long i = iObject.intValue();
        int s = sObject.shortValue();
        double f = fObject.floatValue();
    }

    public static void doNotUseUnboxingWhenTypesDontMatch(Byte byObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        short by = byObject.shortValue();
        short i = iObject.shortValue();
        byte s = sObject.byteValue();
        short l = lObject.shortValue();
        short f = fObject.shortValue();
        short d = dObject.shortValue();
    }

    public static void reuseWrapper(Character cObject, Byte byObject, Boolean boObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        // Keep this comment
        Character c = cObject;
        Byte by = byObject;
        Boolean bo = boObject;
        Integer i = iObject;
        Short s = sObject;
        Long l = lObject;
        Float f = fObject;
        Double d = dObject;
    }

    public static void useUnboxingOnPrimitiveAssignment(Character cObject, Byte byObject, Boolean boObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        // Keep this comment
        char c;
        c = cObject;
        byte by;
        by = byObject;
        boolean bo;
        bo = boObject;
        int i;
        i = iObject;
        short s;
        s = sObject;
        long l;
        l = lObject;
        float f;
        f = fObject;
        double d;
        d = dObject;
    }

    public static char useUnboxingOnPrimitiveReturn(Character cObject) {
        // Keep this comment
        return cObject;
    }

    public static byte useUnboxingOnPrimitiveReturn(Byte byObject) {
        // Keep this comment
        return byObject;
    }

    public static boolean useUnboxingOnPrimitiveReturn(Boolean boObject) {
        // Keep this comment
        return boObject;
    }

    public static int useUnboxingOnPrimitiveReturn(Integer iObject) {
        // Keep this comment
        return iObject;
    }

    public static short useUnboxingOnPrimitiveReturn(Short sObject) {
        // Keep this comment
        return sObject;
    }

    public static long useUnboxingOnPrimitiveReturn(Long lObject) {
        // Keep this comment
        return lObject;
    }

    public static float useUnboxingOnPrimitiveReturn(Float fObject) {
        // Keep this comment
        return fObject;
    }

    public static double useUnboxingOnPrimitiveReturn(Double dObject) {
        // Keep this comment
        return dObject;
    }

    public static void useUnboxingOnSwitch(Byte by, Character c, Short s, Integer i) {
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

    public static String useUnboxingOnArrayAccess(String[] strings, Integer i) {
        return strings[i];
    }

    public static String doNotUseUnboxingOnMethod(Character cObject, Byte byObject, Boolean boObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        StringBuilder sb = new StringBuilder();

        sb.append(cObject.charValue());
        sb.append(byObject.byteValue());
        sb.append(boObject.booleanValue());
        sb.append(iObject.intValue());
        sb.append(sObject.shortValue());
        sb.append(lObject.longValue());
        sb.append(fObject.floatValue());
        sb.append(dObject.doubleValue());

        return sb.toString();
    }
}
