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
package org.autorefactor.refactoring.rules.samples_in;

public class UnboxingRatherThanExplicitMethodSample {
    public static void useUnboxingOnPrimitiveDeclaration(Character cObject, Byte byObject, Boolean boObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        // Keep this comment
        char c = cObject.charValue();
        byte by = byObject.byteValue();
        boolean bo = boObject.booleanValue();
        int i = iObject.intValue();
        short s = sObject.shortValue();
        long l = lObject.longValue();
        float f = fObject.floatValue();
        double d = dObject.doubleValue();
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
        Character c = cObject.charValue();
        Byte by = byObject.byteValue();
        Boolean bo = boObject.booleanValue();
        Integer i = iObject.intValue();
        Short s = sObject.shortValue();
        Long l = lObject.longValue();
        Float f = fObject.floatValue();
        Double d = dObject.doubleValue();
    }

    public static void useUnboxingOnPrimitiveAssignment(Character cObject, Byte byObject, Boolean boObject,
            Integer iObject, Short sObject, Long lObject, Float fObject, Double dObject) {
        // Keep this comment
        char c;
        c = cObject.charValue();
        byte by;
        by = byObject.byteValue();
        boolean bo;
        bo = boObject.booleanValue();
        int i;
        i = iObject.intValue();
        short s;
        s = sObject.shortValue();
        long l;
        l = lObject.longValue();
        float f;
        f = fObject.floatValue();
        double d;
        d = dObject.doubleValue();
    }

    public static char useUnboxingOnPrimitiveReturn(Character cObject) {
        // Keep this comment
        return cObject.charValue();
    }

    public static byte useUnboxingOnPrimitiveReturn(Byte byObject) {
        // Keep this comment
        return byObject.byteValue();
    }

    public static boolean useUnboxingOnPrimitiveReturn(Boolean boObject) {
        // Keep this comment
        return boObject.booleanValue();
    }

    public static int useUnboxingOnPrimitiveReturn(Integer iObject) {
        // Keep this comment
        return iObject.intValue();
    }

    public static short useUnboxingOnPrimitiveReturn(Short sObject) {
        // Keep this comment
        return sObject.shortValue();
    }

    public static long useUnboxingOnPrimitiveReturn(Long lObject) {
        // Keep this comment
        return lObject.longValue();
    }

    public static float useUnboxingOnPrimitiveReturn(Float fObject) {
        // Keep this comment
        return fObject.floatValue();
    }

    public static double useUnboxingOnPrimitiveReturn(Double dObject) {
        // Keep this comment
        return dObject.doubleValue();
    }

    public static void useUnboxingOnSwitch(Byte by, Character c, Short s, Integer i) {
        // Keep this comment
        switch (by.byteValue()) {
        // Keep this comment too
        default:
        }
        switch (c.charValue()) {
        default:
        }
        switch (s.shortValue()) {
        default:
        }
        switch (i.intValue()) {
        default:
        }
    }

    public static String useUnboxingOnArrayAccess(String[] strings, Integer i) {
        return strings[i.intValue()];
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
