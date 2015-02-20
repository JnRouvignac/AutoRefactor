/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.samples_out;

public class StringSample {

    public String replaceNewString() {
        return "";
    }

    public String replaceNewStringFromStringExpression(String s) {
        return s;
    }

    public String replaceStringToString(String s) {
        String s1 = s;
        String s2 = "";
        String s3 = getS();
        return s1 + s2 + s3;
    }

    public String replaceToStringCallInStringConcat() {
        String s1 = "" + Boolean.TRUE + Boolean.FALSE;
        String s2 = Boolean.TRUE + "" + Boolean.FALSE;
        return s1 + s2;
    }

    public String doNotReplaceToStringCallOutsideStringConcat() {
        return Boolean.TRUE.toString();
    }

    public String doNotReplaceTwoConsecutiveToStringCalls() {
        return Boolean.TRUE.toString() + Boolean.FALSE;
    }

    public String removeUselessToString() {
        byte b = 42;
        short s = 42;
        String s1 = true + " foo";
        String s2 = 'c' + " foo";
        String s3 = b + " foo";
        String s4 = s + " foo";
        String s5 = 42 + " foo";
        String s6 = 42l + " foo";
        String s7 = 42.42f + " foo";
        String s8 = 42.42 + " foo";
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8;
    }

    public String removeUselessValueOf() {
        byte b = 42;
        short s = 42;
        String s1 = true + " foo";
        String s2 = 'c' + " foo";
        String s3 = b + " foo";
        String s4 = s + " foo";
        String s5 = 42 + " foo";
        String s6 = 42l + " foo";
        String s7 = 42.42f + " foo";
        String s8 = 42.42 + " foo";
        String s9 = new Object() + " foo";
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9;
    }

    // TODO JNR which operand must be removed here?
    // String s = "" + Integer.toString(42);

    private static String getS() {
        return null;
    }
}
