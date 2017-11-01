/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.math.BigDecimal;
import java.math.BigInteger;

public class BigNumberSample {

    public static String useConstructorWithStringArg() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal(123.345);
        BigDecimal bd2 = new BigDecimal(123.345f);
        BigDecimal bd3 = new BigDecimal(123.345d);
        BigDecimal bd4 = BigDecimal.valueOf(123.345);
        BigDecimal bd5 = BigDecimal.valueOf(123.345f);
        BigDecimal bd6 = BigDecimal.valueOf(123.345d);
        return "" + bd1 + bd2 + bd3 + bd4 + bd5 + bd6;
    }

    public static boolean refactorToCompareToEqualsZero(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        boolean result1 = !bd1.equals(bd2);
        boolean result2 = bd1.equals(bd2);
        return result1 && result2;
    }

    public static boolean refactorToCompareToEqualsZero(BigInteger bd1, BigInteger bd2) {
        // Keep this comment
        boolean result1 = !bd1.equals(bd2);
        boolean result2 = bd1.equals(bd2);
        return result1 && result2;
    }

    public static String refactorToCompareToEqualsZeroSurroundWithParentheses(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        String s = "" + bd1.equals(bd2);
        String s2 = "" + 1 + bd1.equals(bd2) + 2;
        String s3 = "" + 1 + bd1.equals(bd2);
        return s + s2 + s3;
    }

    public static String refactorToCompareToEqualsZeroSurroundWithParentheses(BigInteger bd1, BigInteger bd2) {
        // Keep this comment
        String s = "" + bd1.equals(bd2);
        String s2 = "" + 1 + bd1.equals(bd2) + 2;
        String s3 = "" + 1 + bd1.equals(bd2);        return s + s2 + s3;
    }

    public static String useBigDecimalConstants() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal(0);
        BigDecimal bd2 = new BigDecimal(1);
        BigDecimal bd3 = new BigDecimal(10);
        BigDecimal bd4 = new BigDecimal(0l);
        BigDecimal bd5 = new BigDecimal(1l);
        BigDecimal bd6 = new BigDecimal(10l);
        BigDecimal bd7 = new BigDecimal(0L);
        BigDecimal bd8 = new BigDecimal(1L);
        BigDecimal bd9 = new BigDecimal(10L);

        BigDecimal bd10 = new BigDecimal("0");
        BigDecimal bd11 = new BigDecimal("1");
        BigDecimal bd12 = new BigDecimal("10");

        BigDecimal bd13 = BigDecimal.valueOf(0);
        BigDecimal bd14 = BigDecimal.valueOf(1);
        BigDecimal bd15 = BigDecimal.valueOf(10);
        BigDecimal bd16 = BigDecimal.valueOf(0l);
        BigDecimal bd17 = BigDecimal.valueOf(1l);
        BigDecimal bd18 = BigDecimal.valueOf(10l);
        BigDecimal bd19 = BigDecimal.valueOf(0L);
        BigDecimal bd20 = BigDecimal.valueOf(1L);
        BigDecimal bd21 = BigDecimal.valueOf(10L);

        return "" + bd1 + bd2 + bd3 + bd4 + bd5 + bd6 + bd7 + bd8 + bd9 + bd10 + bd11 + bd12 + bd13 + bd14 + bd15
                + bd16 + bd17 + bd18 + bd19 + bd20 + bd21;
    }

    public static String useBigIntegerConstants() {
        // Keep this comment
        BigInteger bi1 = new BigInteger("0");
        BigInteger bi2 = new BigInteger("1");
        BigInteger bi3 = new BigInteger("10");

        BigInteger bi4 = BigInteger.valueOf(0);
        BigInteger bi5 = BigInteger.valueOf(1);
        BigInteger bi6 = BigInteger.valueOf(10);
        BigInteger bi7 = BigInteger.valueOf(0l);
        BigInteger bi8 = BigInteger.valueOf(1l);
        BigInteger bi9 = BigInteger.valueOf(10l);
        BigInteger bi10 = BigInteger.valueOf(0L);
        BigInteger bi11 = BigInteger.valueOf(1L);
        BigInteger bi12 = BigInteger.valueOf(10L);

        return "" + bi1 + bi2 + bi3 + bi4 + bi5 + bi6 + bi7 + bi8 + bi9 + bi10 + bi11 + bi12;
    }

    public static String useValueOf() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal(5);
        BigDecimal bd2 = new BigDecimal(5l);
        BigDecimal bd3 = new BigDecimal(5L);
        BigDecimal bd4 = new BigDecimal("5");
        BigInteger bi1 = new BigInteger("5");

        return "" + bd1 + bd2 + bd3 + bd4 + bi1;
    }

    public static BigDecimal doNotRefactorCorrectUseOfBigDecimalValueOf() {
        return BigDecimal.valueOf(5);
    }

    public static BigInteger doNotRefactorCorrectUseOfBigIntegerValueOf() {
        return BigInteger.valueOf(5);
    }

    public static BigDecimal doNotRefactorCorrectUseOfBigDecimalCtorWithStringArg() {
        return new BigDecimal("5.4");
    }
}
