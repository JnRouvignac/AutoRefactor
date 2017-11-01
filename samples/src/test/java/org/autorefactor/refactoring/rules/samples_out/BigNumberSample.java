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
package org.autorefactor.refactoring.rules.samples_out;

import java.math.BigDecimal;
import java.math.BigInteger;

public class BigNumberSample {

    public static String useConstructorWithStringArg() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal("123.345");
        BigDecimal bd2 = new BigDecimal("123.345");
        BigDecimal bd3 = new BigDecimal("123.345");
        BigDecimal bd4 = new BigDecimal("123.345");
        BigDecimal bd5 = new BigDecimal("123.345");
        BigDecimal bd6 = new BigDecimal("123.345");
        return "" + bd1 + bd2 + bd3 + bd4 + bd5 + bd6;
    }

    public static boolean refactorToCompareToEqualsZero(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        boolean result1 = bd1.compareTo(bd2) != 0;
        boolean result2 = bd1.compareTo(bd2) == 0;
        return result1 && result2;
    }

    public static boolean refactorToCompareToEqualsZero(BigInteger bd1, BigInteger bd2) {
        // Keep this comment
        boolean result1 = bd1.compareTo(bd2) != 0;
        boolean result2 = bd1.compareTo(bd2) == 0;
        return result1 && result2;
    }

    public static String refactorToCompareToEqualsZeroSurroundWithParentheses(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        String s = "" + (bd1.compareTo(bd2) == 0);
        String s2 = "" + 1 + (bd1.compareTo(bd2) == 0) + 2;
        String s3 = "" + 1 + (bd1.compareTo(bd2) == 0);
        return s + s2 + s3;
    }

    public static String refactorToCompareToEqualsZeroSurroundWithParentheses(BigInteger bd1, BigInteger bd2) {
        // Keep this comment
        String s = "" + (bd1.compareTo(bd2) == 0);
        String s2 = "" + 1 + (bd1.compareTo(bd2) == 0) + 2;
        String s3 = "" + 1 + (bd1.compareTo(bd2) == 0);
        return s + s2 + s3;
    }

    public static String useBigDecimalConstants() {
        // Keep this comment
        BigDecimal bd1 = BigDecimal.ZERO;
        BigDecimal bd2 = BigDecimal.ONE;
        BigDecimal bd3 = BigDecimal.TEN;
        BigDecimal bd4 = BigDecimal.ZERO;
        BigDecimal bd5 = BigDecimal.ONE;
        BigDecimal bd6 = BigDecimal.TEN;
        BigDecimal bd7 = BigDecimal.ZERO;
        BigDecimal bd8 = BigDecimal.ONE;
        BigDecimal bd9 = BigDecimal.TEN;

        BigDecimal bd10 = BigDecimal.ZERO;
        BigDecimal bd11 = BigDecimal.ONE;
        BigDecimal bd12 = BigDecimal.TEN;

        BigDecimal bd13 = BigDecimal.ZERO;
        BigDecimal bd14 = BigDecimal.ONE;
        BigDecimal bd15 = BigDecimal.TEN;
        BigDecimal bd16 = BigDecimal.ZERO;
        BigDecimal bd17 = BigDecimal.ONE;
        BigDecimal bd18 = BigDecimal.TEN;
        BigDecimal bd19 = BigDecimal.ZERO;
        BigDecimal bd20 = BigDecimal.ONE;
        BigDecimal bd21 = BigDecimal.TEN;

        return "" + bd1 + bd2 + bd3 + bd4 + bd5 + bd6 + bd7 + bd8 + bd9 + bd10 + bd11 + bd12 + bd13 + bd14 + bd15
                + bd16 + bd17 + bd18 + bd19 + bd20 + bd21;
    }

    public static String useBigIntegerConstants() {
        // Keep this comment
        BigInteger bi1 = BigInteger.ZERO;
        BigInteger bi2 = BigInteger.ONE;
        BigInteger bi3 = BigInteger.TEN;

        BigInteger bi4 = BigInteger.ZERO;
        BigInteger bi5 = BigInteger.ONE;
        BigInteger bi6 = BigInteger.TEN;
        BigInteger bi7 = BigInteger.ZERO;
        BigInteger bi8 = BigInteger.ONE;
        BigInteger bi9 = BigInteger.TEN;
        BigInteger bi10 = BigInteger.ZERO;
        BigInteger bi11 = BigInteger.ONE;
        BigInteger bi12 = BigInteger.TEN;

        return "" + bi1 + bi2 + bi3 + bi4 + bi5 + bi6 + bi7 + bi8 + bi9 + bi10 + bi11 + bi12;
    }

    public static String useValueOf() {
        // Keep this comment
        BigDecimal bd1 = BigDecimal.valueOf(5);
        BigDecimal bd2 = BigDecimal.valueOf(5);
        BigDecimal bd3 = BigDecimal.valueOf(5);
        BigDecimal bd4 = BigDecimal.valueOf(5);
        BigInteger bi1 = BigInteger.valueOf(5);

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
