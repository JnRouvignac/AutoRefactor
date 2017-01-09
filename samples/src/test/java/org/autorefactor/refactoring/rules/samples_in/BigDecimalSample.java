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

public class BigDecimalSample {

    private static String useConstructorWithStringArg() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal(123.345);
        BigDecimal bd2 = BigDecimal.valueOf(123.345);
        return "" + bd1 + bd2;
    }

    private static boolean refactorToCompareToEqualsZero(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        return bd1.equals(bd2);
    }

    private static String refactorToCompareToEqualsZeroSurroundWithParentheses(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        String s = "" + bd1.equals(bd2);
        String s2 = "" + 1 + bd1.equals(bd2) + 2;
        String s3 = "" + 1 + bd1.equals(bd2);        return s + s2 + s3;
    }

    private static String useBigDecimalConstants() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal(0);
        BigDecimal bd2 = new BigDecimal(1);
        BigDecimal bd3 = new BigDecimal(10);

        BigDecimal bd4 = new BigDecimal("0");
        BigDecimal bd5 = new BigDecimal("1");
        BigDecimal bd6 = new BigDecimal("10");

        BigDecimal bd7 = BigDecimal.valueOf(0);
        BigDecimal bd8 = BigDecimal.valueOf(1);
        BigDecimal bd9 = BigDecimal.valueOf(10);

        return "" + bd1 + bd2 + bd3 + bd4 + bd5 + bd6 + bd7 + bd8 + bd9;
    }

    private static String useValueOf() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal(5);
        BigDecimal bd2 = new BigDecimal("5");
        return "" + bd1 + bd2;
    }

    private static BigDecimal doNotRefactorCorrectUseOfValueOf() {
        return BigDecimal.valueOf(5);
    }

    private static BigDecimal doNotRefactorCorrectUseOfCtorWithStringArg() {
        return new BigDecimal("5.4");
    }
}
