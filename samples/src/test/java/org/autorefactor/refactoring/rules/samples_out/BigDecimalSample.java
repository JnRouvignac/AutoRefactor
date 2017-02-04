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

public class BigDecimalSample {

    private static String useConstructorWithStringArg() {
        // Keep this comment
        BigDecimal bd1 = new BigDecimal("123.345");
        BigDecimal bd2 = new BigDecimal("123.345");
        return "" + bd1 + bd2;
    }

    private static boolean refactorToCompareToEqualsZero(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        return bd1.compareTo(bd2) == 0;
    }

    private static String refactorToCompareToEqualsZeroSurroundWithParentheses(BigDecimal bd1, BigDecimal bd2) {
        // Keep this comment
        String s = "" + (bd1.compareTo(bd2) == 0);
        String s2 = "" + 1 + (bd1.compareTo(bd2) == 0) + 2;
        String s3 = "" + 1 + (bd1.compareTo(bd2) == 0);
        return s + s2 + s3;
    }

    private static String useBigDecimalConstants() {
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

        return "" + bd1 + bd2 + bd3 + bd4 + bd5 + bd6 + bd7 + bd8 + bd9;
    }

    private static String useValueOf() {
        // Keep this comment
        BigDecimal bd1 = BigDecimal.valueOf(5);
        BigDecimal bd2 = BigDecimal.valueOf(5);
        return "" + bd1 + bd2;
    }

    private static BigDecimal doNotRefactorCorrectUseOfValueOf() {
        return BigDecimal.valueOf(5);
    }

    private static BigDecimal doNotRefactorCorrectUseOfCtorWithStringArg() {
        return new BigDecimal("5.4");
    }
}
