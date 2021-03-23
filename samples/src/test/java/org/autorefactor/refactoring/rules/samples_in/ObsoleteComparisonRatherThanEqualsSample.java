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

public class ObsoleteComparisonRatherThanEqualsSample {
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
}
