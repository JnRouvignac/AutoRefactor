/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
import java.util.Date;
import java.util.List;

public class OppositeComparisonRatherThanNegativeExpressionSample {
    public int simplifyDoubleComparison(Double number) {
        Double anotherNumber = Double.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyFloatComparison(Float number) {
        Float anotherNumber = Float.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyShortComparison(Short number) {
        Short anotherNumber = Short.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyIntegerComparison(Integer number) {
        Integer anotherNumber = Integer.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyLongComparison(Long number) {
        Long anotherNumber = Long.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyCharacterComparison(Character number) {
        Character anotherNumber = Character.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyByteComparison(Byte number) {
        Byte anotherNumber = Byte.MAX_VALUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int simplifyBooleanComparison(Boolean number) {
        Boolean anotherNumber = Boolean.TRUE;
        // Keep this comment
        return -number.compareTo(anotherNumber);
    }

    public int doNotRefactorNonFinalBigDecimalComparison() {
        BigDecimal number = new BigDecimal("10") {
            @Override
            public int compareTo(BigDecimal val) {
                return 0;
            }
        };
        BigDecimal anotherNumber = new BigDecimal("-10") {
            @Override
            public int compareTo(BigDecimal val) {
                return -1;
            }
        };
        return -number.compareTo(anotherNumber);
    }

    public int doNotRefactorNonFinalBigIntegerComparison(BigInteger number) {
        BigInteger anotherNumber = BigInteger.ZERO;
        return -number.compareTo(anotherNumber);
    }

    public int doNotRefactorNonFinalDateComparison(Date date) {
        Date anotherDate = new Date();
        return -date.compareTo(anotherDate);
    }

    public int callMethodOnExpression(Float number, List<Float> floats) {
        // Keep this comment
        return -number.compareTo(floats.get(0));
    }

    public int callMethodOnCast(Float number, List<Object> floats) {
        // Keep this comment
        return -number.compareTo((Float) floats.get(0));
    }

    public int doNotRefactorNonNegativeExpr(Double number) {
        Double anotherNumber = Double.MAX_VALUE;
        return number.compareTo(anotherNumber);
    }

    public int doNotCallMethodOnPrimitive(Double number) {
        double anotherNumber = 123d;
        return -number.compareTo(anotherNumber);
    }
}
