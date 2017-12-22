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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.    If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring;

import static org.autorefactor.refactoring.JavaConstants.*;
import static org.junit.Assert.*;

import org.junit.Test;

public class JavaConstantsTest {

    // @DataProvider
    public Object[][] getValidLongZeroLiteral() {
        return new Object[][] {
            // @formatter:off
            // valid
            { "0b0l" }, { "0B0l" }, { "0b0000l" }, { "0B0000l" },
            { "0b0L" }, { "0B0L" }, { "0b0000L" }, { "0B0000L" },
            { "00000l" }, { "00l" },
            { "00000L" }, { "00L" },
            { "0l" }, { "0L" },
            { "0x0l" }, { "0X0l" }, { "0x0000l" }, { "0X0000l" },
            { "0x0L" }, { "0X0L" }, { "0x0000L" }, { "0X0000L" },
            // valid underscores
            { "0b0_0_0_0l" }, { "0B0_000l" }, { "0b000_0l" }, { "0B00_00l" },
            { "0b0_0_0_0L" }, { "0B0_000L" }, { "0b000_0L" }, { "0B00_00L" },
            { "0_0_0_0_0l" }, { "0_0l" }, { "0000_0l" },
            { "0_0_0_0_0L" }, { "0_0L" }, { "0000_0L" },
            { "0l" }, { "0L" },
            { "0x0_000l" }, { "0X00_00l" }, { "0x0_0_0_0l" }, { "0X000_0l" },
            { "0x0_000L" }, { "0X00_00L" }, { "0x0_0_0_0L" }, { "0X000_0L" },
            // @formatter:on
        };
    }

    // @DataProvider
    public Object[][] getValidIntegerZeroLiteral() {
        return new Object[][] {
            // @formatter:off
            // valid
            { "0b0" }, { "0B0" }, { "0b0000" }, { "0B0000" },
            { "00000" }, { "00" },
            { "0" },
            { "0x0" }, { "0X0" }, { "0x0000" }, { "0X0000" },
            // valid underscores
            { "0b0_0_0_0" }, { "0B0_000" }, { "0b000_0" }, { "0B00_00" },
            { "0_0_0_0_0" }, { "0_0" }, { "0000_0" },
            { "0" },
            { "0x0_000" }, { "0X00_00" }, { "0x0_0_0_0" }, { "0X000_0" },
            // @formatter:on
        };
    }

    // @DataProvider
    public Object[][] getInvalidIntegerZeroLiteral() {
        return new Object[][] {
            // @formatter:off
            // invalid
            { "0b_0" }, { "0B_0" },
            // This is an identifier, not a numeric literal
            { "_00000" }, { "_00" },
            { "_0" },
            // Invalid; cannot put underscores at the end of a literal
            { "00000_" }, { "00_" },
            { "0_" },
            // Invalid; cannot put underscores in the 0x radix prefix
            { "0_b0" }, { "0_B0" },
            { "0_x0" }, { "0_X0" },
            // Invalid; cannot put underscores at the beginning of a number
            { "0b_0" }, { "0B_0" }, { "0b_0000" }, { "0B_0000" },
            { "0x_0" }, { "0X_0" }, { "0x_0000" }, { "0X_0000" },
            // Invalid; cannot put underscores at the end of a number
            { "0b0_" }, { "0B0_" }, { "0b0000_" }, { "0B0000_" },
            { "00000_" }, { "00_" },
            { "0_" },
            { "0x0_" }, { "0X0_" }, { "0x0000_" }, { "0X0000_" },
            // @formatter:on
        };
    }



    // @DataProvider
    public Object[][] getValidLongOneLiteral() {
        return new Object[][] {
            // @formatter:off
            // valid
            { "0b1l" }, { "0B1l" }, { "0b0001l" }, { "0B0001l" },
            { "0b1L" }, { "0B1L" }, { "0b0001L" }, { "0B0001L" },
            { "00001l" }, { "01l" },
            { "00001L" }, { "01L" },
            { "1l" }, { "1L" },
            { "0x1l" }, { "0X1l" }, { "0x0001l" }, { "0X0001l" },
            { "0x1L" }, { "0X1L" }, { "0x0001L" }, { "0X0001L" },
            // valid underscores
            { "0b0_0_0_1l" }, { "0B0_001l" }, { "0b000_1l" }, { "0B00_01l" },
            { "0b0_0_0_1L" }, { "0B0_001L" }, { "0b000_1L" }, { "0B00_01L" },
            { "0_0_0_0_1l" }, { "0_1l" }, { "0000_1l" },
            { "0_0_0_0_1L" }, { "0_1L" }, { "0000_1L" },
            { "1l" }, { "1L" },
            { "0x0_001l" }, { "0X00_01l" }, { "0x0_0_0_1l" }, { "0X000_1l" },
            { "0x0_001L" }, { "0X00_01L" }, { "0x0_0_0_1L" }, { "0X000_1L" },
            // @formatter:on
        };
    }

    // @DataProvider
    public Object[][] getValidIntegerOneLiteral() {
        return new Object[][] {
            // @formatter:off
            // valid
            { "0b1" }, { "0B1" }, { "0b0001" }, { "0B0001" },
            { "00001" }, { "01" },
            { "1" },
            { "0x1" }, { "0X1" }, { "0x0001" }, { "0X0001" },
            // valid underscores
            { "0b0_0_0_1" }, { "0B0_001" }, { "0b000_1" }, { "0B00_01" },
            { "0_0_0_0_1" }, { "0_1" }, { "0000_1" },
            { "1" },
            { "0x0_001" }, { "0X00_01" }, { "0x0_0_0_1" }, { "0X000_1" },
            // @formatter:on
        };
    }

    // @DataProvider
    public Object[][] getInvalidIntegerOneLiteral() {
        return new Object[][] {
            // @formatter:off
            // invalid
            { "0b_1" }, { "0B_1" },
            // This is an identifier, not a numeric literal
            { "_00001" }, { "_01" },
            { "_1" },
            // Invalid; cannot put underscores at the end of a literal
            { "00001_" }, { "01_" },
            { "1_" },
            // Invalid; cannot put underscores in the 0x radix prefix
            { "0_b1" }, { "0_B1" },
            { "0_x1" }, { "0_X1" },
            // Invalid; cannot put underscores at the beginning of a number
            { "0b_1" }, { "0B_1" }, { "0b_0001" }, { "0B_0001" },
            { "0x_1" }, { "0X_1" }, { "0x_0001" }, { "0X_0001" },
            // Invalid; cannot put underscores at the end of a number
            { "0b1_" }, { "0B1_" }, { "0b0001_" }, { "0B0001_" },
            { "00001_" }, { "01_" },
            { "1_" },
            { "0x1_" }, { "0X1_" }, { "0x0001_" }, { "0X0001_" },
            // @formatter:on
        };
    }



    // @DataProvider
    public Object[][] getValidLongTenLiteral() {
        return new Object[][] {
            // @formatter:off
            // valid
            { "0b1010l" }, { "0B1010l" }, { "0b0001010l" }, { "0B0001010l" },
            { "0b1010L" }, { "0B1010L" }, { "0b0001010L" }, { "0B0001010L" },
            { "000013l" }, { "013l" },
            { "000013L" }, { "013L" },
            { "10l" }, { "10L" },
            { "0xal" }, { "0Xal" }, { "0x000al" }, { "0X000al" },
            { "0xAl" }, { "0XAl" }, { "0x000Al" }, { "0X000Al" },
            { "0xaL" }, { "0XaL" }, { "0x000aL" },    { "0X000aL" },
            { "0xAL" },    { "0XAL" }, { "0x000AL" }, { "0X000AL" },
            // valid underscores
            { "0b0_0_0_10_10l" }, { "0B0_00101_0l" }, { "0b000_1_010l" }, { "0B00_0101_0l" },
            { "0b0_0_0_10_10L" }, { "0B0_00101_0L" }, { "0b000_1_010L" }, { "0B00_0101_0L" },
            { "0_0_0_0_13l" }, { "0_13l" }, { "0000_13l" }, { "0_1_3l" },
            { "0_0_0_0_13L" }, { "0_13L" }, { "0000_13L" }, { "0_1_3L" },
            { "10l" }, { "10L" },
            { "0x0_00al" }, { "0X00_0al" }, { "0x0_0_0_al" }, { "0X000_al" },
            { "0x0_00Al" }, { "0X00_0Al" }, { "0x0_0_0_Al" }, { "0X000_Al" },
            { "0x0_00aL" }, { "0X00_0aL" }, { "0x0_0_0_aL" }, { "0X000_aL" },
            { "0x0_00AL" }, { "0X00_0AL" }, { "0x0_0_0_AL" }, { "0X000_AL" },
            // @formatter:on
        };
    }

    // @DataProvider
    public Object[][] getValidIntegerTenLiteral() {
        return new Object[][] {
            // @formatter:off
            // valid
            { "0b1010" }, { "0B1010" }, { "0b0001010" }, { "0B0001010" },
            { "000013" }, { "013" },
            { "10" },
            { "0xa" }, { "0Xa" }, { "0x000a" }, { "0X000a" },
            { "0xA" }, { "0XA" }, { "0x000A" }, { "0X000A" },
            // valid underscores
            { "0b0_0_0_1010" }, { "0B0_001010" }, { "0b000_1010" }, { "0B00_01010" },
            { "0_0_0_0_13" }, { "0_13" }, { "0000_13" },
            { "10" },
            { "0x0_00a" }, { "0X00_0a" }, { "0x0_0_0_a" }, { "0X000_a" },
            { "0x0_00A" }, { "0X00_0A" }, { "0x0_0_0_A" }, { "0X000_A" },
            // @formatter:on
        };
    }

    // @DataProvider
    public Object[][] getInvalidIntegerTenLiteral() {
        return new Object[][] {
            // @formatter:off
            // invalid
            { "0b_1010" }, { "0B_1010" },
            // This is an identifier, not a numeric literal
            { "_000013" }, { "_013" },
            { "_10" },
            // Invalid; cannot put underscores at the end of a literal
            { "000013_" }, { "013_" },
            { "10_" },
            // Invalid; cannot put underscores in the 0x radix prefix
            { "0_b1010" }, { "0_B1010" },
            { "0_xa" }, { "0_Xa" },
            { "0_xA" }, { "0_XA" },
            // Invalid; cannot put underscores at the beginning of a number
            { "0b_1010" }, { "0B_1010" }, { "0b_0001010" }, { "0B_0001010" },
            { "0x_a" }, { "0X_a" }, { "0x_000a" }, { "0X_000a" },
            { "0x_A" }, { "0X_A" }, { "0x_000A" }, { "0X_000A" },
            // Invalid; cannot put underscores at the end of a number
            { "0b1010_" }, { "0B1010_" }, { "0b0001010_" }, { "0B0001010_" },
            { "000013_" }, { "013_" },
            { "10_" },
            { "0xa_" }, { "0Xa_" }, { "0x000a_" }, { "0X000a_" },
            { "0xA_" }, { "0XA_" }, { "0x000A_" }, { "0X000A_" },
            // @formatter:on
        };
    }



    // @DataProvider
    public Object[][] getValidIntegerLiteral() {
        Object[][] zeros = getValidIntegerZeroLiteral();
        Object[][] ones = getValidIntegerOneLiteral();
        Object[][] tens = getValidIntegerTenLiteral();
        return collate(zeros, ones, tens);
    }

    // @DataProvider
    public Object[][] getInvalidIntegerLiteral() {
        Object[][] zeros = getInvalidIntegerZeroLiteral();
        Object[][] ones = getInvalidIntegerOneLiteral();
        Object[][] tens = getInvalidIntegerTenLiteral();
        return collate(zeros, ones, tens);
    }

    // @DataProvider
    public Object[][] getValidLongLiteral() {
        Object[][] zeros = getValidLongZeroLiteral();
        Object[][] ones = getValidLongOneLiteral();
        Object[][] tens = getValidLongTenLiteral();
        return collate(zeros, ones, tens);
    }

    private Object[][] collate(Object[][] zeros, Object[][] ones, Object[][] tens) {
        Object[][] results = new Object[zeros.length + ones.length + tens.length][];
        int start = 0;
        System.arraycopy(zeros, 0, results, start, zeros.length);
        start += zeros.length;
        System.arraycopy(ones, 0, results, start, ones.length);
        start += ones.length;
        System.arraycopy(tens, 0, results, start, tens.length);
        return results;
    }



    @Test
    public void validIntegerZeroLongLiteralRegexp() {
        for (Object[] args : getValidIntegerZeroLiteral()) {
            validIntegerZeroLongLiteralRegexp((String) args[0]);
        }
    }

    public void validIntegerZeroLongLiteralRegexp(String literal) {
        assertTrue(ZERO_LONG_LITERAL_RE.matcher(literal).matches());
    }

    @Test
    public void invalidIntegerZeroLongLiteralRegexp() {
        for (Object[] args : getInvalidIntegerZeroLiteral()) {
            invalidIntegerZeroLongLiteralRegexp((String) args[0]);
        }
    }

    public void invalidIntegerZeroLongLiteralRegexp(String literal) {
        assertFalse(ZERO_LONG_LITERAL_RE.matcher(literal).matches());
    }

    @Test
    public void validLongZeroLongLiteralRegexp() {
        for (Object[] args : getValidLongZeroLiteral()) {
            validLongZeroLongLiteralRegexp((String) args[0]);
        }
    }

    public void validLongZeroLongLiteralRegexp(String literal) {
        assertTrue(ZERO_LONG_LITERAL_RE.matcher(literal).matches());
    }



    @Test
    public void validIntegerOneLongLiteralRegexp() {
        for (Object[] args : getValidIntegerOneLiteral()) {
            validIntegerOneLongLiteralRegexp((String) args[0]);
        }
    }

    public void validIntegerOneLongLiteralRegexp(String literal) {
        assertTrue(ONE_LONG_LITERAL_RE.matcher(literal).matches());
    }

    @Test
    public void invalidIntegerOneLongLiteralRegexp() {
        for (Object[] args : getInvalidIntegerOneLiteral()) {
            invalidIntegerOneLongLiteralRegexp((String) args[0]);
        }
    }

    public void invalidIntegerOneLongLiteralRegexp(String literal) {
        assertFalse(ONE_LONG_LITERAL_RE.matcher(literal).matches());
    }

    @Test
    public void validLongOneLongLiteralRegexp() {
        for (Object[] args : getValidLongOneLiteral()) {
            validLongOneLongLiteralRegexp((String) args[0]);
        }
    }

    public void validLongOneLongLiteralRegexp(String literal) {
        assertTrue(ONE_LONG_LITERAL_RE.matcher(literal).matches());
    }



    @Test
    public void validIntegerTenLongLiteralRegexp() {
        for (Object[] args : getValidIntegerTenLiteral()) {
            validIntegerTenLongLiteralRegexp((String) args[0]);
        }
    }

    public void validIntegerTenLongLiteralRegexp(String literal) {
        assertTrue(TEN_LONG_LITERAL_RE.matcher(literal).matches());
    }

    @Test
    public void invalidIntegerTenLongLiteralRegexp() {
        for (Object[] args : getInvalidIntegerTenLiteral()) {
            invalidIntegerTenLongLiteralRegexp((String) args[0]);
        }
    }

    public void invalidIntegerTenLongLiteralRegexp(String literal) {
        assertFalse(TEN_LONG_LITERAL_RE.matcher(literal).matches());
    }

    @Test
    public void validLongTenLongLiteralRegexp() {
        for (Object[] args : getValidLongTenLiteral()) {
            validLongTenLongLiteralRegexp((String) args[0]);
        }
    }

    public void validLongTenLongLiteralRegexp(String literal) {
        assertTrue(TEN_LONG_LITERAL_RE.matcher(literal).matches());
    }



    @Test
    public void validIntegerLiteralRegexp() {
        for (Object[] args : getValidIntegerLiteral()) {
            validIntegerLiteralRegexp((String) args[0]);
        }
    }

    public void validIntegerLiteralRegexp(String literal) {
        assertTrue(INTEGER_LITERAL_COMPATIBLE_RE.matcher(literal).matches());
    }

    @Test
    public void invalidIntegerLiteralRegexp() {
        for (Object[] args : getInvalidIntegerLiteral()) {
            invalidIntegerLiteralRegexp((String) args[0]);
        }
    }

    public void invalidIntegerLiteralRegexp(String literal) {
        assertFalse(INTEGER_LITERAL_COMPATIBLE_RE.matcher(literal).matches());
    }

    @Test
    public void validLongLiteralRegexp() {
        for (Object[] args : getValidLongLiteral()) {
            validLongLiteralRegexp((String) args[0]);
        }
    }

    public void validLongLiteralRegexp(String literal) {
        assertTrue(LONG_LITERAL_COMPATIBLE_RE.matcher(literal).matches());
    }
}
