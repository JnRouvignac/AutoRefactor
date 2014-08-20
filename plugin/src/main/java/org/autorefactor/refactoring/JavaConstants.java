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

import java.util.regex.Pattern;

/**
 * Interface holding java constants.
 */
public interface JavaConstants {

    /**
     * String pattern identifying number literal for an integer.
     *
     * @see <a href="http://docs.oracle.com/javase/7/docs/technotes/guides/language/binary-literals.html">
     *      Binary Literals</a>
     * @see <a href="http://docs.oracle.com/javase/7/docs/technotes/guides/language/underscores-literals.html">
     *      Underscores in Numeric Literals</a>
     */
    String INTEGER_LITERAL_COMPATIBLE_PATTERN = "" + "(?:"
            // binary literal
            + "(?:0b|0B)(?:[0-1][0-1_]*)?[0-1]" + ")|(?:"
            // octal literal
            + "0(?:[0-7_]*)?[0-7]" + ")|(?:"
            // decimal literal
            + "[0-9]([0-9_]*[0-9])?" + ")|(?:"
            // hexadecimal literal
            + "(?:0x|0X)([0-9a-fA-F][0-9a-fA-F_]*)?[0-9a-fA-F]" + ")";

    /** Pattern identifying number literal for an integer. */
    Pattern INTEGER_LITERAL_COMPATIBLE_RE = Pattern
            .compile(INTEGER_LITERAL_COMPATIBLE_PATTERN);

    /** String pattern identifying number literal for a long. */
    String LONG_LITERAL_COMPATIBLE_PATTERN = ""
            + ("(?:" + INTEGER_LITERAL_COMPATIBLE_PATTERN + ")(?:l|L)?");

    /** Pattern identifying number literal for a long. */
    Pattern LONG_LITERAL_COMPATIBLE_RE = Pattern
            .compile(LONG_LITERAL_COMPATIBLE_PATTERN);

    /** Pattern identifying number literal for long "0". */
    Pattern ZERO_LONG_LITERAL_RE = Pattern.compile("(?:" + "(?:"
            // binary literal
            + "(?:0b|0B)(?:0[0_]*)?0" + ")|(?:"
            // octal literal
            + "0[0_]*0" + ")|(?:"
            // decimal literal
            + "0(?:[0_]*0)?" + ")|(?:"
            // hexadecimal literal
            + "(?:0x|0X)(?:0[0_]*)?0" + ")" + ")(?:l|L)?");

    /** Pattern identifying number literal for long "1". */
    Pattern ONE_LONG_LITERAL_RE = Pattern.compile("(?:" + "(?:"
            // binary literal
            + "(?:0b|0B)(?:0[0_]*)?1" + ")|(?:"
            // octal literal
            + "0[0_]*1" + ")|(?:"
            // decimal literal
            + "1" + ")|(?:"
            // hexadecimal literal
            + "(?:0x|0X)(?:0[0_]*)?1" + ")" + ")(?:l|L)?");

    /** Pattern identifying number literal for long "10". */
    Pattern TEN_LONG_LITERAL_RE = Pattern.compile("(?:" + "(?:"
            // binary literal
            + "(?:0b|0B)(?:0[0_]*)?1_*0_*1_*0" + ")|(?:"
            // octal literal
            + "0[0_]*1_*3" + ")|(?:"
            // decimal literal
            + "1_*0" + ")|(?:"
            // hexadecimal literal
            + "(?:0x|0X)(?:0[0_]*)?(?:a|A)" + ")" + ")(?:l|L)?");

}
