/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.util;

/**
 * Utility class containing miscellaneous helper methods.
 */
public final class Utils {

    private Utils() {
        // utility class ctor is private
    }

    /**
     * Returns whether the two provided objects are equal.
     *
     * @param obj1 the first object to compare for equality
     * @param obj2 the second object to compare for equality
     * @return true if the two provided objects are equal, false otherwise.
     */
    public static boolean equal(Object obj1, Object obj2) {
        if (obj1 == null) {
            return obj2 == null;
        }
        return obj1.equals(obj2);
    }

    /**
     * Returns whether the two provided objects are equal and not null.
     *
     * @param obj1 the first object to compare for equality
     * @param obj2 the second object to compare for equality
     * @return true if the two provided objects are equal and not null, false otherwise.
     */
    public static boolean equalNotNull(Object obj1, Object obj2) {
        return obj1 != null && obj1.equals(obj2);
    }

    /**
     * Returns whether the two provided booleans are equal.
     *
     * @param b1 the first boolean to compare for equality
     * @param b2 the second boolean to compare for equality
     * @return true if the two provided booleans are equal, false otherwise.
     */
    public static boolean equal(boolean b1, boolean b2) {
        return b1 == b2;
    }

    /**
     * Returns whether the two provided bytes are equal.
     *
     * @param b1 the first byte to compare for equality
     * @param b2 the second byte to compare for equality
     * @return true if the two provided bytes are equal, false otherwise.
     */
    public static boolean equal(byte b1, byte b2) {
        return b1 == b2;
    }

    /**
     * Returns whether the two provided objects are equal.
     *
     * @param c1 the first character to compare for equality
     * @param c2 the second character to compare for equality
     * @return true if the two provided characters are equal, false otherwise.
     */
    public static boolean equal(char c1, char c2) {
        return c1 == c2;
    }

    /**
     * Returns whether the two provided shorts are equal.
     *
     * @param s1 the first short to compare for equality
     * @param s2 the second short to compare for equality
     * @return true if the two provided shorts are equal, false otherwise.
     */
    public static boolean equal(short s1, short s2) {
        return s1 == s2;
    }

    /**
     * Returns whether the two provided integers are equal.
     *
     * @param i1 the first integer to compare for equality
     * @param i2 the second integer to compare for equality
     * @return true if the two provided integers are equal, false otherwise.
     */
    public static boolean equal(int i1, int i2) {
        return i1 == i2;
    }

    /**
     * Returns whether the two provided longs are equal.
     *
     * @param l1 the first long to compare for equality
     * @param l2 the second long to compare for equality
     * @return true if the two provided longs are equal, false otherwise.
     */
    public static boolean equal(long l1, long l2) {
        return l1 == l2;
    }

    /**
     * Returns whether the two provided floats are equal.
     *
     * @param f1 the first float to compare for equality
     * @param f2 the second float to compare for equality
     * @return true if the two provided floats are equal, false otherwise.
     */
    public static boolean equal(float f1, float f2) {
        return f1 == f2;
    }

    /**
     * Returns whether the two provided doubles are equal.
     *
     * @param d1 the first double to compare for equality
     * @param d2 the second double to compare for equality
     * @return true if the two provided doubles are equal, false otherwise.
     */
    public static boolean equal(double d1, double d2) {
        return d1 == d2;
    }

}
