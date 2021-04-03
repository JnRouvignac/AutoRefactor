/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

public class ObsoletePrimitiveComparisonRatherThanWrapperComparisonSample {
    public int simplifyIntegerComparison(int number, int anotherNumber) {
        // Keep this comment
        return Integer.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyDoubleComparison(double number, double anotherNumber) {
        // Keep this comment
        return Double.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyFloatComparison(float number, float anotherNumber) {
        // Keep this comment
        return Float.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyShortComparison(short number, short anotherNumber) {
        // Keep this comment
        return Short.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyLongComparison(long number, long anotherNumber) {
        // Keep this comment
        return Long.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyCharacterComparison(char number, char anotherNumber) {
        // Keep this comment
        return Character.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyByteComparison(byte number, byte anotherNumber) {
        // Keep this comment
        return Byte.valueOf(number).compareTo(anotherNumber);
    }

    public int simplifyBooleanComparison(boolean number, boolean anotherNumber) {
        // Keep this comment
        return Boolean.valueOf(number).compareTo(anotherNumber);
    }

    public int refactorIntegerInstantiation(int number, int anotherNumber) {
        // Keep this comment
        return new Integer(number).compareTo(anotherNumber);
    }

    public int refactorIntegerCast(int number, int anotherNumber) {
        // Keep this comment
        return ((Integer) number).compareTo(anotherNumber);
    }

    public int doNotRefactorWrapper(Integer number, int anotherNumber) {
        return Integer.valueOf(number).compareTo(anotherNumber);
    }

    public int doNotRefactorWrapperComparator(int number, Integer anotherNumber) {
        return Integer.valueOf(number).compareTo(anotherNumber);
    }

    public int doNotRefactorString(String number, int anotherNumber) {
        return Integer.valueOf(number).compareTo(anotherNumber);
    }

    public int doNotRefactorBadMethod(int number, int anotherNumber) {
        return Integer.valueOf(number).valueOf(anotherNumber);
    }
}
