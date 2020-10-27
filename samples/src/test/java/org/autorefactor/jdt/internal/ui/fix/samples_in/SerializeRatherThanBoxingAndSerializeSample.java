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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

public class SerializeRatherThanBoxingAndSerializeSample {
    public String simplifyIntegerSerialization(int number) {
        // Keep this comment
        return Integer.valueOf(number).toString();
    }

    public String simplifyDoubleSerialization(double number) {
        // Keep this comment
        return Double.valueOf(number).toString();
    }

    public String simplifyFloatSerialization(float number) {
        // Keep this comment
        return Float.valueOf(number).toString();
    }

    public String simplifyShortSerialization(short number) {
        // Keep this comment
        return Short.valueOf(number).toString();
    }

    public String simplifyLongSerialization(long number) {
        // Keep this comment
        return Long.valueOf(number).toString();
    }

    public String simplifyCharacterSerialization(char number) {
        // Keep this comment
        return Character.valueOf(number).toString();
    }

    public String simplifyByteSerialization(byte number) {
        // Keep this comment
        return Byte.valueOf(number).toString();
    }

    public String simplifyBooleanSerialization(boolean number) {
        // Keep this comment
        return Boolean.valueOf(number).toString();
    }

    public String refactorIntegerInstantiation(int number) {
        // Keep this comment
        return new Integer(number).toString();
    }

    public String refactorIntegerCast(int number) {
        // Keep this comment
        return ((Integer) number).toString();
    }

    public String doNotRefactorWrapper(Integer number) {
        return Integer.valueOf(number).toString();
    }

    public String doNotRefactorString(String number) {
        return Integer.valueOf(number).toString();
    }

    public String doNotRefactorBadMethod(int number) {
        return Integer.valueOf(number).toBinaryString(0);
    }
}
