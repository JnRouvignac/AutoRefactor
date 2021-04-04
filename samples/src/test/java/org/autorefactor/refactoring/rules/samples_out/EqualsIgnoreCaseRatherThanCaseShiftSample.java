/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

public class EqualsIgnoreCaseRatherThanCaseShiftSample {
    public void replaceCaseShift(String s1, String s2) {
        // Keep this comment
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        "lorem".equalsIgnoreCase("ipsum");
        "lorem".equalsIgnoreCase("ipsum");
    }

    public void simplifyInsensitiveCaseEquality(String s1, String s2) {
        // Keep this comment
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
    }

    public void doNotReplaceUnilateralCaseShift(String s1, String s2) {
        s1.toUpperCase().equals(s2);
        s1.toLowerCase().equals(s2);
        s1.equals(s2.toLowerCase());
        s1.equals(s2.toUpperCase());
    }

    public void doNotReplaceCaseIncompatibility(String s1, String s2) {
        s1.toLowerCase().equals(s2.toUpperCase());
    }
}
