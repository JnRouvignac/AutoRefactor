/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

public class UseStringContainsSample {
    public boolean replaceStringIndexOf(String s) {
        // Keep this comment
        boolean b1 = s.contains("test");
        // Keep this comment
        boolean b2 = !s.contains("test");
        // Keep this comment
        boolean b3 = s.contains("test");
        // Keep this comment
        boolean b4 = !s.contains("test");
        // Keep this comment
        boolean b5 = s.contains("test");
        // Keep this comment
        boolean b6 = !s.contains("test");

        return b1 && b2 && b3 && b4 && b5 && b6;
    }

    public boolean replaceStringLastIndexOf(String s) {
        // Keep this comment
        boolean b1 = s.contains("test");
        // Keep this comment
        boolean b2 = !s.contains("test");
        // Keep this comment
        boolean b3 = s.contains("test");
        // Keep this comment
        boolean b4 = !s.contains("test");
        // Keep this comment
        boolean b5 = s.contains("test");
        // Keep this comment
        boolean b6 = !s.contains("test");

        return b1 && b2 && b3 && b4 && b5 && b6;
    }

    public boolean doNotReplaceStringIndexOfCharacter(String s) {
        return s.indexOf(':') >= 0;
    }

    public boolean doNotReplaceStringLastIndexOfCharacter(String s) {
        return s.lastIndexOf(':') >= 0;
    }
}
