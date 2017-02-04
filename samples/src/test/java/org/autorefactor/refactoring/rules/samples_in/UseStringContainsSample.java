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
package org.autorefactor.refactoring.rules.samples_in;

public class UseStringContainsSample {

    private boolean replaceStringIndexOfGreaterThanOrEqual(String s) {
        // Keep this comment
        return s.indexOf("test") >= 0;
    }

    private boolean replaceStringIndexOfLesserThan(String s) {
        // Keep this comment
        return s.indexOf("test") < 0;
    }

    private boolean replaceStringIndexOfNotEqualsMinusOne(String s) {
        // Keep this comment
        return s.indexOf("test") != -1;
    }

    private boolean replaceStringIndexOfEqualsMinusOne(String s) {
        // Keep this comment
        return s.indexOf("test") == -1;
    }

    private boolean replaceStringLastIndexOfGreaterThanOrEqual(String s) {
        // Keep this comment
        return s.lastIndexOf("test") >= 0;
    }

    private boolean replaceStringLastIndexOfLesserThan(String s) {
        // Keep this comment
        return s.lastIndexOf("test") < 0;
    }

    private boolean replaceStringLastIndexOfNotEqualsMinusOne(String s) {
        // Keep this comment
        return s.lastIndexOf("test") != -1;
    }

    private boolean replaceStringLastIndexOfEqualsMinusOne(String s) {
        // Keep this comment
        return s.lastIndexOf("test") == -1;
    }

    private boolean doNotReplaceStringIndexOfCharacter(String s) {
        return s.indexOf(':') >= 0;
    }

    private boolean doNotReplaceStringLastIndexOfCharacter(String s) {
        return s.lastIndexOf(':') >= 0;
    }
}
