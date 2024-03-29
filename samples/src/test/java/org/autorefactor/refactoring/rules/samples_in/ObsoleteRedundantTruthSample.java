/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2020 Jean-Noël Rouvignac - initial API and implementation
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

public class ObsoleteRedundantTruthSample {
    public void simplifyPrimitiveBooleanExpression(boolean isValid) {
        if (isValid == true) {
            int i = 0;
        }

        if (isValid != false) {
            int i = 0;
        }

        if (isValid == false) {
            int i = 0;
        }

        if (isValid != true) {
            int i = 0;
        }

        if (isValid == Boolean.TRUE) {
            int i = 0;
        }

        if (isValid != Boolean.FALSE) {
            int i = 0;
        }

        if (isValid == Boolean.FALSE) {
            int i = 0;
        }

        if (isValid != Boolean.TRUE) {
            int i = 0;
        }
    }

    public void removeParenthesis(boolean isValid, boolean isActive) {
        if ((isValid == true) == isActive) {
            int i = 0;
        }

        if (isActive == (isValid == true)) {
            int i = 0;
        }

        if ((isValid == true) != isActive) {
            int i = 0;
        }

        if (isActive != (isValid == true)) {
            int i = 0;
        }

        if ((isValid == false) == isActive) {
            int i = 0;
        }

        if (isActive == (isValid == false)) {
            int i = 0;
        }

        if ((isValid == false) != isActive) {
            int i = 0;
        }

        if (isActive != (isValid == false)) {
            int i = 0;
        }
    }

    public void simplifyBooleanWrapperExpression(Boolean isValid) {
        if (isValid == true) {
            int i = 0;
        }

        if (isValid != false) {
            int i = 0;
        }

        if (isValid == false) {
            int i = 0;
        }

        if (isValid != true) {
            int i = 0;
        }
    }

    public int doNotSimplifyBooleanWrapperExpression(Boolean isValid) {
        if (isValid == Boolean.TRUE) {
            return 1;
        }

        if (isValid != Boolean.FALSE) {
            return 2;
        }

        if (isValid == Boolean.FALSE) {
            return 3;
        }

        if (isValid != Boolean.TRUE) {
            return 4;
        }

        return 0;
    }
}
