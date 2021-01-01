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

public class DoubleNegationSample {
    public boolean reduceBooleanExpression(boolean b1, boolean b2) {
        boolean b3 = !b1 == !b2;
        boolean b4 = !b1 != !b2;
        boolean b5 = !b1 ^ !b2;
        boolean b6 = !b1 == b2;
        boolean b7 = !b1 != b2;
        boolean b8 = !b1 ^ b2;
        boolean b9 = b1 == !b2;
        boolean b10 = b1 != !b2;
        boolean b11 = b1 ^ !b2;
        return b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;
    }

    public boolean doNotRefactorPositiveExpression(boolean isValid, boolean isEnabled) {
        boolean b1 = isValid == isEnabled;
        boolean b2 = isValid != isEnabled;
        return b1 && b2;
    }
}
