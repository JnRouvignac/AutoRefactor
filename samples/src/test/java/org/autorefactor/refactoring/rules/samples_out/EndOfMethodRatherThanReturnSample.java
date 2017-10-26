/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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

public class EndOfMethodRatherThanReturnSample {

    public void removeUselessReturn() {
    }

    public void removeUselessReturnWithPreviousCode() {
        System.out.println("Keep this line");
    }

    public int doNotRemoveReturnWithValue() {
        return 0;
    }

    public void removeUselessReturnWithIf(boolean b) {
        if (b) {
            System.out.println("Keep this line");
        }
    }

    public void removeUselessReturnWithIfElse(boolean b) {
        if (b) {
            System.out.println("Keep this line");
        } else {
            System.out.println("Remove anyway");
        }
    }

    public void doNotRemoveReturnWithFollowingCode(boolean b) {
        if (b) {
            System.out.println("Keep this line");
            return;
        }
        System.out.println("Keep this line");
    }

    public void doNotRemoveReturnInWhile(int i) {
        while (i-- > 0) {
            System.out.println("Keep this line");
            return;
        }
    }

    public void doNotRemoveReturnInDoWhile(int i) {
        do {
            System.out.println("Keep this line");
            return;
        } while (i-- > 0);
    }

    public void doNotRemoveReturnInFor() {
        for (int i = 0; i < 10; i++) {
            System.out.println("Keep this line");
            return;
        }
    }

    public void doNotRemoveReturnInForEach(int[] integers) {
        for (int i : integers) {
            System.out.println("Only the first value: " + i);
            return;
        }
    }
}
