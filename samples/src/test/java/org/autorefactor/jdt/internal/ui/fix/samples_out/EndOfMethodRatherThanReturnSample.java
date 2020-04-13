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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

public class EndOfMethodRatherThanReturnSample {
    public void removeUselessReturn() {
    }

    public void removeUselessReturnWithPreviousCode() {
        System.out.println("Keep this line");
    }

    public int doNotRemoveReturnWithValue() {
        return 0;
    }

    public void removeUselessReturnWithIf(boolean isValid) {
        if (isValid) {
            System.out.println("Keep this line");
        }
    }

    public void replaceByBlock(boolean isEnabled) {
        System.out.println("Keep this line");
        if (isEnabled) {
        }
    }

    public void removeElseStatement(boolean isValid) {
        System.out.println("Keep this line");
        if (isValid)
            System.out.println("isValid is true");
    }

    public void removeUselessReturnWithSwitch(int myNumber) {
        switch (myNumber) {
        case 0:
            System.out.println("Keep this line");
        }
    }

    public void doNotRemoveUselessReturnInMiddleOfSwitch(int myNumber) {
        switch (myNumber) {
        case 0:
            System.out.println("I'm not the last statement");
            return;
        case 1:
            System.out.println("Do some stuff");
            break;
        }
    }

    public void removeUselessReturnWithIfElse(boolean isValid) {
        if (isValid) {
            System.out.println("Keep this line");
        } else {
            System.out.println("Remove anyway");
        }
    }

    public void doNotRemoveReturnWithFollowingCode(boolean isValid) {
        if (isValid) {
            System.out.println("Keep this line");
            return;
        }
        System.out.println("Keep this line");
    }

    public void doNotRemoveReturnInWhile(int myNumber) {
        while (myNumber-- > 0) {
            System.out.println("Keep this line");
            return;
        }
    }

    public void doNotRemoveReturnInDoWhile(int myNumber) {
        do {
            System.out.println("Keep this line");
            return;
        } while (myNumber-- > 0);
    }

    public void doNotRemoveReturnInFor() {
        for (int myNumber = 0; myNumber < 10; myNumber++) {
            System.out.println("Keep this line");
            return;
        }
    }

    public void doNotRemoveReturnInForEach(int[] integers) {
        for (int myNumber : integers) {
            System.out.println("Only the first value: " + myNumber);
            return;
        }
    }
}
