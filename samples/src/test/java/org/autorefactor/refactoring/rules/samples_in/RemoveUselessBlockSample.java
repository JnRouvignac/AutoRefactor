/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
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

public class RemoveUselessBlockSample {
    {
        System.out.println("Don't touch to the static initializer");
    }

    public int removeUselessBlock(int i) {
        // Keep this comment
        {
            // Keep this comment too
            return i + 1;
        }
    }

    public int removeUselessBlockWithForwardCode(int i) {
        // Keep this comment
        {
            // Keep this comment too
            System.out.println("The next value of " + i);
        }
        return i + 1;
    }

    public int removeUselessBlockWithPreviousCode(int i) {
        System.out.println("Let's start");
        // Keep this comment
        {
            // Keep this comment too
            System.out.println("The next value of " + i);
        }
        return i + 1;
    }

    public int doNotRemoveBlockWithDuplicateVariable() {
        {
            int i = 0;
            System.out.println("The next value of " + i);
        }
        int i = 0;
        return i + 1;
    }

    public int removeLastBlockWithDuplicateVariable() {
        {
            int i = 0;
            System.out.println("The next value of " + i);
        }
        // Keep this comment
        {
            // Keep this comment too
            int i = 0;
            return i + 1;
        }
    }

    public void doNotRemoveIfBlock(int i) {
        if (i > 0) {
            System.out.println("i is positive.");
        }
    }
}