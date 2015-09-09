/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

public class ReduceVariableScopeSample {

    public void reduceScopeSameScopeSameLevel(long l) {
        int i = 0;
        System.out.println(l);
        System.out.println(i);
    }

    public void doNotReduceScopeSameScopeDifferentLevel(long l) {
        int i = 0;
        {
            i++;
        }
        System.out.println(i);
    }

    public void removeUnusedVariable(long l) {
        int i = 0;
        System.out.println(l);
    }

    public void removeDeadVariableOnlyRemoveFragment() {
        int i, j = 0; // TODO remove i
        System.out.println(j);
    }

    public void removeUnusedVariableOnlyRemoveFragment() {
        int i = 0, j = 0;
        System.out.println(j);
    }

    public void removeDeadStoreToVariable1(int i, long l) {
        i = 0;
        System.out.println(l);
    }

    public void removeDeadStoreToVariable2(int i, long l) {
        i = 0;
        i = 1;
        System.out.println(i);
    }

    public static void main(String[] args) {
        // push variable into for loops initializers
        int i;
        {
            i = 0;
            System.out.println(i);
        }
        for (i = 0; i < args.length; i++) {
            System.out.println(i);
        }
        for (i = 0; i < args.length; i++)
            System.out.println(i);
        for (Object obj : (List) null) {
            System.out.println(i);
        }
        for (Object obj : (List) null)
            System.out.println(i);
        if (isOk()) {
            System.out.println(i);
        }
        if (isOk())
            System.out.println(i = 0);
        while (isOk()) {
            i = 0;
            System.out.println(i);
        }
        while (isOk())
            System.out.println(i = 0);
    }

    private static boolean isOk() {
        return false;
    }

    private static void doIt() {
    }
}
