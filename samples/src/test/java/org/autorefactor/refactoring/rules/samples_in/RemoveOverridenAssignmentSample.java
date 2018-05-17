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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.Arrays;
import java.util.List;

public class RemoveOverridenAssignmentSample {
    public boolean removeUselessInitialization() {
        // Keep this comment
        boolean reassignedVar = true;
        reassignedVar = "\n".equals(System.lineSeparator());
        return reassignedVar;
    }

    public long removeInitForLong() {
        // Keep this comment
        long reassignedVar = 0;
        reassignedVar = System.currentTimeMillis();
        return reassignedVar;
    }

    public String removeInitForString() {
        // Keep this comment
        String reassignedVar = "";
        reassignedVar = System.lineSeparator();
        return reassignedVar;
    }

    public long doNotRemoveWithPlusAssignment() {
        long incrementedVar = 123;
        incrementedVar += 456;
        incrementedVar = 789;
        return incrementedVar;
    }

    public long doNotRemoveWithMinusAssignment() {
        long decrementedVar = 123;
        decrementedVar -= 456;
        decrementedVar = 789;
        return decrementedVar;
    }

    public List<String> doNotRemoveActiveInit() {
        List<String> aList = Arrays.asList("lorem", "ipsum");

        boolean reassignedVar = aList.remove("lorem");
        reassignedVar = "\n".equals(System.lineSeparator());
        return aList;
    }

    public String doNotRemoveInitWithoutOverriding() {
        String usedVar = "";
        return usedVar;
    }

    public String doNotRemoveInitWithUse() {
        String usedVar = "";
        System.out.println(usedVar);
        usedVar = System.lineSeparator();
        return usedVar;
    }

    public String doNotRemoveInitWithUseInIf() {
        String usedVar = "";
        if ("\n".equals(System.lineSeparator())) {
            System.out.println(usedVar);
        }
        usedVar = System.lineSeparator();
        return usedVar;
    }

    public String doNotRemoveInitWithCall() {
        String usedVar = "";
        usedVar.length();
        usedVar = System.lineSeparator();
        return usedVar;
    }

    public char[] doNotRemoveInitWithIndex() {
        char[] usedVar = new char[] {'a', 'b', 'c'};
        char oneChar = usedVar[1];
        usedVar = new char[] {'d', 'e', 'f'};
        return usedVar;
    }

    public byte doNotRemoveInitWhenUsed() {
        byte usedVar = 0;
        usedVar = usedVar++;
        return usedVar;
    }

    public String doNotRemoveInitWhenOverriddenInIf() {
        String usedVar = "";
        if ("\n".equals(System.lineSeparator())) {
            usedVar = System.lineSeparator();
        }
        return usedVar;
    }

    public boolean doNotRemoveActiveInitialization(List<String> aList) {
        boolean reassignedActiveVar = aList.remove("foo");
        reassignedActiveVar = "\n".equals(System.lineSeparator());
        return reassignedActiveVar;
    }

    public int doNotRemoveInitializationWithIncrement(int i) {
        int reassignedActiveVar = i++;
        reassignedActiveVar = 123;
        return reassignedActiveVar + i;
    }

    public long doNotRemoveInitializationWithAssignment(long i, long j) {
        long reassignedActiveVar = i = j;
        reassignedActiveVar = 123;
        return reassignedActiveVar + i + j;
    }

    public boolean removePassiveInitialization(int i) {
        // Keep this comment
        boolean reassignedPassiveVar = i > 0;
        reassignedPassiveVar = "\n".equals(System.lineSeparator());
        return reassignedPassiveVar;
    }
}
