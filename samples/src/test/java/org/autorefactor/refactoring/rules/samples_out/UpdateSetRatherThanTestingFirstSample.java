/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Separate the code.
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

import java.util.List;
import java.util.Set;

public class UpdateSetRatherThanTestingFirstSample {

    public void replaceCheckOnSetNotContainsBeforeAdd(Set<String> col, String s) {
        // Keep this comment
        if (col.add(s)) {
            System.out.println("OK");
        } else {
            System.out.println("KO");
        }
    }

    public void replaceCheckOnSetContainsBeforeAdd(Set<String> col, String s) {
        // Keep this comment
        if (!col.add(s)) {
            System.out.println("KO");
        } else {
            System.out.println("OK");
        }
    }

    public void replaceCheckOnSetContainsOneAddStatement(Set<String> col, String s) {
        // Keep this comment
        col.add(s);
    }

    public void replaceCheckOnSetNotContainsOneAddStatement(Set<String> col, String s) {
        // Keep this comment
        col.add(s);
    }

    public void replaceCheckOnSetContainsBeforeRemove(Set<String> col, String s) {
        // Keep this comment
        if (!col.remove(s)) {
            System.out.println("KO");
        } else {
            System.out.println("OK");
        }
    }

    public void replaceCheckOnSetNotContainsBeforeRemove(Set<String> col, String s) {
        // Keep this comment
        if (col.remove(s)) {
            System.out.println("OK");
        } else {
            System.out.println("KO");
        }
    }

    public void replaceCheckOnSetContainsOneRemoveStatement(Set<String> col, String s) {
        // Keep this comment
        col.remove(s);
    }

    public void replaceCheckOnSetNotContainsOneRemoveStatement(Set<String> col, String s) {
        // Keep this comment
        col.remove(s);
    }

    public void doNotReplaceWhenCheckedValueIsDifferent(Set<String> col) {
        if (!col.contains("this")) {
            col.add("that");
            System.out.println("OK");
        }
        if (col.contains("this")) {
            col.remove("that");
            System.out.println("OK");
        }
    }

    public void doNotReplaceWhenCollectionsAreDifferent(Set<String> col1, Set<String> col2) {
        if (!col1.contains("that")) {
            col2.add("that");
            System.out.println("OK");
        }
        if (col1.contains("that")) {
            col2.remove("that");
            System.out.println("OK");
        }
    }

    public void doNotReplaceCheckOnListContainsBeforeAdd(List<String> col, String s) {
        if (!col.contains(s)) {
            col.add(s);
            System.out.println("OK");
        } else {
            System.out.println("KO");
        }
        if (!col.contains(s)) {
            System.out.println("KO");
        } else {
            col.remove(s);
            System.out.println("OK");
        }
    }
}
