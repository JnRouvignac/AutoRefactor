/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
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

public class MergeConditionalBlocksSample {

    /** Duplicate if and else if code, merge it */
    public void duplicateIfAndElseIf(int i) {
        // Keep this comment
        if ((i == 0) || (i == 1)) {
            // Keep this comment too
            System.out.println("Duplicate");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }

    /** Duplicate if and else code, merge it */
    public void duplicateIfAndElse(int i) {
        // Keep this comment
        if ((i == 0) || !(i == 1)) {
            // Keep this comment too
            System.out.println("Duplicate");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }

    /** Duplicate if and else if code, merge it */
    public void duplicateIfAndElseIfWithoutElse(int i) {
        // Keep this comment
        if ((i == 0) || (i == 1)) {
            // Keep this comment too
            System.out.println("Duplicate");
        }
    }

    /** Duplicate else if codes, merge it */
    public void duplicateIfAndElseIfAmongOther(int i) {
        // Keep this comment
        if (i == 0) {
            // Keep this comment too
            System.out.println("A given code");
        } if ((i == 1) || (i == 2)) {
            // Keep this comment too
            System.out.println("Duplicate");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }

    /** Duplicate if and else if code, merge it */
    public void duplicateSingleStatement(int i) {
        // Keep this comment
        if ((i == 0) || (i == 1))
            // Keep this comment too
            System.out.println("Duplicate");
        else
            // Keep this comment also
            System.out.println("Different");
    }

    /** Hardly identified elements */
    public void doNotCreateMalFormedTree(boolean b1, boolean b2) {
        if (b1) {
        } else if (b2) {
            ;
        }
//
    }

    /** Duplicate if and else if code, merge it */
    public void numerousDuplicateIfAndElseIf(int i) {
        // Keep this comment
        if ((((i == 0) || (i == 1)) || (i == 2)) || (i == 3)) {
            // Keep this comment too
            System.out.println("Duplicate");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }

    /** Duplicate if and else if code, merge it */
    public void complexIfAndElseIf(int i) {
        // Keep this comment
        if (((i == 0) || (i == 1 || i == 2)) || (i > 10)) {
            // Keep this comment too
            System.out.println("Duplicate");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }

    /** Duplicate if and else if code, merge it */
    public void longIfAndElseIf(int i) {
        // Keep this comment
        if ((i == 0) || (i == 1)) {
            // Keep this comment too
            System.out.println("Duplicate");
            System.out.println("code");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }

    /** Different if and else if code, leave it */
    public void doNotMergeSameCode(int i) {
        // Keep this comment
        if (i == 0) {
            // Keep this comment too
            System.out.println("Duplicate");
        } else if (i == 1) {
            // Keep this comment too
            System.out.println("Duplicate");
            System.out.println("but not only");
        } else {
            // Keep this comment also
            System.out.println("Different");
        }
    }
}
