/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

public class CommonCodeInIfElseStatementSample {

    /** no code at all, remove all */
    public void emptyIfOrElseClauses(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            System.out.println();
        } else {
        }
    }

    /** no common code, Do not remove anything */
    public void ifElseRemoveIf(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            i++;
        } else {
            j++;
        }
    }

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIfNoBrackets(Boolean b, int i) {
        // keep this comment
        i++;
    }

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIf(Boolean b, int i) {
        // keep this comment
        i++;
    }

    /** common code: i++, Remove then case */
    public void ifElseRemoveThen(Boolean b, int i, int j) {
        // keep this comment
        i++;
        if (!b.booleanValue()) {
            j++;
        }
    }

    /** common code: i++, Remove else case */
    public void ifElseRemoveElse(Boolean b, int i, int j) {
        // keep this comment
        i++;
        if (b.booleanValue()) {
            j++;
        }
    }

    /**
     * common code: put i++ before if statement, put l++ after if statement. Do
     * not remove if statement.
     */
    public void ifElseRemoveIf(Boolean b, int i, int j, int k, int l) {
        // keep this comment
        i++;
        if (b.booleanValue()) {
            j++;
        } else {
            k++;
        }
        // keep this comment
        l++;
    }

    /** only common code, Remove if statement */
    public void ifElseRemoveIfSeveralStatements(Boolean b, int i, int j) {
        // keep this comment
        i++;
        j++;
    }

    /** not all cases covered, Do not remove anything */
    public void ifElseIfNoElseDoNotTouch(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            i++;
            j++;
        } else if (!b.booleanValue()) {
            i++;
            j++;
        }
    }

    /** only common code: remove if statement */
    public void ifElseIfElseRemoveIf(Boolean b, int i, int j) {
        // keep this comment
        i++;
        j++;
    }

    public int doNotRefactorDifferentVariablesInReturn(boolean b) {
        if (b) {
            int i = 1;
            return i;
        } else {
            int i = 2;
            return i;
        }
    }

    public void refactorMethodInvocatoin(boolean b, Object o) {
        o.toString();
        if (b) {
            System.out.println(b);
        }
    }
}
