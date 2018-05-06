/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.function.Function;
import java.util.function.Predicate;

public class CommonCodeInIfElseStatementSample {

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIfNoBrackets(Boolean b, int i) {
        // Keep this!
        if (b.booleanValue())
            // Keep this comment
            i++;
        else
            i = i + 1;
    }

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIf(Boolean b, int i) {
        if (b.booleanValue()) {
            // Keep this comment
            i = i + 1;
        } else {
            i++;
        }
    }

    /** no common code, Do not remove anything */
    public void doNotRemoveNotCommonCode(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            i++;
        } else {
            j++;
        }
    }

    /** common code: i++, Remove then case */
    public void ifElseRemoveThen(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            // Keep this comment
            ++i;
        } else {
            // Keep this comment
            i = i + 1;
            j++;
        }
    }

    /** common code: i++, Remove else case */
    public void ifElseRemoveElse(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            // Keep this comment
            i++;
            j++;
        } else {
            // Keep this comment
            i++;
        }
    }

    /**
     * common code: put i++ before if statement, put l++ after if statement. Do
     * not remove if statement.
     */
    public void ifElseRemoveIf(Boolean b, int i, int j, int k, int l) {
        if (b.booleanValue()) {
            // Keep this comment
            i--;
            j++;
            // Keep this comment
            l++;
        } else {
            // Keep this comment
            i = i - 1;
            k++;
            // Keep this comment
            l++;
        }
    }

    /** only common code, Remove if statement */
    public void ifElseRemoveIfSeveralStatements(Boolean b, int i, int j) {
        if (b.booleanValue()) {
            // Keep this comment
            i++;
            j++;
        } else {
            // Keep this comment
            i++;
            j++;
        }
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
        if (b.booleanValue()) {
            // Keep this comment
            i++;
            j++;
        } else if (!b.booleanValue()) {
            // Keep this comment
            i++;
            j++;
        } else {
            // Keep this comment
            i++;
            j++;
        }
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
        if (b) {
            o.toString();
            System.out.println(b);
        } else {
            o.toString();
        }
    }

    public int doNotRefactorNoElse(boolean b) {
        if (b) {
            return 1;
        }
        return 1;
    }

    public int refactorIfElseInThenClause(boolean b1, boolean b2) {
        if (b1) {
            if (b2) {
                return 1;
            } else {
                return 1;
            }
        } else {
            return 1;
        }
    }

    public int refactorIfElseInThenClauseNoBrackets(boolean b1, boolean b2) {
        if (b1)
            if (b2)
                return 1;
        else
            return 1;
        return 1;
        // FIXME
        // Should be refactored into this unique statement:
        // return 1;
    }

    public int refactorIfElseInElseClause(boolean b1, boolean b2) {
        if (b1) {
            return 1;
        } else if (b2) {
            return 2;
        } else {
            return 2;
        }
    }

    public int refactorIfElseInElseClauseNoBrackets(boolean b1, boolean b2) {
        if (b1)      return 1;
        else if (b2) return 2;
        else         return 2;
        // FIXME code above should be refactored to:
        // if (b1)      return 1;
        // else         return 2;
    }

    public static Predicate<String> doNotMergeDifferentLambdaExpr(final boolean caseSensitive, final String... allowedSet) {
        if (caseSensitive) {
            return x -> Arrays.stream(allowedSet).anyMatch(y -> (x == null && y == null) || (x != null && x.equals(y)));
        } else {
            Function<String,String> toLower = x -> x == null ? null : x.toLowerCase();
            return x -> Arrays.stream(allowedSet).map(toLower).anyMatch(y -> (x == null && y == null) || (x != null && toLower.apply(x).equals(y)));
        }
    }
}
