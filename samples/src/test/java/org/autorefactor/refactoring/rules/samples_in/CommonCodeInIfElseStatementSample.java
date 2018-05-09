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
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

public class CommonCodeInIfElseStatementSample {

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIfNoBrackets(boolean b, int i) {
        // Keep this!
        if (b)
            // Keep this comment
            i++;
        else
            i = i + 1;
    }

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIf(boolean b, int i) {
        if (b) {
            // Keep this comment
            i = i + 1;
        } else {
            i++;
        }
    }

    /** no common code, Do not remove anything */
    public void doNotRemoveNotCommonCode(boolean b, int i, int j) {
        if (b) {
            i++;
        } else {
            j++;
        }
    }

    /** common code: i++, Remove then case */
    public void ifElseRemoveThen(boolean b, int i, int j) {
        if (b) {
            // Keep this comment
            ++i;
        } else {
            j++;
            // Keep this comment
            i = i + 1;
        }
    }

    /** common code: i++, Remove else case */
    public void ifElseRemoveElse(boolean b, int i, int j) {
        if (b) {
            j++;
            // Keep this comment
            i++;
        } else {
            // Keep this comment
            i++;
        }
    }

    /** only common code, Remove if statement */
    public void ifElseRemoveIfSeveralStatements(boolean b, int i, int j) {
        if (b) {
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
    public void ifElseIfNoElseDoNotTouch(boolean b, int i, int j) {
        if (b) {
            i++;
            j++;
        } else if (!b) {
            i++;
            j++;
        }
    }

    /** only common code: remove if statement */
    public void ifElseIfElseRemoveIf(boolean b, int i, int j) {
        if (b) {
            // Keep this comment
            i++;
            j++;
        } else if (!b) {
            // Keep this comment
            i++;
            j++;
        } else {
            // Keep this comment
            i++;
            j++;
        }
    }

    /** specific code: keep some if statement */
    public void ifElseIfElseRemoveSomeIf(boolean b1, boolean b2, List<String> modifiableList, int i, int j) {
        if (b1) {
            // Keep this comment
            i++;
            j++;
        } else if (b2) {
            i++;
            // Keep this comment
            i++;
            j++;
        } else if (modifiableList.remove("foo")) {
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
            System.out.println(b);
            o.toString();
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
    }

    /** common code: i++, Remove if statement */
    public void ifElseRemoveIfInsideWhile(boolean b, int i) {
        while (i < 100)
            if (b) {
                // Keep this comment
                i = 1 + i;
            } else {
                i++;
            }
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
