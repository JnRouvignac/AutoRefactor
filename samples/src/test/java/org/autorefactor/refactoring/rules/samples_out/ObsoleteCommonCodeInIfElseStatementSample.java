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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

public class ObsoleteCommonCodeInIfElseStatementSample {
    private Date j = new Date();

    /** Common code: i++, Remove if statement */
    public void ifElseRemoveIfNoBrackets(boolean b, int i) {
        // Keep this!
        // Keep this comment
        i++;
    }

    /** Common code: i++, Remove if statement */
    public void ifElseRemoveIf(boolean b, int number) {
        // Keep this comment
        number = number + 1;
    }

    /** No common code, Do not remove anything */
    public void doNotRemoveNotCommonCode(boolean condition, int number1, int number2) {
        if (condition) {
            number1++;
        } else {
            number2++;
        }
    }

    /** Common code: i++, Remove then case */
    public void ifElseRemoveThen(boolean condition, int i, int j) {
        if (!condition) {
            j++;
        }
        // Keep this comment
        ++i;
    }

    /** Common code: i++, Remove else case */
    public void ifElseRemoveElse(boolean b, int i, int j) {
        if (b) {
            j++;
        }
        // Keep this comment
        i++;
    }

    /** Common code: i++, Remove second case */
    public void reverseMiddle(boolean isActive, boolean isEnabled, int i, int j) {
        if (isActive) {
            j++;
        } else if (!isEnabled) {
            j++;
        }
        // Keep this comment
        i++;
    }

    /** Common code: i++, Remove second case */
    public void reverseEmptySecond(boolean isActive, boolean isEnabled, int i, int j) {
        if (isActive) {
            j++;
        } else if (isEnabled) {
        } else if (i > 0) {
            j--;
        } else {
            j++;
        }
        // Keep this comment
        i++;
    }

    /** Only common code, Remove if statement */
    public void ifElseRemoveIfSeveralStatements(boolean b1, boolean b2, int i, int j) {
        // Keep this comment
        i++;
        if (b2 && true) {
            i++;
        } else {
            j++;
        }
    }

    /** Not all cases covered, Do not remove anything */
    public void ifElseIfNoElseDoNotTouch(boolean b, int k, int l) {
        if (b) {
            k++;
            l++;
        } else if (!b) {
            k++;
            l++;
        }
    }

    /** Only common code: remove if statement */
    public void ifElseIfElseRemoveIf(boolean b, int i, int j) {
        // Keep this comment
        i++;
        j++;
    }

    /** Specific code: keep some if statement */
    public void ifElseIfElseRemoveSomeIf(boolean b1, boolean b2, List<String> modifiableList, int i, int j) {
        if (b1) {
        } else if (b2) {
            i++;
        } else if (modifiableList.remove("foo")) {
        }
        // Keep this comment
        i++;
        j++;
    }

    public int doNotRefactorDifferentVariablesInReturn(boolean condition) {
        if (condition) {
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
        }
        o.toString();
    }

    public int doNotRefactorNoElse(boolean b) {
        if (b) {
            return 1;
        }
        return 1;
    }

    public int refactorIfElseInThenClause(boolean b1, boolean b2) {
        return 1;
    }

    public int refactorIfElseInElseClause(boolean b1, boolean b2, int increment) {
        if (b1) {
            increment++;
        } else {
            increment--;
        }
        return increment;
    }

    public int refactorIfElseInElseClauseNoBrackets(boolean b1, boolean b2, int increment) {
        if (b1)      increment++;
        else {
            increment--;
        }
        return increment;
    }

    public int doNotRefactorWithNameConflict(boolean isActive) {
        int i;

        if (isActive) {
            int j = 1;
            i = j + 10;
        } else {
            int j = 1;
            i = j + 10;
        }

        int j = 123;
        System.out.println("Other number: " + j);
        return i;
    }

    public int doNotRefactorWithNameConflictInBlock(boolean isActive) {
        int i;

        if (isActive) {
            int j = 1;
            i = j + 10;
        } else {
            int j = 1;
            i = j + 10;
        }

        if (isActive) {
            int j = 123;
            System.out.println("Other number: " + j);
        }
        return i;
    }

    public int doNotRefactorWithNameConfusion(boolean b) {
        int i;

        if (b) {
            int j = 1;
            i = j + 10;
        } else {
            int j = 1;
            i = j + 10;
        }

        System.out.println("Today: " + j);
        return i;
    }

    public int doNotMoveVarOutsideItsScope(boolean b) {
        if (b) {
            int dontMoveMeIMLocal = 1;
            return dontMoveMeIMLocal + 10;
        } else {
            int dontMoveMeIMLocal = 2;
            return dontMoveMeIMLocal + 10;
        }
    }

    /** Common code: i++, Remove if statement */
    public void ifElseRemoveIfInsideWhile(boolean b, int integer) {
        while (integer < 100) {
            // Keep this comment
            integer = 1 + integer;
        }
    }

    public static Predicate<String> doNotMergeDifferentLambdaExpression(final boolean caseSensitive, final String... allowedSet) {
        if (caseSensitive) {
            return x -> Arrays.stream(allowedSet).anyMatch(y -> (x == null && y == null) || (x != null && x.equals(y)));
        } else {
            Function<String,String> toLower = x -> x == null ? null : x.toLowerCase();
            return x -> Arrays.stream(allowedSet).map(toLower).anyMatch(y -> (x == null && y == null) || (x != null && toLower.apply(x).equals(y)));
        }
    }

    public String refactorExceptFallingThroughCase(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
        } else if (i > 0) {
            return "Do completely other things";
        }
        // Keep this comment
        i++;
        if (isEnabled && true) {
            i++;
        } else {
            j++;
        }

        return "Common code";
    }

    public String refactorExceptFirstCase(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
            return "Do absolutely other things";
        }
        // Keep this comment
        i++;
        if (isEnabled && true) {
            i++;
        } else {
            j++;
        }

        return "Common code";
    }

    public String refactorExceptLastCase(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
        } else if (i <= 0) {
            return "Do completely other stuff";
        }
        // Keep this comment
        i++;
        if (Boolean.TRUE && isEnabled && isValid) {
            i++;
        } else {
            j++;
        }

        return "Common code";
    }

    public String refactorExceptThrow(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
        } else if (i > 0) {
            throw new NullPointerException("Do completely other things");
        }
        // Keep this comment
        i++;
        if (isEnabled && true) {
            i++;
        } else {
            j++;
        }

        return "Common code";
    }

    public String refactorExceptBreak(boolean isValid, boolean isEnabled, int i, int j, List<String> texts) {
        for (String text : texts) {
            if (isValid) {
            } else if (i > 0) {
                break;
            }
            // Keep this comment
            i++;
            if (isEnabled && true) {
                i++;
            } else {
                j++;
            }
        }

        return "Common code";
    }

    public String refactorExceptContinue(boolean isValid, boolean isEnabled, int i, int j, List<String> texts) {
        for (String text : texts) {
            if (isValid) {
            } else if (i > 0) {
                continue;
            }
            // Keep this comment
            i++;
            if (isEnabled && true) {
                i++;
            } else {
                j++;
            }
        }

        return "Common code";
    }

    public String doNotRefactorWithNotFallingThroughCase(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
            i++;
            if (isEnabled && true) {
                i++;
            } else {
                j++;
            }
        } else if (i > 0) {
            "Do completely other things".chars();
        } else {
            i++;
            if (false || !isEnabled) {
                j++;
            } else {
                i++;
            }
        }

        return "Common code";
    }

    public String refactorSomeFallingThroughCases(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
        } else if (i > 0) {
            return "Do completely other things";
        }
        // Keep this comment
        i++;
        if (isEnabled && true) {
            i++;
        } else {
            j++;
        }
        return "Common code";
    }

    public String refactorExceptFallingThroughCases(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
        } else if (i > 0) {
            return "Do completely other things";
        } else if (i > 10) {
            return "Do completely another thing";
        }
        // Keep this comment
        i++;
        if (isEnabled && true) {
            i++;
        } else {
            j++;
        }

        return "Common code";
    }

    public String refactorOtherCases(boolean isValid, boolean isEnabled, int i, int j) {
        if (isValid) {
            i++;
            if (isEnabled && true) {
                i++;
            } else {
                j++;
            }
            return "Common code";
        } else if (i > 0) {
        } else if (i > 10) {
        } else if (i <= 20) {
            i++;
            if (false || !isEnabled) {
                j++;
            } else {
                i++;
            }
            return "Common code";
        }
        // Keep this comment
        return "Do completely other things";
    }

    /** Common code: i++, Remove if statement */
    public void ifElseRemoveIfInSwitch(boolean b, int i, int discriminant) {
        switch (discriminant) {
        case 0:
            // Keep this!
            if (b) {
                i= i * 2;
            } else {
                i= i * 3;
            }
            // Keep this comment
            i++;
        case 1:
            i--;
        }
    }
}
