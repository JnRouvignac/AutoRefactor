/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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

public class BreakRatherThanPassiveIterationsSample {

    private int crazyInteger = 0;

    public String addBreak(int number) {
        boolean isFound = false;

        for (int i = 0; i < number; i++) {
            if (i == 42) {
                // Keep this comment
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : "The result has not been found";
    }

    public String addBreakInForeachLoop(int[] array) {
        boolean isFound = false;

        for (int i : array) {
            if (i == 42) {
                // Keep this comment
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : "The result has not been found";
    }

    public String addBreakWithoutBlock(int[] array) {
        boolean isFound = false;

        for (int i : array) {
            // Keep this comment
            if (i == 42)
                isFound = true;
        }

        return isFound ? "The result has been found" : "The result has not been found";
    }

    public String addBreakAfterSeveralAssignments(String[] array, boolean isFound, int count) {
        for (String text : array) {
            if (text == null) {
                // Keep this comment
                isFound = true;
                count = 1;
            }
        }

        if (isFound) {
            return "We have found " + count + " result(s)";
        } else {
            return "The result has not been found";
        }
    }

    public String addBreakAfterComplexAssignment(int[] array) {
        int hourNumber = 0;

        for (int dayNumber : array) {
            if (dayNumber == 7) {
                // Keep this comment
                hourNumber = 7 * 24;
            }
        }

        return "Hour number: " + hourNumber;
    }

    public String addBreakWithTemporaryVariable(int number) {
        boolean isFound = false;

        for (int i = 0; i < number; i++) {
            int temporaryInteger = i * 3;

            if (temporaryInteger == 42) {
                // Keep this comment
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : "The result has not been found";
    }

    public String doNotAddBreakWithExternalIterator(int number) {
        boolean isFound = false;
        int i;

        for (i = 0; i < number; i++) {
            if (i == 42) {
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : ("The result has not been found on " + i + " iteration(s)");
    }

    public String doNotAddBreakWithActiveConditions(int number) {
        boolean isFound = false;

        for (int i = 0; i < number--; i++) {
            if (i == 42) {
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : ("The result has not been found on " + number + " iteration(s)");
    }

    public String doNotAddBreakWithActiveUpdater(int number) {
        boolean isFound = false;

        for (int i = 0; i < number; i++, number--) {
            if (i == 42) {
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : ("The result has not been found on " + number + " iteration(s)");
    }

    public String addBreakWithUpdatedIterator(int number) {
        boolean isFound = false;

        for (int i = 0; i < number; i++) {
            if (i++ == 42) {
                // Keep this comment
                isFound = true;
            }
        }

        return isFound ? "The result has been found" : "The result has not been found";
    }

    public String doNotAddBreakWithSeveralConditions(int[] array) {
        int tenFactor = 0;

        for (int i : array) {
            if (i == 10) {
                tenFactor = 1;
            }
            if (i == 100) {
                tenFactor = 2;
            }
        }

        return "The result: " + tenFactor;
    }

    public int doNotAddBreakWithActiveCondition(int[] array, int modifiedInteger) {
        boolean isFound = false;

        for (int i : array) {
            if (i == modifiedInteger++) {
                isFound = true;
            }
        }

        return isFound ? 0 : modifiedInteger;
    }

    public int doNotAddBreakWithActiveAssignment(int[] array, int modifiedInteger) {
        int result = 0;

        for (int i : array) {
            if (i == 42) {
                result = modifiedInteger++;
            }
        }

        return result;
    }

    public int doNotAddBreakWithVariableAssignment(int[] array) {
        int result = 0;

        new Thread() {
            @Override
            public void run() {
                while (crazyInteger++ < 10000) {}
            }
        }.start();

        for (int i : array) {
            if (i == 42) {
                result = crazyInteger;
            }
        }

        return result;
    }
}