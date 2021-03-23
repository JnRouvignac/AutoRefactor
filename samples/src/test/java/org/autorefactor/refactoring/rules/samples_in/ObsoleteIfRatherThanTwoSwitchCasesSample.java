/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.time.DayOfWeek;

public class ObsoleteIfRatherThanTwoSwitchCasesSample {
    public int i2 = 0;

    public void replaceSwitchWithParameterDiscriminant(int i1) {
        int i = 0;
        // Keep this comment
        switch (i1) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            i = 0;
            break;

        case 3:
        default:
            if (i2 == 2) {
                // Keep this comment also
                i = 150;
            }
            break;
        }
    }

    public void replaceSwitchWithoutBreakAtTheEnd(int i1) {
        int i = 0;
        // Keep this comment
        switch (i1) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            i = 0;
            break;

        case 3:
        default:
            if (i2 == 2) {
                // Keep this comment also
                i = 150;
            }
        }
    }

    public void replaceSwitchWithoutStatementsAtTheEnd(int i1) {
        int i = 0;
        // Keep this comment
        switch (i1) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            i = 0;
            break;

        case 3:
        default:
        }
    }

    public void replaceSwitchWithDefault(int i1) {
        int i = 0;
        // Keep this comment
        switch (i1) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            i = 0;
            break;

        default:
            // Keep this comment also
            i = 150;
            break;
        }
    }

    public void replaceSwitchWithEmptyDefault(int i1) {
        int i = 0;
        // Keep this comment
        switch (i1) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            i = 0;
            break;

        default:
            break;
        }
    }

    public void replaceSwitchWithLocalVariableDiscriminant() {
        int i1 = 0;
        int i = 0;
        // Keep this comment
        switch (i1) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            i = 0;
            break;

        default:
        case 3:
            if (i2 == 2) {
                // Keep this comment also
                i = 150;
            }
            break;
        }
    }

    public void replaceSwitchWithFieldDiscriminant() {
        int i = 0;
        // Keep this comment
        switch (i2) {
        case 0:
            // Keep this comment too
            i = 0;
            // Keep this comment also
            break;

        case 1:
            i = 10;
            break;
        }
    }

    public void replaceSwitchWithCharacterDiscriminant(char a1) {
        int i = 0;
        // Keep this comment
        switch (a1) {
        case 'b':
        case 'c':
        case 'd':
        default:
            i = 40;
            break;

        case 'a':
            // Keep this comment too
            i = 0;
            // Keep this comment also
            break;
        }
    }

    public void replaceSwitchWithStringDiscriminant(String text) {
        int i = 0;
        // Keep this comment
        switch (text) {
        case "foo":
        case "bar":
            // Keep this comment too
            i = 0;
            break;

        case "foo bar":
        default:
            if (i2 == 2) {
                // Keep this comment also
                i = 150;
            }
            break;
        }
    }

    public void replaceSwitchWithEnumDiscriminant(DayOfWeek day) {
        int i = 0;
        // Keep this comment
        switch (day) {
        case MONDAY:
        case TUESDAY:
        case WEDNESDAY:
            // Keep this comment too
            i = 0;
            break;

        case THURSDAY:
        default:
            if (i2 == 2) {
                // Keep this comment also
                i = 150;
            }
            break;
        }
    }

    public int replaceSwitchWithReturn(int i) {
        // Keep this comment
        switch (i) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            return 0;

        case 3:
        default:
            if (i2 == 2) {
                // Keep this comment also
                return 150;
            }
            break;
        }

        return 10;
    }

    public int replaceSwitchWithThrownExceptions(int i) {
        // Keep this comment
        switch (i) {
        case 0:
        case 1:
        case 2:
            // Keep this comment too
            throw new NullPointerException();

        case 3:
        default:
            if (i2 == 2) {
                // Keep this comment also
                throw new IllegalArgumentException();
            }
            break;
        }

        return 10;
    }

    public void replaceSwitchKeepExistingControlFlowBreaks(byte i1) {
        byte j = 0;
        loop: for (byte i = 0; i < 10; i++) {
            switch (i1) {
            case 0:
            case 1:
                j = 10;
                continue;

            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
                j = 20;
                break loop;
            }
        }
    }

    public void replaceWithInnerLoopBreak(short i1) {
        short j = 0;
        switch (i1) {
        case 0:
        case 1:
            j = 10;
            short k = 0;
            do {
                if (j == i1) {
                    break;
                }
                k++;
            } while (k < j);
            break;

        case 2:
        case 3:
        case 4:
            j = 40;
            for (short o : new short[] { 1, 2, 3 }) {
                if (o == i1) {
                    break;
                }
            }
            break;
        }
    }

    public void doNotReplaceSwitchCaseThatContinues(int i1) {
        int i = 0;
        switch (i1) {
        case 0:
        case 1:
        case 2:
            i = 0;

        case 3:
        default:
            if (i2 == 2) {
                i = 150;
            }
            break;
        }
    }

    public void doNotReplaceSwitchWithMoreThanTwoDistinctCases(int i1) {
        int i = 0;
        switch (i1) {
        case 0:
        case 1:
        case 2:
            i = 0;
            break;

        case 3:
            i = 10;
            break;

        case 4:
        default:
            if (i2 == 2) {
                i = 150;
            }
            break;
        }
    }

    public void doNotRefactorReusedVariable(int discriminant) {
        switch (discriminant) {
        case 0:
            String reusedVariable = "I'm created here";
            System.out.println(reusedVariable);
            break;

        default:
            reusedVariable = "And reused there";
            System.out.println(reusedVariable);
            break;
        }
    }
}
