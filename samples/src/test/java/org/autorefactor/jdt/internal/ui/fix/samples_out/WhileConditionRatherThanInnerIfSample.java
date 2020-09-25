/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class WhileConditionRatherThanInnerIfSample {
    public void moveConditionIntoWhileCondition(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && !isEnabled) {
            i++;
        }
    }

    public void moveOppositeConditionIntoWhileCondition(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && isEnabled) {
            System.out.println("Keep this code");
            i++;
        }
    }

    public void moveConditionIntoWhileConditionAndKeepCode(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && !isEnabled) {
            System.out.println("Keep this code");
            i++;
        }
    }

    public void moveConditionAndSeveralStatements(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && !isEnabled) {
            System.out.println("Keep this code");
            System.out.println("And this code too!");
            i++;
        }
    }

    public void moveComplexConditionIntoWhileCondition(int i, int j) {
        // Keep this comment
        while ((i < 10) && !(j == 0)) {
            i++;
        }
    }

    public void moveConditionWithoutBrackets(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && !isEnabled) {
            i++;
        }
    }

    public void moveConditionIntoUnbrackettedWhileCondition(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && !isEnabled) {
        }
    }

    public void moveConditionIntoUnbrackettedWhileConditionAndMoveCode(int i, boolean isEnabled) {
        // Keep this comment
        while ((i < 10) && !isEnabled) {
            System.out.println("Keep this code");
            System.out.println("And this code too!");
        }
    }

    public void doNotMoveConditionThatIsNotAtTheBeginning(int i, boolean isEnabled) {
        while (i < 10) {
            i++;
            if (isEnabled) {
                break;
            }
        }
    }

    public void doNotMoveConditionThatReturns(int i, boolean isEnabled) {
        while (i < 10) {
            if (isEnabled) {
                return;
            }
            i++;
        }

        System.out.println("Don't forget me!");
    }

    public void doNotMoveConditionWithLabel(int i, int j, boolean isEnabled) {
        badGoto: while (j > 10) {
            while (i < 10) {
                if (isEnabled) {
                    break badGoto;
                }
                i++;
                j++;
            }
        }
    }

    public void doNotMoveConditionInDoWhileLoop(int i, boolean isEnabled) {
        do {
            if (isEnabled) {
                break;
            }
            i++;
        } while (i < 10);
    }

    public void doNotMoveConditionWithCode(int i, boolean isEnabled) {
        while (i < 10) {
            if (isEnabled) {
                System.out.println("Don't forget me!");
                break;
            }
            i++;
        }
    }
}
