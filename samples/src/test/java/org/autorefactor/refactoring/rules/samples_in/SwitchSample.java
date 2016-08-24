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
package org.autorefactor.refactoring.rules.samples_in;

public class SwitchSample {

    public int i2 = 0;

    public void replaceIfWithParameter(int i1) {
        int i = 0;
        if (i1 == 0) {
            // Keep this comment 2
            i = 0;
            // Keep this comment 3
        } else if (i1 == 1) {
            i = 10;
        } else if (2 == i1) {
            i = 20;
        } else if (i1 == 3) {
            i = 25;
            i = 30;
        } else if (i1 == 4)
            i = 40;
        else if ((i1 == 5) || (i1 == 6)) {
            i = 60;
        } else if ((i1 == 7) ^ (i1 == 8)) {
            i = 80;
        } else if ((i1 == 9) | (i1 == 10)) {
            i = 100;
        } else if ((i1 == 11) || i1 == 12 || (i1 == 13)) {
            i = 130;
        } else if (14 == i1) {
            if (i2 == 1) {
                i = 140;
            }
        } else if (i2 == 2) {
            i = 150;
        }
    }

    public void replaceIfWithLocalVariable() {
        int i1 = 0;
        int i = 0;
        if (i1 == 0) {
            // Keep this comment 2
            i = 0;
            // Keep this comment 3
        } else if (i1 == 1) {
            i = 10;
        } else if (2 == i1) {
            i = 20;
        } else if (i1 == 3) {
            i = 25;
            i = 30;
        } else if (i1 == 4)
            i = 40;
        else {
            i = 50;
        }
    }

    public void replaceIfWithField() {
        int i = 0;
        if (i2 == 0) {
            // Keep this comment 2
            i = 0;
            // Keep this comment 3
        } else if (i2 == 1) {
            i = 10;
        }
    }

    public void replaceIfWithCharacter(char a1) {
        int i = 0;
        if (a1 == 'a') {
            // Keep this comment 2
            i = 0;
            // Keep this comment 3
        } else if (a1 == 'b')
            i = 10;
        else if ('c' == a1) {
            i = 20;
        } else if (a1 == 'd') {
            i = 30;
        } else
            i = 40;
    }

    public void replaceIfWithDuplicateConditions(char a1) {
        int i = 0;
        if (a1 == 'a') {
            i = 0;
        } else if (a1 == 'b') {
            i = 10;
        } else if (a1 == 'a') {
            i = 20;
        } else if (a1 == 'b') {
            i = 30;
        } else if ('c' == a1) {
            i = 40;
        } else if (a1 == 'd' || a1 == 'b' || ('c' == a1)) {
            i = 50;
        } else {
            i = 60;
        }
    }

    public void replaceIfWithExistingBreaks(int i1) {
        int j = 0;
        for (int i = 0; i < 10; i++) {
            if (i1 == 0) {
                j = 0;
                return;
            } else if (i1 == 1) {
                j = 10;
                continue;
            } else if (2 == i1) {
                j = 20;
                break;
            } else if (i1 == 3) {
                j = 25;
                j = 30;
            } else if (4 == i1) {
                j = 40;
                throw new RuntimeException();
            } else if (5 == i1) {
                j = 50;
                if (i == 5) {
                    throw new RuntimeException();
                }
            }
        }
    }

    public void replaceIfWithCompatibleVariableDeclarations(int i1) {
        int i = 0;
        if (i1 == 0) {
            int newVariable1 = 0;
            i = newVariable1;
        } else if (i1 == 1) {
            int newVariable2 = 10;
            i = newVariable2;
        } else if (2 == i1) {
            char newVariable3 = 'a';
            i = newVariable3;
        }
    }

    public void doNotReplaceIfWithConflictedVariableDeclarations(int i1) {
        int i = 0;
        if (i1 == 0) {
            int integer1 = 0;
            i = integer1;
        } else if (i1 == 1) {
            int integer1 = 10;
            i = integer1;
        }
    }

    public void doNotReplaceIfWithConflictedVariableTypes(int i1) {
        int i = 0;
        if (i1 == 0) {
            int integer1 = 0;
            i = integer1;
        } else if (i1 == 2) {
            char integer1 = 'a';
            i = integer1;
        }
    }

    public void doNotReplaceOneConditionIf(int i1) {
        int i = 0;
        if (i1 == 0) {
            i = 0;
        } else {
            i = 10;
        }
    }

    public void doNotReplaceOneCaseIf(int i1) {
        int i = 0;
        if (i1 == 0) {
            i = 10;
        }
    }

    public void doNotReplaceWithPrimitiveWrapper(Integer i1) {
        int i = 0;
        if (i1 == 0) {
            i = 0;
        } else if (i1 == 10) {
            i = 10;
        } else if (i1 == 20) {
            i = 20;
        } else {
            i = 30;
        }
    }
}
