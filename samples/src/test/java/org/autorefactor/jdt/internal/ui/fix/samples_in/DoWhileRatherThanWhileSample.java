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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

public class DoWhileRatherThanWhileSample {
    private void replaceWhileByDoWhile(int i) {
        // Keep this comment
        while (true) {
            // Keep this comment too
            if (i > 100) {
                return;
            }
            i *= 2;
        }
    }

    private void replaceWithInitedBoolean(int i) {
        boolean isInitedToTrue= true;

        // Keep this comment
        while (isInitedToTrue) {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
        }
    }

    private void replaceWithInitedInteger(int i) {
        int j= 1_000;

        // Keep this comment
        while (j > 0) {
            // Keep this comment too
            if (i > 100) {
                return;
            }
            i *= 2;
            j--;
        }
    }

    private void replaceWithInitedBooleanAndInteger(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        // Keep this comment
        while (isInitedToTrue && j > 0) {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void replaceWithORExpression(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        // Keep this comment
        while (isInitedToTrue || j > 0) {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void replaceWithFalseOperand(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        // Keep this comment
        while (isInitedToTrue || j > 0 || false) {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void doNotReplaceWithUnnkownOperand(int i, boolean isValid) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        while (isInitedToTrue && j > 0 && isValid) {
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void doNotReplaceWithUnnkownInitialization(int i, boolean isValid) {
        int j= 1_000;
        boolean isInitedToTrue= isValid;

        while (isInitedToTrue && j > 0) {
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void doNotReplaceWithIncrement(int i) {
        int j= 1_000;
        boolean isInitedToTrue= true;

        while (isInitedToTrue && j++ > 0) {
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void replaceRecursiveInitialization(int i) {
        int j= 1_000;
        int k= -1_000;
        boolean isInitedToTrue= k < 0;

        // Keep this comment
        while (isInitedToTrue && j > 0) {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void replaceWithReassignment(int i) {
        int j= 1_000;
        int k= -1_000;
        boolean isInitedToTrue= false;
        isInitedToTrue= k < 0;

        // Keep this comment
        while (isInitedToTrue && j > 0) {
            // Keep this comment too
            if (i > 100) {
                isInitedToTrue= false;
            }
            i *= 2;
            j--;
        }
    }

    private void doNotReplaceWhileWithCondition(int i) {
        while (i <= 100) {
            i *= 2;
        }
    }
}
