/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

public class OneIfRatherThanDuplicateBlocksThatFallThroughSample {

    public void mergeConditionsWithReturn(int i1) {
        // Keep this comment
        if (i1 == 0) {
            System.out.println("The same code");
            return;
        }
        if (i1 == 1) {
            System.out.println("The same code");
            return;
        }
        System.out.println("Next code");
    }

    public void mergeConditionsWithThrow(int i1) throws Exception {
        // Keep this comment
        if (i1 == 0) {
            System.out.println("The same code");
            i1--;
            throw new Exception();
        }
        if (i1 == 1) {
            System.out.println("The same code");
            --i1;
            throw new Exception();
        }
        System.out.println("Next code");
    }

    public void mergeConditionsWithContinue() {
        for (int i1 = 0; i1 < 10; i1++) {
            // Keep this comment
            if (i1 == 0) {
                System.out.println("The same code");
                i1++;
                continue;
            }
            if (i1 == 1) {
                System.out.println("The same code");
                ++i1;
                continue;
            }
            System.out.println("Next code");
        }
        System.out.println("Another code");
    }

    public void mergeConditionsWithBreak() {
        for (int i1 = 0; i1 < 10; i1++) {
            // Keep this comment
            if (i1 == 0) {
                System.out.println("The same code");
                i1++;
                break;
            }
            if (i1 == 1) {
                System.out.println("The same code");
                i1 = i1 + 1;
                break;
            }
            System.out.println("Next code");
        }
        System.out.println("Another code");
    }

    public void mergeConditionsWithReturnAndThrow(int i1, int i2) throws Exception {
        // Keep this comment
        if (i1 == 0) {
            System.out.println("The same code");
            if (i2 == 0) {
                return;
            } else {
                throw new Exception("Error #" + i1++);
            }
        }
        if (i1 == 1) {
            System.out.println("The same code");
            if (i2 == 0) {
                return;
            } else {
                throw new Exception("Error #" + i1++);
            }
        }
        System.out.println("Next code");
    }

    public void doNotMergeConditionsWithConditionalReturn(int i1, int i2) {
        if (i1 == 0) {
            System.out.println("The same code");
            if (i2 == 0) {
                return;
            }
        }
        if (i1 == 1) {
            System.out.println("The same code");
            if (i2 == 0) {
                return;
            }
        }
        System.out.println("Next code");
    }

    public void mergeSeveralConditions(int i1) {
        // Keep this comment
        if (i1 == 0) {
            System.out.println("The same code");
            return;
        }
        if (i1 == 1) {
            System.out.println("The same code");
            return;
        }
        if (i1 == 2) {
            System.out.println("The same code");
            return;
        }
        if (i1 == 3) {
            System.out.println("The same code");
            return;
        }
        System.out.println("Next code");
    }

    public void doNotMergeConditionsWithoutJump(int i) {
        if (i == 0) {
            System.out.println("The same code");
        }
        if (i == 1) {
            System.out.println("The same code");
        }
        System.out.println("Next code");
    }

    public void doNotMergeDifferentBlocks(int i) {
        if (i == 0) {
            System.out.println("A code");
            return;
        }
        if (i == 1) {
            System.out.println("Another code");
            return;
        }
        System.out.println("Next code");
    }

    public void doNotMergeConditionsWithElse(int i1, int counter) {
        // Keep this comment
        if (i1 == 0) {
            System.out.println("The count is: " + counter++);
            return;
        } else {
            System.out.println("The count is: " + ++counter);
        }
        if (i1 == 1) {
            System.out.println("The count is: " + counter++);
            return;
        }
        System.out.println("Next code");
    }

    public void doNotMergeConditionsWithAnotherElse(int i) {
        // Keep this comment
        if (i == 0) {
            System.out.println("The same code");
            return;
        }
        if (i == 1) {
            System.out.println("The same code");
            return;
        } else {
            System.out.println("Another code");
        }
        System.out.println("Next code");
    }
}
