/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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

public class IfRatherThanWhileAndFallsThroughSample {
    public void replaceWhileByIf(boolean isValid) {
        // Keep this comment
        while (isValid) {
            System.out.println("foo");
            return;
        }
    }

    public void replaceWhileThrowingExceptions(boolean isEnabled) {
        // Keep this comment
        while (isEnabled) {
            System.out.println("foo");
            throw new NullPointerException();
        }
    }

    public void replaceWhileByIfAndRemoveBreak(boolean isVisible) {
        // Keep this comment
        while (isVisible) {
            System.out.println("foo");
            break;
        }
    }

    public void replaceWhileByIfAndReplaceBreaksByBlocks(boolean isVisible, int i) {
        // Keep this comment
        while (isVisible) {
            if (i > 0)
                break;
            else
                break;
        }
    }

    public void doNotReplaceWhileEndedByContinue(boolean b) {
        while (b) {
            System.out.println("foo");
            continue;
        }
    }

    public void doNotReplaceWhileUsingContinue(boolean b1, boolean b2) {
        while (b1) {
            if (b2) {
                System.out.println("bar");
                continue;
            }
            System.out.println("foo");
            return;
        }
    }

    public void replaceWhileWithComplexCode(boolean b1, boolean b2) {
        // Keep this comment
        while (b1) {
            System.out.println("foo");
            if (b2) {
                System.out.println("bar");
                return;
            } else {
                throw new NullPointerException();
            }
        }
    }

    public void doNotReplaceWhileThatMayHaveSeveralIterations(int i) {
        while (i-- > 0) {
            System.out.println("foo");
            if (i == 1) {
                System.out.println("bar");
                return;
            } else if (i == 2) {
                throw new NullPointerException();
            }
        }
    }

    public void replaceWhileButOnlyRemoveBreakForTheWhileLoop(boolean b, int magicValue) {
        // Keep this comment
        while (b) {
            for (int i = 0; i < 10; i++) {
                if (i == magicValue) {
                    System.out.println("Magic value! Goodbye!");
                    break;
                } else {
                    System.out.println("Current value: " + i);
                }
            }
            break;
        }
    }

    public void doNotReplaceWhileThatHasLabeledBreak(boolean b) {
        doNotTrashThisSpecialBreak:while (b) {
            System.out.println("foo");
            break doNotTrashThisSpecialBreak;
        }
    }

    public void doNotRemoveBreakThatShortcutsCode(boolean b1, boolean b2) {
        while (b1) {
            if (b2) {
                System.out.println("foo");
                break;
            }
            System.out.println("bar");
            break;
        }
    }
}
