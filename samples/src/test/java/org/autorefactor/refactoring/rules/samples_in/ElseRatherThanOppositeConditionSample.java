/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - Initial implementation
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

import java.io.IOException;
import java.util.List;

public class ElseRatherThanOppositeConditionSample {
    public int removeOppositeCondition(boolean b1, boolean b2) {
        int i = -1;
        // Keep this comment
        if (b1 && b2) {
            i = 0;
        } else if (!b2 || !b1) {
            i = 1;
        }

        return i;
    }

    public int removeOppositeConditionWithElse(int i1, int i2) {
        int i = -1;
        // Keep this comment
        if (i1 < i2) {
            i = 0;
        } else if (i2 <= i1) {
            i = 1;
        } else {
            i = 2;
        }

        return i;
    }

    public int removeOppositeConditionAmongOthers(int i1, int i2) {
        int i = -1;
        // Keep this comment
        if (i1 == 0) {
            i = -1;
        } else if (i1 < i2 + 1) {
            i = 0;
        } else if (1 + i2 <= i1) {
            i = 1;
        }

        return i;
    }

    public int doNotRemoveDifferentCondition(boolean b1, boolean b2) {
        int i = -1;
        if (b1 && b2) {
            i = 0;
        } else if (b2 || b1) {
            i = 1;
        }

        return i;
    }

    public int doNotRemoveActiveCondition(List<String> myList) {
        int i = -1;
        if (myList.remove("I will be removed")) {
            i = 0;
        } else if (myList.remove("I will be removed")) {
            i = 1;
        }

        return i;
    }

    public int refactorCaughtCode(boolean b1, boolean b2) {
        int i = -1;
        try {
            // Keep this comment
            if (b1 && b2) {
                i = 0;
            } else if (!b2 || !b1) {
                throw new IOException();
            }
        } catch (IOException e) {
            System.out.println("I should be reachable");
        }

        return i;
    }

    public int doNotRemoveCaughtCode(boolean b1, boolean b2) {
        int i = -1;
        try {
            if (b1 && b2) {
                i = 0;
            } else if (!b2 || !b1) {
                i = 1;
            } else {
                throw new IOException();
            }
        } catch (IOException e) {
            System.out.println("I should be reachable");
        }

        return i;
    }

    public int removeUncaughtCode(boolean b1, boolean b2) {
        int i = -1;
        try {
            // Keep this comment
            if (b1 && b2) {
                i = 0;
            } else if (!b2 || !b1) {
                i = 1;
            } else {
                throw new NullPointerException();
            }
        } finally {
            System.out.println("I should be reachable");
        }

        return i;
    }

    public int doNotRefactorFallThroughBlocks(boolean b1, boolean b2) {
        if (b1 && b2) {
            return 0;
        } else if (!b2 || !b1) {
            return 1;
        }

        return 2;
    }
}
