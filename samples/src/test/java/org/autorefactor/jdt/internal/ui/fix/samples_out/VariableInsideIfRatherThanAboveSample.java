/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.List;

public class VariableInsideIfRatherThanAboveSample {
    public int moveDeclaration(boolean isValid) {
        // Keep this comment too
        if (isValid) {
            // Keep this comment
            int i = 42;
            // Keep this comment again
            return i * 10;
        }

        return 0;
    }

    public int moveDeclarationWithoutBlock(boolean isValid) {
        // Keep this comment too
        if (isValid) {
            // Keep this comment
            int i = 42;
            return i * 10;
        }

        return 0;
    }

    public int moveDeclarationToElseWithoutBlock(boolean isValid) {
        // Keep this comment too
        if (isValid)
            return 0;
        else {
            // Keep this comment
            int i = 42;
            return i * 10;
        }
    }

    public int moveComplexDeclaration(boolean isValid, int j, int k) {
        // Keep this comment too
        if (isValid) {
            // Keep this comment
            int i = j + k;
            // Keep this comment too
            return i * 10;
        }

        return 0;
    }

    public int moveDeclarationToElse(boolean isValid, int j, int k) {
        // Keep this comment too
        if (isValid) {
            return 0;
        } else {
            // Keep this comment
            int i = j + k;
            // Keep this comment too
            return i * 10;
        }
    }

    public int moveDeclarationWithoutInitializer(boolean isValid) {
        // Keep this comment too
        if (isValid) {
            // Keep this comment
            int i;
            i = 42;
            // Keep this comment too
            return i * 10;
        }

        return 0;
    }

    public int doNotMoveDeclarationWithPotentialException(boolean isValid, int[] array) {
        int i = array[-1];

        if (isValid) {
            return i * 10;
        }

        return 0;
    }

    public int doNotMoveDeclarationUsedBelow(boolean isValid) {
        int i = 42;

        if (isValid) {
            return i * 10;
        }

        return i * 100;
    }

    public int doNotMoveDeclarationUsedInThenAndElse(boolean isValid) {
        int i = 42;

        if (isValid) {
            return i * 10;
        } else {
            return i * 100;
        }
    }

    public int moveDeclaration(boolean isValid, List<String> texts) {
        int j = texts.size();
        // Keep this comment too
        if (isValid) {
            // Keep this comment
            int i = 42 + j;
            // Keep this comment too
            return i * 10;
        }

        return 0;
    }

    public int doNotMoveDeclarationUsedInCondition() {
        int i = 42;

        if (i == 42) {
            return i * 10;
        }

        return 0;
    }

    public int doNotMoveDeclarationUnused(boolean isValid) {
        int i = 42;

        if (isValid) {
            return 10;
        }

        return 0;
    }

    public int doNotMoveMultiDeclaration(boolean isValid) {
        int i = 42, j = 43;

        if (isValid) {
            return i * 10;
        }

        return 0;
    }
}
