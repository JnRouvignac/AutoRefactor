/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

public class ObsoleteAndConditionRatherThanEmbededIfSample {
    public void collapseIfStatements(boolean isActive, boolean isValid) {
        // Keep this comment
        if (isActive) {
            // Keep this comment too
            if (isValid) {
                // Keep this comment also
                int i = 0;
            }
        }
    }

    public void collapseLoneIfStatements(boolean isActive, boolean isValid, List<String> texts) {
        // Keep this comment
        if (isActive)
            if (isValid)
                texts.clear();
    }

    public void collapseCommentedLoneIfStatements(boolean isActive, boolean isValid, List<String> texts) {
        // Keep this comment
        if (isActive)
            if (isValid)
                texts.clear(); // Keep this comment too
    }

    public void collapseWithFourOperands(int i1, int i2) {
        // Keep this comment
        if (0 < i1 && i1 < 10) {
            // Keep this comment too
            if (0 < i2 && i2 < 10) {
                // Keep this comment also
                int i = 0;
            }
        }
    }

    public void doNotCollapseWithFiveOperands(int i1, int i2) {
        // Keep this comment
        if (0 < i1 && i1 < 10) {
            // Keep this comment too
            if (100 < i2 && i2 < 200 || i2 < 0) {
                // Keep this comment also
                int i = 0;
            }
        }
    }

    public void collapseIfStatementsAddParenthesesIfDifferentConditionalOperator(boolean isActive, boolean isValid, boolean isEditMode) {
        // Keep this comment
        if (isActive) {
            // Keep this comment too
            if (isValid || isEditMode) {
                // Keep this comment also
                int i = 0;
            }
        }
    }

    public void collapseIfWithOROperator(boolean isActive, boolean isValid, boolean isEditMode) {
        // Keep this comment
        if (isActive) {
            // Keep this comment too
            if (isValid | isEditMode) {
                // Keep this comment also
                int i = 0;
            }
        }
    }

    public void doNotCollapseOuterIfWithElseStatement(boolean isActive, boolean isValid) {
        if (isActive) {
            if (isValid) {
                int i = 0;
            }
        } else {
            int i = 0;
        }
    }

    public void doNotCollapseIfWithElseStatement2(boolean isActive, boolean isValid) {
        if (isActive) {
            if (isValid) {
                int i = 0;
            } else {
                int i = 0;
            }
        }
    }

}
