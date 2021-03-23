/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2021 Fabrice TIERCELIN - initial API and implementation
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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.List;

public class ObsoleteDuplicateAlternativeSample {
    public void collapseIfStatements(boolean isActive, boolean isValid) {
        // Keep this comment
        // Keep this comment too
        if (isActive && isValid) {
            // Keep this comment also
            int i = 0;
        } else {
            System.out.println("Duplicate code");
        }
    }

    public void collapseInnerElse(boolean isActive, boolean isValid) {
        // Keep this comment
        // Keep this comment too
        if (!isActive || isValid) {
            System.out.println("Duplicate code");
        } else {
            // Keep this comment also
            int j = 0;
        }
    }

    public void collapseLoneIfStatements(boolean isActive, boolean isValid, List<String> texts) {
        // Keep this comment
        if (isActive && isValid)
            texts.clear();
        else
            System.out.println("Duplicate code");
    }

    public void collapseCommentedLoneIfStatements(boolean isActive, boolean isValid, List<String> texts) {
        // Keep this comment
        if (isActive && isValid)
            texts.clear(); // Keep this comment too
        else
            System.out.println("Duplicate code");
    }

    public void collapseWithFourOperands(int i1, int i2) {
        // Keep this comment
        // Keep this comment too
        if ((0 < i1 && i1 < 10) && (0 < i2 && i2 < 10)) {
            // Keep this comment also
            int i = 0;
        } else {
            System.out.println("Duplicate code");
        }
    }

    public void collapseIfStatementsAddParenthesesIfDifferentConditionalOperator(boolean isActive, boolean isValid,
            boolean isEditMode) {
        // Keep this comment
        // Keep this comment too
        if (isActive && (isValid || isEditMode)) {
            // Keep this comment also
            int i = 0;
        } else {
            System.out.println("Duplicate code");
        }
    }

    public void collapseIfWithOROperator(boolean isActive, boolean isValid, boolean isEditMode) {
        // Keep this comment
        // Keep this comment too
        if (isActive && (isValid | isEditMode)) {
            // Keep this comment also
            int i = 0;
        } else {
            System.out.println("Duplicate code");
        }
    }

    public void mergeLongDuplicateCode(boolean isActive, boolean isValid, int number) {
        // Keep this comment
        // Keep this comment too
        if (isActive && isValid) {
            // Keep this comment also
            int i = 0;
        } else {
            int j = number + 123;
            System.out.println((j == 0) ? "Duplicate" : "code");
        }
    }

    public void doNotCollapseIfStatementsWithAdditionalStatement(boolean isActive, boolean isValid) {
        if (isActive) {
            if (isValid) {
                int i = 0;
            } else {
                System.out.println("Duplicate code");
            }
            System.out.println("Hi!");
        } else {
            System.out.println("Duplicate code");
        }
    }

    public void doNotCollapseWithFiveOperands(int number1, int number2) {
        if (0 < number1 && number1 < 10) {
            if (100 < number2 && number2 < 200 || number2 < 0) {
                int i = 0;
            } else {
                System.out.println("Duplicate code");
            }
        } else {
            System.out.println("Duplicate code");
        }
    }

    public void doNotMergeDifferentCode(boolean isActive, boolean isValid) {
        if (isActive) {
            if (isValid) {
                int i = 0;
            } else {
                System.out.println("One code");
            }
        } else {
            System.out.println("Another code");
        }
    }

    public void doNotMergeEmptyCode(boolean isActive, boolean isValid) {
        if (isActive) {
            if (isValid) {
                int i = 0;
            } else {
            }
        } else {
        }
    }
}
