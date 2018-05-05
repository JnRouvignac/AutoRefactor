/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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

public class OppositeConditionRatherThanDuplicateConditionSample {
    public int mergeDuplicateCondition(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (duplicateCondition && anotherCondition) {
            // Keep this comment also
            return 0;
        } else if (duplicateCondition) {
            return 10;
        } else {
            return 20;
        }
    }

    public int mergeDuplicateCondition2(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (duplicateCondition && anotherCondition) {
            // Keep this comment also
            return 0;
        } else if (duplicateCondition) {
            return 10;
        } else if (anotherCondition) {
            return 20;
        } else {
            return 30;
        }
    }

    public void mergeDuplicateSecondCondition(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (anotherCondition && duplicateCondition) {
            // Keep this comment also
            System.out.println("0");
        } else if (duplicateCondition) {
            System.out.println("10");
        } else {
            System.out.println("20");
        }
    }

    public int mergeInvertedCondition(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (duplicateCondition && anotherCondition) {
            // Keep this comment also
            return 0;
        } else if (!duplicateCondition) {
            return 1;
        } else {
            return 2;
        }
    }

    public int mergeAnotherInvertedCondition(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (!duplicateCondition && anotherCondition) {
            // Keep this comment also
            return 0;
        } else if (duplicateCondition) {
            return 1;
        } else {
            return 2;
        }
    }

    public int mergeSeveralConditions(boolean duplicateCondition, boolean anotherCondition,
            boolean yetAnotherCondition) {
        // Keep this comment
        if (duplicateCondition && anotherCondition) {
            // Keep this comment also
            return 100;
        } else if (yetAnotherCondition && duplicateCondition) {
            return 200;
        } else if (duplicateCondition) {
            return 300;
        } else {
            return 400;
        }
    }

    public int doNotMergeExclusiveCondition(boolean duplicateCondition, boolean anotherCondition) {
        if (duplicateCondition ^ anotherCondition) {
            return 0;
        } else if (duplicateCondition) {
            return 10;
        } else {
            return 20;
        }
    }

    public int mergeDuplicateConditionWithoutBrackets(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (duplicateCondition && anotherCondition)
            return 0;
        else if (duplicateCondition)
            return 10;
        else
            return 20;
    }

    public void mergeDuplicateEagerCondition(boolean duplicateCondition, boolean anotherCondition) {
        // Keep this comment
        if (duplicateCondition & anotherCondition) {
            // Keep this comment also
            System.out.println("foo");
        } else if (duplicateCondition) {
            System.out.println("bar");
        } else {
            System.out.println("foo bar");
        }
    }

    public char mergeEmbeddedDuplicateCondition(boolean b, boolean duplicateCondition, boolean anotherCondition) {
        if (b) {
            // Keep this comment
            if (duplicateCondition && anotherCondition) {
                // Keep this comment also
                return 'g';
            } else if (duplicateCondition) {
                return 'm';
            } else {
                return 'd';
            }
        }
        return 'z';
    }

    public String mergeDuplicateExpressionCondition(int i, boolean anotherCondition) {
        // Keep this comment
        if (i == 0 && anotherCondition) {
            // Keep this comment also
            return "foo bar";
        } else if (i == 0) {
            return "bar";
        } else {
            return "foo";
        }
    }

    public double mergeDuplicateComplexCondition(int i, boolean anotherCondition) {
        // Keep this comment
        if ((i < 10 || i > 20) && anotherCondition) {
            // Keep this comment also
            return 0.0;
        } else if (i < 10 || i > 20) {
            return 10.0;
        } else {
            return 20.0;
        }
    }

    public int doNotMergeAssignment(int i, boolean assignedCondition) {
        if (i == 0 && (assignedCondition = i == 0)) {
            return 0;
        } else if (assignedCondition = i == 0) {
            return 10;
        } else {
            return 20;
        }
    }

    public int doNotMergeIncrement(int i, int j) {
        if (i == 0 && (++j == 0)) {
            return 0;
        } else if (++j == 0) {
            return 10;
        } else {
            return 20;
        }
    }

    public int doNotMergeDecrement(int i, int j) {
        if (i == 0 && (j-- == 0)) {
            return 0;
        } else if (j-- == 0) {
            return 10;
        } else {
            return 20;
        }
    }

    public int doNotMergeMethod(int i, List<String> changedList) {
        if (i == 0 && changedList.remove("foo")) {
            return 0;
        } else if (changedList.remove("foo")) {
            return 10;
        } else {
            return 20;
        }
    }
}
