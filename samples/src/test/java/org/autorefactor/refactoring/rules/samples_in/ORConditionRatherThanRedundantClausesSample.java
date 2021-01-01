/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial implementation
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

public class ORConditionRatherThanRedundantClausesSample {
    private static int staticField = 0;

    public void removeDuplicateConditionsWithEagerOperator(boolean redundantCondition, boolean condition) {
        // Keep this comment
        boolean newBoolean1 = redundantCondition | !redundantCondition & condition;
        boolean newBoolean2 = !redundantCondition | redundantCondition & !condition;
        boolean newBoolean3 = redundantCondition & condition | !redundantCondition;
        boolean newBoolean4 = !redundantCondition & !condition | redundantCondition;
        boolean newBoolean5 = redundantCondition | condition & !redundantCondition;
        boolean newBoolean6 = !redundantCondition | !condition & redundantCondition;
        boolean newBoolean7 = condition & redundantCondition | !redundantCondition;
        boolean newBoolean8 = !condition & !redundantCondition | redundantCondition;
    }

    public void removeDuplicateConditionsWithPrimitiveTypes(boolean redundantCondition, boolean condition) {
        // Keep this comment
        boolean newBoolean1 = redundantCondition || !redundantCondition && condition;
        boolean newBoolean2 = redundantCondition || !redundantCondition && !condition;
        boolean newBoolean3 = redundantCondition && condition || !redundantCondition;
        boolean newBoolean4 = !redundantCondition && !condition || redundantCondition;
        boolean newBoolean5 = redundantCondition || condition && !redundantCondition;
        boolean newBoolean6 = !redundantCondition || !condition && redundantCondition;
        boolean newBoolean7 = condition && redundantCondition || !redundantCondition;
        boolean newBoolean8 = !condition && !redundantCondition || redundantCondition;
    }

    public void removeDuplicateConditionsInLargerExpressions(boolean redundantCondition, boolean condition, List<String> lastList) {
        // Keep this comment
        boolean newBoolean1 = redundantCondition || !redundantCondition && condition || lastList.remove("lorem");
        boolean newBoolean2 = redundantCondition || !redundantCondition && !condition || lastList.remove("lorem");
        boolean newBoolean3 = redundantCondition && condition || !redundantCondition || lastList.remove("lorem");
        boolean newBoolean4 = !redundantCondition && !condition || redundantCondition || lastList.remove("lorem");
    }

    public void removeDuplicateConditionsWithOtherExpressionFirst(boolean redundantCondition, boolean condition, List<String> firstList) {
        // Keep this comment
        boolean newBoolean1 = firstList.remove("lorem") || redundantCondition || !redundantCondition && condition;
        boolean newBoolean2 = firstList.remove("lorem") || redundantCondition || !redundantCondition && !condition;
        boolean newBoolean3 = firstList.remove("lorem") || (redundantCondition && condition) || !redundantCondition;
        boolean newBoolean4 = firstList.remove("lorem") || (!redundantCondition && !condition) || redundantCondition;
        boolean newBoolean5 = firstList.remove("lorem") || (!redundantCondition && condition) || redundantCondition;
        boolean newBoolean6 = firstList.remove("lorem") || (condition && !redundantCondition) || redundantCondition;
        boolean newBoolean7 = firstList.remove("lorem") || (condition && !redundantCondition) || redundantCondition;
    }

    public void removeWithoutParenthesis(boolean redundantCondition, boolean condition, List<String> firstList) {
        // Keep this comment
        boolean newBoolean3 = firstList.remove("lorem") || redundantCondition && condition || !redundantCondition;
        boolean newBoolean4 = firstList.remove("lorem") || !redundantCondition && !condition || redundantCondition;
        boolean newBoolean5 = firstList.remove("lorem") || !redundantCondition && condition || redundantCondition;
        boolean newBoolean6 = firstList.remove("lorem") || condition && !redundantCondition || redundantCondition;
        boolean newBoolean7 = firstList.remove("lorem") || condition && !redundantCondition || redundantCondition;
    }

    public void removeDuplicateConditionsAmongOtherExpressions(boolean redundantCondition, boolean condition, List<String> myList) {
        // Keep this comment
        boolean newBoolean1 = redundantCondition || myList.remove("lorem") && !redundantCondition && condition;
        boolean newBoolean2 = redundantCondition || myList.remove("lorem") && !redundantCondition && !condition;
        boolean newBoolean3 = myList.remove("lorem") && redundantCondition && condition || !redundantCondition;
        boolean newBoolean4 = myList.remove("lorem") && !redundantCondition && !condition || redundantCondition;
    }

    public void removeDuplicateConditionsAmongOtherPassiveExpressions(boolean redundantCondition, boolean condition, boolean otherCondition) {
        // Keep this comment
        boolean newBoolean1 = redundantCondition || !redundantCondition && condition && otherCondition;
        boolean newBoolean2 = redundantCondition || !redundantCondition && !condition && otherCondition;
        boolean newBoolean3 = redundantCondition && condition && otherCondition || !redundantCondition;
        boolean newBoolean4 = !redundantCondition && !condition && otherCondition || redundantCondition;
    }

    public void removeDuplicateConditionsWithPermutedBooleans(boolean redundantCondition, boolean condition) {
        // Keep this comment
        boolean newBoolean1 = redundantCondition || condition && !redundantCondition;
        boolean newBoolean2 = redundantCondition || !condition && !redundantCondition;
        boolean newBoolean3 = condition && redundantCondition || !redundantCondition;
        boolean newBoolean4 = !condition && redundantCondition || !redundantCondition;
    }

    public void doNoReplaceDuplicateConditionsWithWrappers() {
        boolean redundantCondition = false;
        Boolean condition = null;
        boolean newBoolean1 = redundantCondition && condition || !redundantCondition;
        boolean newBoolean2 = !redundantCondition && condition || redundantCondition;
    }

    public void removeDuplicateConditionsWithExpressions(int i1, int i2, int i3, int i4) {
        // Keep this comment
        boolean newBoolean1 = (i1 == i2) || !(i2 == i1) && (i3 == i4);
        boolean newBoolean2 = (i1 < i2) || !(i2 > i1) && !(i3 == i4);
        boolean newBoolean3 = (i1 == i2) || (i2 != i1) && (i3 == i4);
        boolean newBoolean4 = (i1 < i2) || (i2 <= i1) && (i3 != i4);
    }

    public void doNotReplaceDuplicateConditionsWithMethods(List<String> myList) {
        boolean newBoolean1 = myList.remove("lorem") || !myList.remove("lorem")
                && myList.remove("ipsum");
        boolean newBoolean2 = myList.remove("lorem") || !myList.remove("lorem")
                && !myList.remove("ipsum");
    }

    public void doNotReplaceDuplicateConditionsWithIncrements(int i1, int i2, int i3, int i4) {
        boolean newBoolean1 = (i1 == i2) || !(i1 == i2) && (i3 == i4++);
        boolean newBoolean2 = (i1 == i2) || !(i1 == i2) && (i3 == ++i4);
        boolean newBoolean3 = (i1 == i2) || !(i1 == i2) && (i3 == i4--);
        boolean newBoolean4 = (i1 == i2) || !(i1 == i2) && (i3 == --i4);

        boolean newBoolean5 = (i1 == i2) || !(i1 == i2) && !(i3 == i4++);
        boolean newBoolean6 = (i1 == i2) || !(i1 == i2) && !(i3 == ++i4);
        boolean newBoolean7 = (i1 == i2) || !(i1 == i2) && !(i3 == i4--);
        boolean newBoolean8 = (i1 == i2) || !(i1 == i2) && !(i3 == --i4);
    }

    public void doNotReplaceDuplicateConditionsWithAssignments(int i1, int i2, boolean b1, boolean b2) {
        boolean newBoolean1 = (i1 == i2) || !(i1 == i2) && (b1 = b2);
        boolean newBoolean2 = (i1 == i2) || !(i1 == i2) && !(b1 = b2);
    }

    private class SideEffect {
        private SideEffect() {
            staticField++;
        }
    }

    public void doNotReplaceDuplicateConditionsWithInstanciations(Boolean redundantCondition) {
        boolean newBoolean1 = redundantCondition
                || !redundantCondition && new SideEffect() instanceof SideEffect;
        boolean newBoolean2 = redundantCondition
                || !redundantCondition && !(new SideEffect() instanceof SideEffect);
    }
}
