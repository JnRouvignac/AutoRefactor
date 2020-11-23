/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - Initial implementation
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

public class OperandFactorizationSample {
    private static int staticField = 0;

    public void replaceDuplicateConditionsWithPrimitiveTypes(boolean repeatedBoolean, boolean isValid, boolean isActive) {
        // Keep this comment
        boolean newBoolean1 = (repeatedBoolean && (isValid || isActive));
        boolean newBoolean2 = (repeatedBoolean && (!isValid || isActive));
        boolean newBoolean3 = (repeatedBoolean && (isValid || !isActive));
        boolean newBoolean4 = (repeatedBoolean && (!isValid || !isActive));
        boolean newBoolean5 = (!repeatedBoolean && (isValid || isActive));
        boolean newBoolean6 = (!repeatedBoolean && (!isValid || isActive));
        boolean newBoolean7 = (!repeatedBoolean && (isValid || !isActive));
        boolean newBoolean8 = (!repeatedBoolean && (!isValid || !isActive));
    }

    public void replaceDuplicateConditionsWithEagerOperator(boolean repeatedBoolean, boolean isValid, boolean isEnable) {
        // Keep this comment
        boolean newBoolean1 = (repeatedBoolean & (isValid | isEnable));
        boolean newBoolean2 = (repeatedBoolean & (!isValid | isEnable));
        boolean newBoolean3 = (repeatedBoolean & (isValid | !isEnable));
        boolean newBoolean4 = (repeatedBoolean & (!isValid | !isEnable));
        boolean newBoolean5 = (!repeatedBoolean & (isValid | isEnable));
        boolean newBoolean6 = (!repeatedBoolean & (!isValid | isEnable));
        boolean newBoolean7 = (!repeatedBoolean & (isValid | !isEnable));
        boolean newBoolean8 = (!repeatedBoolean & (!isValid | !isEnable));
    }

    public void replaceDuplicateConditionsWithPermutedBooleans(boolean repeatedBoolean, boolean isValid, boolean isActive) {
        // Keep this comment
        boolean newBoolean1 = (repeatedBoolean && (isValid || isActive));
        boolean newBoolean2 = (repeatedBoolean && (!isValid || isActive));
        boolean newBoolean3 = (repeatedBoolean && (isValid || !isActive));
        boolean newBoolean4 = (repeatedBoolean && (!isValid || !isActive));
        boolean newBoolean5 = (!repeatedBoolean && (isValid || isActive));
        boolean newBoolean6 = (!repeatedBoolean && (!isValid || isActive));
        boolean newBoolean7 = (!repeatedBoolean && (isValid || !isActive));
        boolean newBoolean8 = (!repeatedBoolean && (!isValid || !isActive));

        newBoolean1 = (repeatedBoolean && (isValid || isActive));
        newBoolean2 = (repeatedBoolean && (!isValid || isActive));
        newBoolean3 = (repeatedBoolean && (isValid || !isActive));
        newBoolean4 = (repeatedBoolean && (!isValid || !isActive));
        newBoolean5 = (!repeatedBoolean && (isValid || isActive));
        newBoolean6 = (!repeatedBoolean && (!isValid || isActive));
        newBoolean7 = (!repeatedBoolean && (isValid || !isActive));
        newBoolean8 = (!repeatedBoolean && (!isValid || !isActive));
    }

    public void replaceDuplicateConditionsWithExpressions(int i1, int i2, int i3, int i4, int i5, int i6) {
        // Keep this comment
        boolean newBoolean1 = ((i1 == i2 * 2) && (!(i3 == i4) || (i5 == i6)));
        boolean newBoolean2 = ((i1 + 1 + 0 == i2) && ((i3 == i4) || !(i5 == i6)));
        boolean newBoolean3 = ((i1 < i2) && ((i3 == i4) || !(i5 == i6)));
    }

    public int replaceBitwiseOperation(int b1, int b2, int b3) {
        return (b1 & (b2 | b3));
    }

    public boolean doNoRefactorFailingCode(boolean b1, boolean[] b2, boolean b3) {
        return b2[-1] && b1 || b3 && b1;
    }

    public boolean doNoReplaceDuplicateConditionsWithOtherCondition(boolean b1, boolean b2, boolean b3, boolean b4) {
        return b1 && b2 || b1 && b3 && b4;
    }

    public void doNoReplaceDuplicateConditionsWithOtherOperandBefore(boolean b1, boolean b2, boolean b3, boolean unrelevantCondition) {
        boolean newBoolean1 = unrelevantCondition || (b1 && b2) || (!b1 && b3);
        boolean newBoolean2 = unrelevantCondition || (b1 && !b2) || (b3 && !b1);
        boolean newBoolean3 = unrelevantCondition || (b1 && b2) || (!b3 && !b1);
        boolean newBoolean4 = unrelevantCondition || (b1 && !b2) || (!b3 && !b1);
        boolean newBoolean5 = unrelevantCondition || (!b1 && b2) || (b3 && b1);
        boolean newBoolean6 = unrelevantCondition || (!b1 && !b2) || (b3 && b1);
        boolean newBoolean7 = unrelevantCondition || (!b1 && b2) || (!b3 && b1);
        boolean newBoolean8 = unrelevantCondition || (!b1 && !b2) || (!b3 && b1);
    }

    public void doNoReplaceDuplicateConditionsWithOtherOperandAfter(boolean b1, boolean b2, boolean b3, boolean unrelevantCondition) {
        boolean newBoolean1 = (b1 && b2) || (!b1 && b3) || unrelevantCondition;
        boolean newBoolean2 = (b1 && !b2) || (b3 && !b1) || unrelevantCondition;
        boolean newBoolean3 = (b1 && b2) || (!b3 && !b1) || unrelevantCondition;
        boolean newBoolean4 = (b1 && !b2) || (!b3 && !b1) || unrelevantCondition;
        boolean newBoolean5 = (!b1 && b2) || (b3 && b1) || unrelevantCondition;
        boolean newBoolean6 = (!b1 && !b2) || (b3 && b1) || unrelevantCondition;
        boolean newBoolean7 = (!b1 && b2) || (!b3 && b1) || unrelevantCondition;
        boolean newBoolean8 = (!b1 && !b2) || (!b3 && b1) || unrelevantCondition;
    }

    public boolean doNoReplaceDuplicateConditionsWithWrappers(Boolean b1, Boolean b2, Boolean b3) {
        return b1 && b2 || b1 && b3;
    }

    public void doNotReplaceDuplicateConditionsWithMethods(List<String> myList) {
        boolean newBoolean1 = myList.remove("lorem") && !myList.remove("foo") || myList.remove("lorem")
                && myList.remove("ipsum");
        boolean newBoolean2 = myList.remove("lorem") && myList.remove("bar") || myList.remove("lorem")
                && !myList.remove("ipsum");
    }

    public void doNotReplaceDuplicateConditionsWithIncrements(int i1, int i2, int i3, int i4, int i5, int i6) {
        boolean newBoolean1 = (i1 == i2) && !(i3 == i4++) || (i1 == i2) && (i5 == i6++);
        boolean newBoolean2 = (i1 == i2) && !(i3 == ++i4) || (i1 == i2) && (i5 == ++i6);
        boolean newBoolean3 = (i1 == i2) && !(i3 == i4--) || (i1 == i2) && (i5 == i6--);
        boolean newBoolean4 = (i1 == i2) && !(i3 == --i4) || (i1 == i2) && (i5 == --i6);

        boolean newBoolean5 = (i1 == i2) && (i3 == i4++) || (i1 == i2) && !(i5 == i6++);
        boolean newBoolean6 = (i1 == i2) && (i3 == ++i4) || (i1 == i2) && !(i5 == ++i6);
        boolean newBoolean7 = (i1 == i2) && (i3 == i4--) || (i1 == i2) && !(i5 == i6--);
        boolean newBoolean8 = (i1 == i2) && (i3 == --i4) || (i1 == i2) && !(i5 == --i6);
    }

    public void doNotReplaceDuplicateConditionsWithAssignments(int i1, int i2, boolean b1, boolean b2, boolean b3) {
        boolean newBoolean1 = (i1 == i2) && !(b1 = b2) || (i1 == i2) && (b1 = b3);
        boolean newBoolean2 = (i1 == i2) && (b1 = b2) || (i1 == i2) && !(b1 = b3);
    }

    private class SideEffect {
        private SideEffect() {
            staticField++;
        }
    }

    public void doNotReplaceDuplicateConditionsWithInstanciations(Boolean b1) {
        boolean newBoolean1 = b1 && !(new SideEffect() instanceof SideEffect)
                || b1 && new SideEffect() instanceof Object;
        boolean newBoolean2 = b1 && new SideEffect() instanceof SideEffect
                || b1 && !(new SideEffect() instanceof Object);
    }
}
