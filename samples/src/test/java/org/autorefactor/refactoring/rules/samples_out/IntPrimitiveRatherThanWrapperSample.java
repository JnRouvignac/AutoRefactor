/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - initial API and implementation
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

import java.util.Map;
import java.util.Observable;

public class IntPrimitiveRatherThanWrapperSample {

    public Integer doNotRefactorFields = Integer.MIN_VALUE;

    public int intField;

    public Integer wrapperField;

    public Object objectField;

    public void replaceWrapper(int i) {
        // Keep this comment
        int alwaysInitializedVar = Integer.MIN_VALUE;
        if (alwaysInitializedVar > i) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(int i) {
        // Keep this comment
        int alwaysInitializedVar = Integer.MIN_VALUE;
        if (alwaysInitializedVar < i) {
            System.out.println("True!");
        }
    }

    public boolean replacePlusWrapper(int i1, int i2) {
        // Keep this comment
        int plusVar = i1 + i2;
        return plusVar > 0;
    }

    public int replaceLessWrapper(int i1, int i2) {
        // Keep this comment
        int lessVar = i1 - i2;
        return -lessVar;
    }

    public int replaceTimesWrapper(int i1, int i2) {
        // Keep this comment
        int timesVar = i1 * i2;
        return timesVar + 100;
    }

    public int replaceDivideWrapper(int i1, int i2) {
        // Keep this comment
        int divideVar = i1 / i2;
        if (divideVar <= 0) {
            return -1;
        }
        return 1;
    }

    public int replaceAndMaskWrapper(int i1, int i2) {
        // Keep this comment
        int divideVar = i1 & i2;
        return divideVar++;
    }

    public int replaceOrMaskWrapper(int i1, int i2) {
        // Keep this comment
        int divideVar = i1 | i2;
        return divideVar++;
    }

    public int replaceShiftMaskWrapper(int i1, int i2) {
        // Keep this comment
        int divideVar = i1 ^ i2;
        return divideVar++;
    }

    public int replaceMinusWrapper(int i) {
        // Keep this comment
        int minusVar = -i;
        return minusVar++;
    }

    public int replacePreDecrementWrapper(int i) {
        // Keep this comment
        int preDecrementVar = --i;
        return preDecrementVar++;
    }

    public int replacePreIncrementWrapper(int i) {
        // Keep this comment
        int preDecrementVar = ++i;
        return preDecrementVar++;
    }

    public int replacePostDecrementWrapper(int i) {
        // Keep this comment
        int postDecrementVar = i--;
        return postDecrementVar++;
    }

    public int replacePostIncrementWrapper(int i) {
        // Keep this comment
        int postIncrementVar = i++;
        return postIncrementVar++;
    }

    public int replaceWrapperFromValueOf(int i1) {
        // Keep this comment
        int varFromValueOf = Integer.valueOf(i1);
        return varFromValueOf++;
    }

    public int replaceParentherizedWrapper(int i1, int i2) {
        // Keep this comment
        int parentherizedVar = (i1 + i2);
        return parentherizedVar++;
    }

    public int replaceComplexExprWrapper(int i1, int i2, int i3, int i4) {
        // Keep this comment
        int complexVar = i1 + i2 / (i3 - i4);
        return complexVar++;
    }

    public int replaceCastWrapper(Integer i) {
        // Keep this comment
        int castVar = (int) i;
        return castVar++;
    }

    public int replaceWrapperInPreIncrement() {
        // Keep this comment
        int alwaysInitializedVar = Integer.MIN_VALUE;
        return ++alwaysInitializedVar;
    }

    public int replaceWrapperInPreDecrement() {
        // Keep this comment
        int alwaysInitializedVar = Integer.MIN_VALUE;
        return --alwaysInitializedVar;
    }

    public int replaceWrapperInPostDecrement() {
        // Keep this comment
        int alwaysInitializedVar = Integer.MIN_VALUE;
        return alwaysInitializedVar--;
    }

    public int replaceWrapperInPostIncrement() {
        // Keep this comment
        int alwaysInitializedVar = Integer.MIN_VALUE;
        return alwaysInitializedVar++;
    }

    public void replaceWrapperInSwitch() {
        // Keep this comment
        int intInSwitch = Integer.MIN_VALUE;
        switch (intInSwitch) {
        case 1:
            System.out.println("One");
            break;

        case 2:
            System.out.println("Two");
            break;

        default:
            break;
        }
    }

    public String replaceWrapperInArrayAccess(String[] strings) {
        // Keep this comment
        int intInArrayAccess = Integer.MIN_VALUE;
        return strings[intInArrayAccess];
    }

    public int replaceReturnedWrapper() {
        // Keep this comment
        int returnedInteger = Integer.MIN_VALUE;
        return returnedInteger;
    }

    public Object doNotBreakAutoboxing() {
        Integer returnedObject = Integer.MIN_VALUE;
        return returnedObject;
    }

    public int replaceMultiReturnedWrapper(int i) {
        // Keep this comment
        int returnedInteger = Integer.MIN_VALUE;
        if (i > 0) {
            System.out.println("Positive");
            return returnedInteger;
        } else {
            System.out.println("Negative");
            return returnedInteger;
        }
    }

    public Integer replaceReturnedAutoBoxedWrapper(int i) {
        // Keep this comment
        int returnedInteger = Integer.MIN_VALUE;
        if (i > 0) {
            System.out.println("Positive");
            return returnedInteger;
        } else {
            System.out.println("Negative");
            return returnedInteger;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        int reassignedInteger = Integer.MIN_VALUE;
        reassignedInteger = 123;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        int multiReassignedInteger = Integer.MIN_VALUE;
        multiReassignedInteger = 123;
        multiReassignedInteger = 456;
    }

    public void doNotReplaceNullWrapper() {
        Integer reassignedInteger = Integer.MIN_VALUE;
        reassignedInteger = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Integer, Observable> obsByInteger) {
        Integer reassignedInteger = Integer.MIN_VALUE;
        obsByInteger.get(reassignedInteger).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        int assignedInteger = Integer.MIN_VALUE;
        Integer anotherInteger = assignedInteger;
    }

    public void replaceWrapperAssignedOnIntegerField() {
        // Keep this comment
        int assignedInteger = Integer.MIN_VALUE;
        intField = assignedInteger;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        int assignedInteger = Integer.MIN_VALUE;
        wrapperField = assignedInteger;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Integer assignedInteger = Integer.MIN_VALUE;
        objectField = assignedInteger;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Integer assignedInteger = Integer.MIN_VALUE;
        Integer anotherInteger = assignedInteger;
        Integer yetAnotherInteger = assignedInteger;
    }

    public void replaceBitAssignedWrapper(Integer aInteger, Integer anotherInteger,
            Integer yetAnotherInteger) {
        // Keep this comment
        int assignedInteger = Integer.MIN_VALUE;
        aInteger &= assignedInteger;
        anotherInteger += assignedInteger;
        yetAnotherInteger ^= assignedInteger;
    }

    public Integer doNotReplaceMultiAutoBoxedWrapper() {
        Integer assignedInteger = Integer.MIN_VALUE;
        Integer anotherInteger = assignedInteger;
        return assignedInteger;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Integer returnedObject = Integer.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Integer doNotReplaceAssignedAndReturnedWrapper(Integer i) {
        Integer returnedObject = Integer.MIN_VALUE;
        returnedObject = i;
        return returnedObject;
    }
}
