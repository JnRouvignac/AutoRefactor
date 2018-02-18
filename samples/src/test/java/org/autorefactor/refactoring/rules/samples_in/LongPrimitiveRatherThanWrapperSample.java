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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.Map;
import java.util.Observable;

public class LongPrimitiveRatherThanWrapperSample {

    public Long doNotRefactorFields = Long.MIN_VALUE;

    public long longField;

    public Long wrapperField;

    public Object objectField;

    public void replaceWrapper(long l) {
        // Keep this comment
        Long alwaysInitializedVar = Long.MIN_VALUE;
        if (alwaysInitializedVar > l) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(long l) {
        // Keep this comment
        java.lang.Long alwaysInitializedVar = Long.MIN_VALUE;
        if (alwaysInitializedVar < l) {
            System.out.println("True!");
        }
    }

    public boolean replacePlusWrapper(long l1, long l2) {
        // Keep this comment
        Long plusVar = l1 + l2;
        return plusVar > 0;
    }

    public long replaceLessWrapper(long l1, long l2) {
        // Keep this comment
        Long lessVar = l1 - l2;
        return -lessVar;
    }

    public long replaceTimesWrapper(long l1, long l2) {
        // Keep this comment
        Long timesVar = l1 * l2;
        return timesVar + 100;
    }

    public long replaceDivideWrapper(long l1, long l2) {
        // Keep this comment
        Long divideVar = l1 / l2;
        if (divideVar <= 0) {
            return -1;
        }
        return 1;
    }

    public long replaceAndMaskWrapper(long l1, long l2) {
        // Keep this comment
        Long divideVar = l1 & l2;
        return divideVar++;
    }

    public long replaceOrMaskWrapper(long l1, long l2) {
        // Keep this comment
        Long divideVar = l1 | l2;
        return divideVar++;
    }

    public long replaceShiftMaskWrapper(long l1, long l2) {
        // Keep this comment
        Long divideVar = l1 ^ l2;
        return divideVar++;
    }

    public long replaceMinusWrapper(long l) {
        // Keep this comment
        Long minusVar = -l;
        return minusVar++;
    }

    public long replacePreDecrementWrapper(long l) {
        // Keep this comment
        Long preDecrementVar = --l;
        return preDecrementVar++;
    }

    public long replacePreIncrementWrapper(long l) {
        // Keep this comment
        Long preDecrementVar = ++l;
        return preDecrementVar++;
    }

    public long replacePostDecrementWrapper(long l) {
        // Keep this comment
        Long postDecrementVar = l--;
        return postDecrementVar++;
    }

    public long replacePostIncrementWrapper(long l) {
        // Keep this comment
        Long postIncrementVar = l++;
        return postIncrementVar++;
    }

    public long replaceWrapperFromValueOf(long l1) {
        // Keep this comment
        Long varFromValueOf = Long.valueOf(l1);
        return varFromValueOf++;
    }

    public long replaceParentherizedWrapper(long l1, long l2) {
        // Keep this comment
        Long parentherizedVar = (l1 + l2);
        return parentherizedVar++;
    }

    public long replaceComplexExprWrapper(long l1, long l2, long l3, long l4) {
        // Keep this comment
        Long complexVar = l1 + l2 / (l3 - l4);
        return complexVar++;
    }

    public long replaceCastWrapper(Long l) {
        // Keep this comment
        Long castVar = (long) l;
        return castVar++;
    }

    public long replaceWrapperInPreIncrement() {
        // Keep this comment
        Long longInPreIncrement = Long.MIN_VALUE;
        return ++longInPreIncrement;
    }

    public long replaceWrapperInPreDecrement() {
        // Keep this comment
        Long longInPreDecrement = Long.MIN_VALUE;
        return --longInPreDecrement;
    }

    public long replaceWrapperInPostDecrement() {
        // Keep this comment
        Long longInPostDecrement = Long.MIN_VALUE;
        return longInPostDecrement--;
    }

    public long replaceWrapperInPostIncrement() {
        // Keep this comment
        Long longInPostIncrement = Long.MIN_VALUE;
        return longInPostIncrement++;
    }

    public long replaceReturnedWrapper() {
        // Keep this comment
        Long returnedLong = Long.MIN_VALUE;
        return returnedLong;
    }

    public Object doNotBreakAutoboxing() {
        Long returnedObject = Long.MIN_VALUE;
        return returnedObject;
    }

    public long replaceMultiReturnedWrapper(long l) {
        // Keep this comment
        Long returnedLong = Long.MIN_VALUE;
        if (l > 0) {
            System.out.println("Positive");
            return returnedLong;
        } else {
            System.out.println("Negative");
            return returnedLong;
        }
    }

    public Long replaceReturnedAutoBoxedWrapper(long l) {
        // Keep this comment
        Long returnedLong = Long.MIN_VALUE;
        if (l > 0) {
            System.out.println("Positive");
            return returnedLong;
        } else {
            System.out.println("Negative");
            return returnedLong;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        Long reassignedLong = Long.MIN_VALUE;
        reassignedLong = 123L;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        Long multiReassignedLong = Long.MIN_VALUE;
        multiReassignedLong = 123L;
        multiReassignedLong = 456L;
    }

    public void doNotReplaceNullWrapper() {
        Long reassignedLong = Long.MIN_VALUE;
        reassignedLong = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Long, Observable> obsByLong) {
        Long reassignedLong = Long.MIN_VALUE;
        obsByLong.get(reassignedLong).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        Long assignedLong = Long.MIN_VALUE;
        Long anotherLong = assignedLong;
    }

    public void replaceWrapperAssignedOnLongField() {
        // Keep this comment
        Long assignedLong = Long.MIN_VALUE;
        longField = assignedLong;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        Long assignedLong = Long.MIN_VALUE;
        wrapperField = assignedLong;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Long assignedLong = Long.MIN_VALUE;
        objectField = assignedLong;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Long assignedLong = Long.MIN_VALUE;
        Long anotherLong = assignedLong;
        Long yetAnotherLong = assignedLong;
    }

    public void replaceBitAssignedWrapper(Long aLong, Long anotherLong,
            Long yetAnotherLong) {
        // Keep this comment
        Long assignedLong = Long.MIN_VALUE;
        aLong &= assignedLong;
        anotherLong += assignedLong;
        yetAnotherLong ^= assignedLong;
    }

    public Long doNotReplaceMultiAutoBoxedWrapper() {
        Long assignedLong = Long.MIN_VALUE;
        Long anotherLong = assignedLong;
        return assignedLong;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Long returnedObject = Long.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Long doNotReplaceAssignedAndReturnedWrapper(Long l) {
        Long returnedObject = Long.MIN_VALUE;
        returnedObject = l;
        return returnedObject;
    }
}
