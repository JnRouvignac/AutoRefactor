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

public class ShortPrimitiveRatherThanWrapperSample {

    public Short doNotRefactorFields = Short.MIN_VALUE;

    public short shortField;

    public Short wrapperField;

    public Object objectField;

    public void replaceWrapper(short s) {
        // Keep this comment
        Short alwaysInitializedVar = Short.MIN_VALUE;
        if (alwaysInitializedVar > s) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(short s) {
        // Keep this comment
        java.lang.Short alwaysInitializedVar = Short.MIN_VALUE;
        if (alwaysInitializedVar < s) {
            System.out.println("True!");
        }
    }

    public short replacePreDecrementWrapper(short s) {
        // Keep this comment
        Short preDecrementVar = --s;
        if (preDecrementVar <= 0) {
            return -1;
        }
        return 1;
    }

    public short replacePreIncrementWrapper(short s) {
        // Keep this comment
        Short preDecrementVar = ++s;
        return preDecrementVar++;
    }

    public short replacePostDecrementWrapper(short s) {
        // Keep this comment
        Short postDecrementVar = s--;
        return postDecrementVar++;
    }

    public short replacePostIncrementWrapper(short s) {
        // Keep this comment
        Short postIncrementVar = s++;
        return postIncrementVar++;
    }

    public short replaceWrapperFromValueOf(short s1) {
        // Keep this comment
        Short varFromValueOf = Short.valueOf(s1);
        return varFromValueOf++;
    }

    public short replaceParentherizedWrapper(short s1, short s2) {
        // Keep this comment
        Short parentherizedVar = ((short)(s1 + s2));
        return parentherizedVar++;
    }

    public short replaceCastWrapper(Short s) {
        // Keep this comment
        Short castVar = (short) s;
        return castVar++;
    }

    public short replaceWrapperInPreIncrement() {
        // Keep this comment
        Short shortInPreIncrement = Short.MIN_VALUE;
        return ++shortInPreIncrement;
    }

    public short replaceWrapperInPreDecrement() {
        // Keep this comment
        Short shortInPreDecrement = Short.MIN_VALUE;
        return --shortInPreDecrement;
    }

    public short replaceWrapperInPostDecrement() {
        // Keep this comment
        Short shortInPostDecrement = Short.MIN_VALUE;
        return shortInPostDecrement--;
    }

    public short replaceWrapperInPostIncrement() {
        // Keep this comment
        Short shortInPostIncrement = Short.MIN_VALUE;
        return shortInPostIncrement++;
    }

    public void replaceWrapperInSwitch() {
        // Keep this comment
        Short shortInSwitch = Short.MIN_VALUE;
        switch (shortInSwitch) {
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
        Short shortInArrayAccess = Short.MIN_VALUE;
        return strings[shortInArrayAccess];
    }

    public short replaceReturnedWrapper() {
        // Keep this comment
        Short returnedShort = Short.MIN_VALUE;
        return returnedShort;
    }

    public Object doNotBreakAutoboxing() {
        Short returnedObject = Short.MIN_VALUE;
        return returnedObject;
    }

    public short replaceMultiReturnedWrapper(short s) {
        // Keep this comment
        Short returnedShort = Short.MIN_VALUE;
        if (s > 0) {
            System.out.println("Positive");
            return returnedShort;
        } else {
            System.out.println("Negative");
            return returnedShort;
        }
    }

    public Short replaceReturnedAutoBoxedWrapper(short s) {
        // Keep this comment
        Short returnedShort = Short.MIN_VALUE;
        if (s > 0) {
            System.out.println("Positive");
            return returnedShort;
        } else {
            System.out.println("Negative");
            return returnedShort;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        Short reassignedShort = Short.MIN_VALUE;
        reassignedShort = 123;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        Short multiReassignedShort = Short.MIN_VALUE;
        multiReassignedShort = 123;
        multiReassignedShort = 456;
    }

    public void doNotReplaceNullWrapper() {
        Short reassignedShort = Short.MIN_VALUE;
        reassignedShort = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Short, Observable> obsByShort) {
        Short reassignedShort = Short.MIN_VALUE;
        obsByShort.get(reassignedShort).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        Short assignedShort = Short.MIN_VALUE;
        Short anotherShort = assignedShort;
    }

    public void replaceWrapperAssignedOnShortField() {
        // Keep this comment
        Short assignedShort = Short.MIN_VALUE;
        shortField = assignedShort;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        Short assignedShort = Short.MIN_VALUE;
        wrapperField = assignedShort;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Short assignedShort = Short.MIN_VALUE;
        objectField = assignedShort;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Short assignedShort = Short.MIN_VALUE;
        Short anotherShort = assignedShort;
        Short yetAnotherShort = assignedShort;
    }

    public void replaceBitAssignedWrapper(Integer anInteger, Integer anotherInteger,
            Integer yetAnotherInteger) {
        // Keep this comment
        Short assignedShort = Short.MIN_VALUE;
        anInteger |= assignedShort;
        anotherInteger += assignedShort;
        yetAnotherInteger ^= assignedShort;
    }

    public Short doNotReplaceMultiAutoBoxedWrapper() {
        Short assignedShort = Short.MIN_VALUE;
        Short anotherShort = assignedShort;
        return assignedShort;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Short returnedObject = Short.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Short doNotReplaceAssignedAndReturnedWrapper(Short s) {
        Short returnedObject = Short.MIN_VALUE;
        returnedObject = s;
        return returnedObject;
    }
}
