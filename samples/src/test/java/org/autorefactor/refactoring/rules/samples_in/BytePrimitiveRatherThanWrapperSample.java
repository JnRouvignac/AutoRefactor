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

public class BytePrimitiveRatherThanWrapperSample {

    public Byte doNotRefactorFields = Byte.MIN_VALUE;

    public byte byteField;

    public Byte wrapperField;

    public Object objectField;

    public void replaceWrapper(byte b) {
        // Keep this comment
        Byte alwaysInitializedVar = Byte.MIN_VALUE;
        if (alwaysInitializedVar > b) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(byte b) {
        // Keep this comment
        java.lang.Byte alwaysInitializedVar = Byte.MIN_VALUE;
        if (alwaysInitializedVar < b) {
            System.out.println("True!");
        }
    }

    public int replacePreDecrementWrapper(byte b) {
        // Keep this comment
        Byte preDecrementVar = --b;
        return preDecrementVar - 1;
    }

    public int replacePreIncrementWrapper(byte b) {
        // Keep this comment
        Byte preDecrementVar = ++b;
        return preDecrementVar + 1;
    }

    public int replacePostDecrementWrapper(byte b) {
        // Keep this comment
        Byte postDecrementVar = b--;
        return +postDecrementVar;
    }

    public int replacePostIncrementWrapper(byte b) {
        // Keep this comment
        Byte postIncrementVar = b++;
        return -postIncrementVar;
    }

    public byte replaceWrapperFromValueOf(byte b1) {
        // Keep this comment
        Byte varFromValueOf = Byte.valueOf(b1);
        return varFromValueOf++;
    }

    public byte replaceParentherizedWrapper(byte b1) {
        // Keep this comment
        Byte parentherizedVar = (Byte.MIN_VALUE);
        return parentherizedVar++;
    }

    public byte replaceCastWrapper(Byte b) {
        // Keep this comment
        Byte castVar = (byte) b;
        return castVar++;
    }

    public byte replaceObjectCastWrapper() {
        // Keep this comment
        Byte castVar = (Byte) Byte.MIN_VALUE;
        return castVar++;
    }

    public byte replaceWrapperInPreIncrement() {
        // Keep this comment
        Byte alwaysInitializedVar = Byte.MIN_VALUE;
        return ++alwaysInitializedVar;
    }

    public byte replaceWrapperInPreDecrement() {
        // Keep this comment
        Byte alwaysInitializedVar = Byte.MIN_VALUE;
        return --alwaysInitializedVar;
    }

    public byte replaceWrapperInPostDecrement() {
        // Keep this comment
        Byte alwaysInitializedVar = Byte.MIN_VALUE;
        return alwaysInitializedVar--;
    }

    public byte replaceWrapperInPostIncrement() {
        // Keep this comment
        Byte alwaysInitializedVar = Byte.MIN_VALUE;
        return alwaysInitializedVar++;
    }

    public void replaceWrapperInSwitch() {
        // Keep this comment
        Byte byteInSwitch = Byte.MIN_VALUE;
        switch (byteInSwitch) {
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
        Byte byteInArrayAccess = Byte.MIN_VALUE;
        return strings[byteInArrayAccess];
    }

    public byte replaceReturnedWrapper() {
        // Keep this comment
        Byte returnedByte = Byte.MIN_VALUE;
        return returnedByte;
    }

    public Object doNotBreakAutoboxing() {
        Byte returnedObject = Byte.MIN_VALUE;
        return returnedObject;
    }

    public byte replaceMultiReturnedWrapper(byte b) {
        // Keep this comment
        Byte returnedByte = Byte.MIN_VALUE;
        if (b > 0) {
            System.out.println("Positive");
            return returnedByte;
        } else {
            System.out.println("Negative");
            return returnedByte;
        }
    }

    public Byte replaceReturnedAutoBoxedWrapper(byte b) {
        // Keep this comment
        Byte returnedByte = Byte.MIN_VALUE;
        if (b > 0) {
            System.out.println("Positive");
            return returnedByte;
        } else {
            System.out.println("Negative");
            return returnedByte;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        Byte reassignedByte = Byte.MIN_VALUE;
        reassignedByte = 123;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        Byte multiReassignedByte = Byte.MIN_VALUE;
        multiReassignedByte = 1;
        multiReassignedByte = 2;
    }

    public void doNotReplaceNullWrapper() {
        Byte reassignedByte = Byte.MIN_VALUE;
        reassignedByte = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Byte, Observable> obsByByte) {
        Byte reassignedByte = Byte.MIN_VALUE;
        obsByByte.get(reassignedByte).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        Byte assignedByte = Byte.MIN_VALUE;
        Byte anotherByte = assignedByte;
    }

    public void replaceWrapperAssignedOnByteField() {
        // Keep this comment
        Byte assignedByte = Byte.MIN_VALUE;
        byteField = assignedByte;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        Byte assignedByte = Byte.MIN_VALUE;
        wrapperField = assignedByte;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Byte assignedByte = Byte.MIN_VALUE;
        objectField = assignedByte;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Byte assignedByte = Byte.MIN_VALUE;
        Byte anotherByte = assignedByte;
        Byte yetAnotherByte = assignedByte;
    }

    public Byte doNotReplaceMultiAutoBoxedWrapper() {
        Byte assignedByte = Byte.MIN_VALUE;
        Byte anotherByte = assignedByte;
        return assignedByte;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Byte returnedObject = Byte.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Byte doNotReplaceAssignedAndReturnedWrapper(Byte b) {
        Byte returnedObject = Byte.MIN_VALUE;
        returnedObject = b;
        return returnedObject;
    }
}
