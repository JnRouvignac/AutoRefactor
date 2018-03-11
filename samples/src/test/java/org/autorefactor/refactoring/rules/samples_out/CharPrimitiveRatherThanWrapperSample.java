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

public class CharPrimitiveRatherThanWrapperSample {

    public Character doNotRefactorFields = Character.MIN_VALUE;

    public char charField;

    public Character wrapperField;

    public Object objectField;

    public void replaceWrapper(char c) {
        // Keep this comment
        char alwaysInitializedVar = Character.MIN_VALUE;
        if (alwaysInitializedVar > c) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(char c) {
        // Keep this comment
        char alwaysInitializedVar = Character.MIN_VALUE;
        if (alwaysInitializedVar < c) {
            System.out.println("True!");
        }
    }

    public int replacePreDecrementWrapper(char c) {
        // Keep this comment
        char preDecrementVar = --c;
        return preDecrementVar - 1;
    }

    public int replacePreIncrementWrapper(char c) {
        // Keep this comment
        char preDecrementVar = ++c;
        return preDecrementVar + 1;
    }

    public int replacePostDecrementWrapper(char c) {
        // Keep this comment
        char postDecrementVar = c--;
        return -postDecrementVar;
    }

    public char replacePostIncrementWrapper(char c) {
        // Keep this comment
        char postIncrementVar = c++;
        return postIncrementVar++;
    }

    public int replaceWrapperFromValueOf(char c1) {
        // Keep this comment
        char varFromValueOf = Character.valueOf(c1);
        return +varFromValueOf;
    }

    public char replaceCastWrapper(Character c) {
        // Keep this comment
        char castVar = (char) c;
        return castVar++;
    }

    public char replaceObjectCastWrapper() {
        // Keep this comment
        char castVar = (Character) Character.MIN_VALUE;
        return castVar++;
    }

    public char replaceWrapperInPreIncrement() {
        // Keep this comment
        char alwaysInitializedVar = Character.MIN_VALUE;
        return ++alwaysInitializedVar;
    }

    public char replaceWrapperInPreDecrement() {
        // Keep this comment
        char alwaysInitializedVar = Character.MIN_VALUE;
        return --alwaysInitializedVar;
    }

    public char replaceWrapperInPostDecrement() {
        // Keep this comment
        char alwaysInitializedVar = Character.MIN_VALUE;
        return alwaysInitializedVar--;
    }

    public char replaceWrapperInPostIncrement() {
        // Keep this comment
        char alwaysInitializedVar = Character.MIN_VALUE;
        return alwaysInitializedVar++;
    }

    public void replaceWrapperInSwitch() {
        // Keep this comment
        char charInSwitch = Character.MIN_VALUE;
        switch (charInSwitch) {
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
        char charInArrayAccess = Character.MIN_VALUE;
        return strings[charInArrayAccess];
    }

    public char replaceReturnedWrapper() {
        // Keep this comment
        char returnedCharacter = Character.MIN_VALUE;
        return returnedCharacter;
    }

    public Object doNotBreakAutoboxing() {
        Character returnedObject = Character.MIN_VALUE;
        return returnedObject;
    }

    public char replaceMultiReturnedWrapper(char c) {
        // Keep this comment
        char returnedCharacter = Character.MIN_VALUE;
        if (c > 0) {
            System.out.println("Positive");
            return returnedCharacter;
        } else {
            System.out.println("Negative");
            return returnedCharacter;
        }
    }

    public Character replaceReturnedAutoBoxedWrapper(char c) {
        // Keep this comment
        char returnedCharacter = Character.MIN_VALUE;
        if (c > 0) {
            System.out.println("Positive");
            return returnedCharacter;
        } else {
            System.out.println("Negative");
            return returnedCharacter;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        char reassignedCharacter = Character.MIN_VALUE;
        reassignedCharacter = 123;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        char multiReassignedCharacter = Character.MIN_VALUE;
        multiReassignedCharacter = 123;
        multiReassignedCharacter = 456;
    }

    public void doNotReplaceNullWrapper() {
        Character reassignedCharacter = Character.MIN_VALUE;
        reassignedCharacter = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Character, Observable> obsByCharacter) {
        Character reassignedCharacter = Character.MIN_VALUE;
        obsByCharacter.get(reassignedCharacter).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        char assignedCharacter = Character.MIN_VALUE;
        Character anotherCharacter = assignedCharacter;
    }

    public void replaceWrapperAssignedOnCharacterField() {
        // Keep this comment
        char assignedCharacter = Character.MIN_VALUE;
        charField = assignedCharacter;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        char assignedCharacter = Character.MIN_VALUE;
        wrapperField = assignedCharacter;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Character assignedCharacter = Character.MIN_VALUE;
        objectField = assignedCharacter;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Character assignedCharacter = Character.MIN_VALUE;
        Character anotherCharacter = assignedCharacter;
        Character yetAnotherCharacter = assignedCharacter;
    }

    public void replaceBitAssignedWrapper(int anInteger, int anotherInteger,
            int yetAnotherInteger) {
        // Keep this comment
        char assignedCharacter = Character.MIN_VALUE;
        anInteger &= assignedCharacter;
        anotherInteger += assignedCharacter;
        yetAnotherInteger ^= assignedCharacter;
    }

    public Character doNotReplaceMultiAutoBoxedWrapper() {
        Character assignedCharacter = Character.MIN_VALUE;
        Character anotherCharacter = assignedCharacter;
        return assignedCharacter;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Character returnedObject = Character.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Character doNotReplaceAssignedAndReturnedWrapper(Character c) {
        Character returnedObject = Character.MIN_VALUE;
        returnedObject = c;
        return returnedObject;
    }
}
