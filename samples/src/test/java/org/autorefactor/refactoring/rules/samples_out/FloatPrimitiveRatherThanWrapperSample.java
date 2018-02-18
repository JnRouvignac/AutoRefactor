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

public class FloatPrimitiveRatherThanWrapperSample {

    public Float doNotRefactorFields = Float.MIN_VALUE;

    public float floatField;

    public Float wrapperField;

    public Object objectField;

    public void replaceWrapper(float f) {
        // Keep this comment
        float alwaysInitializedVar = Float.MIN_VALUE;
        if (alwaysInitializedVar > f) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(float f) {
        // Keep this comment
        float alwaysInitializedVar = Float.MIN_VALUE;
        if (alwaysInitializedVar < f) {
            System.out.println("True!");
        }
    }

    public boolean replacePlusWrapper(float f1, float f2) {
        // Keep this comment
        float plusVar = f1 + f2;
        return plusVar > 0;
    }

    public float replaceLessWrapper(float f1, float f2) {
        // Keep this comment
        float lessVar = f1 - f2;
        return -lessVar;
    }

    public float replaceTimesWrapper(float f1, float f2) {
        // Keep this comment
        float timesVar = f1 * f2;
        return timesVar + 100;
    }

    public float replaceDivideWrapper(float f1, float f2) {
        // Keep this comment
        float divideVar = f1 / f2;
        if (divideVar <= 0) {
            return -1;
        }
        return 1;
    }

    public float replaceMinusWrapper(float f) {
        // Keep this comment
        float minusVar = -f;
        return minusVar++;
    }

    public float replacePreDecrementWrapper(float f) {
        // Keep this comment
        float preDecrementVar = --f;
        return preDecrementVar++;
    }

    public float replacePreIncrementWrapper(float f) {
        // Keep this comment
        float preDecrementVar = ++f;
        return preDecrementVar++;
    }

    public float replacePostDecrementWrapper(float f) {
        // Keep this comment
        float postDecrementVar = f--;
        return postDecrementVar++;
    }

    public float replacePostIncrementWrapper(float f) {
        // Keep this comment
        float postIncrementVar = f++;
        return postIncrementVar++;
    }

    public float replaceWrapperFromValueOf(float f1) {
        // Keep this comment
        float varFromValueOf = Float.valueOf(f1);
        return varFromValueOf++;
    }

    public float replaceParentherizedWrapper(float f1, float f2) {
        // Keep this comment
        float parentherizedVar = (f1 + f2);
        return parentherizedVar++;
    }

    public float replaceComplexExprWrapper(float f1, float f2, float f3, float f4) {
        // Keep this comment
        float complexVar = f1 + f2 / (f3 - f4);
        return complexVar++;
    }

    public float replaceCastWrapper(Float f) {
        // Keep this comment
        float castVar = (float) f;
        return castVar++;
    }

    public float replaceWrapperInPreIncrement() {
        // Keep this comment
        float floatInPreIncrement = Float.MIN_VALUE;
        return ++floatInPreIncrement;
    }

    public float replaceWrapperInPreDecrement() {
        // Keep this comment
        float floatInPreDecrement = Float.MIN_VALUE;
        return --floatInPreDecrement;
    }

    public float replaceWrapperInPostDecrement() {
        // Keep this comment
        float floatInPostDecrement = Float.MIN_VALUE;
        return floatInPostDecrement--;
    }

    public float replaceWrapperInPostIncrement() {
        // Keep this comment
        float floatInPostIncrement = Float.MIN_VALUE;
        return floatInPostIncrement++;
    }

    public float replaceReturnedWrapper() {
        // Keep this comment
        float returnedFloat = Float.MIN_VALUE;
        return returnedFloat;
    }

    public Object doNotBreakAutoboxing() {
        Float returnedObject = Float.MIN_VALUE;
        return returnedObject;
    }

    public float replaceMultiReturnedWrapper(float f) {
        // Keep this comment
        float returnedFloat = Float.MIN_VALUE;
        if (f > 0) {
            System.out.println("Positive");
            return returnedFloat;
        } else {
            System.out.println("Negative");
            return returnedFloat;
        }
    }

    public Float replaceReturnedAutoBoxedWrapper(float f) {
        // Keep this comment
        float returnedFloat = Float.MIN_VALUE;
        if (f > 0) {
            System.out.println("Positive");
            return returnedFloat;
        } else {
            System.out.println("Negative");
            return returnedFloat;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        float reassignedFloat = Float.MIN_VALUE;
        reassignedFloat = 123f;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        float multiReassignedFloat = Float.MIN_VALUE;
        multiReassignedFloat = 123f;
        multiReassignedFloat = 456f;
    }

    public void doNotReplaceNullWrapper() {
        Float reassignedFloat = Float.MIN_VALUE;
        reassignedFloat = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Float, Observable> obsByFloat) {
        Float reassignedFloat = Float.MIN_VALUE;
        obsByFloat.get(reassignedFloat).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        float assignedLocal = Float.MIN_VALUE;
        Float anotherFloat = assignedLocal;
    }

    public void replaceWrapperAssignedOnFloatField() {
        // Keep this comment
        float assignedFloat = Float.MIN_VALUE;
        floatField = assignedFloat;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        float assignedWrapper = Float.MIN_VALUE;
        wrapperField = assignedWrapper;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Float assignedObject = Float.MIN_VALUE;
        objectField = assignedObject;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Float assignedFloat = Float.MIN_VALUE;
        Float anotherFloat = assignedFloat;
        Float yetAnotherFloat = assignedFloat;
    }

    public void replaceBitAssignedWrapper(Float aFloat, Float anotherFloat,
            Float yetAnotherFloat, Float evenAnotherFloat) {
        // Keep this comment
        float assignedFloat = Float.MIN_VALUE;
        aFloat += assignedFloat;
        anotherFloat -= assignedFloat;
        yetAnotherFloat *= assignedFloat;
        evenAnotherFloat /= assignedFloat;
    }

    public Float doNotReplaceMultiAutoBoxedWrapper() {
        Float assignedFloat = Float.MIN_VALUE;
        Float anotherFloat = assignedFloat;
        return assignedFloat;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Float returnedObject = Float.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Float doNotReplaceAssignedAndReturnedWrapper(Float f) {
        Float returnedObject = Float.MIN_VALUE;
        returnedObject = f;
        return returnedObject;
    }
}
