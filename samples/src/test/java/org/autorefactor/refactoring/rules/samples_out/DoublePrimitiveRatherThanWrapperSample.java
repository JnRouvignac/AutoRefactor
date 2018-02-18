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

public class DoublePrimitiveRatherThanWrapperSample {

    public Double doNotRefactorFields = Double.MIN_VALUE;

    public double doubleField;

    public Double wrapperField;

    public Object objectField;

    public void replaceWrapper(double d) {
        // Keep this comment
        double alwaysInitializedVar = Double.MIN_VALUE;
        if (alwaysInitializedVar > d) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(double d) {
        // Keep this comment
        double alwaysInitializedVar = Double.MIN_VALUE;
        if (alwaysInitializedVar < d) {
            System.out.println("True!");
        }
    }

    public boolean replacePlusWrapper(double d1, double d2) {
        // Keep this comment
        double plusVar = d1 + d2;
        return plusVar > 0;
    }

    public double replaceLessWrapper(double d1, double d2) {
        // Keep this comment
        double lessVar = d1 - d2;
        return -lessVar;
    }

    public double replaceTimesWrapper(double d1, double d2) {
        // Keep this comment
        double timesVar = d1 * d2;
        return timesVar + 100;
    }

    public double replaceDivideWrapper(double d1, double d2) {
        // Keep this comment
        double divideVar = d1 / d2;
        if (divideVar <= 0) {
            return -1;
        }
        return 1;
    }

    public double replaceMinusWrapper(double d) {
        // Keep this comment
        double minusVar = -d;
        return minusVar++;
    }

    public double replacePreDecrementWrapper(double d) {
        // Keep this comment
        double preDecrementVar = --d;
        return preDecrementVar++;
    }

    public double replacePreIncrementWrapper(double d) {
        // Keep this comment
        double preDecrementVar = ++d;
        return preDecrementVar++;
    }

    public double replacePostDecrementWrapper(double d) {
        // Keep this comment
        double postDecrementVar = d--;
        return postDecrementVar++;
    }

    public double replacePostIncrementWrapper(double d) {
        // Keep this comment
        double postIncrementVar = d++;
        return postIncrementVar++;
    }

    public double replaceWrapperFromValueOf(double d1) {
        // Keep this comment
        double varFromValueOf = Double.valueOf(d1);
        return varFromValueOf++;
    }

    public double replaceParentherizedWrapper(double d1, double d2) {
        // Keep this comment
        double parentherizedVar = (d1 + d2);
        return parentherizedVar++;
    }

    public double replaceComplexExprWrapper(double d1, double d2, double d3, double d4) {
        // Keep this comment
        double complexVar = d1 + d2 / (d3 - d4);
        return complexVar++;
    }

    public double replaceCastWrapper(Double d) {
        // Keep this comment
        double castVar = (double) d;
        return castVar++;
    }

    public double replaceWrapperInPreIncrement() {
        // Keep this comment
        double alwaysInitializedVar = Double.MIN_VALUE;
        return ++alwaysInitializedVar;
    }

    public double replaceWrapperInPreDecrement() {
        // Keep this comment
        double alwaysInitializedVar = Double.MIN_VALUE;
        return --alwaysInitializedVar;
    }

    public double replaceWrapperInPostDecrement() {
        // Keep this comment
        double alwaysInitializedVar = Double.MIN_VALUE;
        return alwaysInitializedVar--;
    }

    public double replaceWrapperInPostIncrement() {
        // Keep this comment
        double alwaysInitializedVar = Double.MIN_VALUE;
        return alwaysInitializedVar++;
    }

    public double replaceReturnedWrapper() {
        // Keep this comment
        double returnedDouble = Double.MIN_VALUE;
        return returnedDouble;
    }

    public Object doNotBreakAutoboxing() {
        Double returnedObject = Double.MIN_VALUE;
        return returnedObject;
    }

    public double replaceMultiReturnedWrapper(double d) {
        // Keep this comment
        double returnedDouble = Double.MIN_VALUE;
        if (d > 0) {
            System.out.println("Positive");
            return returnedDouble;
        } else {
            System.out.println("Negative");
            return returnedDouble;
        }
    }

    public Double replaceReturnedAutoBoxedWrapper(double d) {
        // Keep this comment
        double returnedDouble = Double.MIN_VALUE;
        if (d > 0) {
            System.out.println("Positive");
            return returnedDouble;
        } else {
            System.out.println("Negative");
            return returnedDouble;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        double reassignedDouble = Double.MIN_VALUE;
        reassignedDouble = 123.0;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        double multiReassignedDouble = Double.MIN_VALUE;
        multiReassignedDouble = 123.0;
        multiReassignedDouble = 456.0;
    }

    public void doNotReplaceNullWrapper() {
        Double reassignedDouble = Double.MIN_VALUE;
        reassignedDouble = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Double, Observable> obsByDouble) {
        Double reassignedDouble = Double.MIN_VALUE;
        obsByDouble.get(reassignedDouble).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        double assignedDouble = Double.MIN_VALUE;
        Double anotherDouble = assignedDouble;
    }

    public void replaceWrapperAssignedOnDoubleField() {
        // Keep this comment
        double assignedDouble = Double.MIN_VALUE;
        doubleField = assignedDouble;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        double assignedDouble = Double.MIN_VALUE;
        wrapperField = assignedDouble;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Double assignedDouble = Double.MIN_VALUE;
        objectField = assignedDouble;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Double assignedDouble = Double.MIN_VALUE;
        Double anotherDouble = assignedDouble;
        Double yetAnotherDouble = assignedDouble;
    }

    public void replaceBitAssignedWrapper(Double aDouble, Double anotherDouble,
            Double yetAnotherDouble) {
        // Keep this comment
        double assignedDouble = Double.MIN_VALUE;
        aDouble -= assignedDouble;
        anotherDouble += assignedDouble;
        yetAnotherDouble *= assignedDouble;
    }

    public Double doNotReplaceMultiAutoBoxedWrapper() {
        Double assignedDouble = Double.MIN_VALUE;
        Double anotherDouble = assignedDouble;
        return assignedDouble;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Double returnedObject = Double.MIN_VALUE;
        Object anotherObject = returnedObject;
    }

    public Double doNotReplaceAssignedAndReturnedWrapper(Double d) {
        Double returnedObject = Double.MIN_VALUE;
        returnedObject = d;
        return returnedObject;
    }
}
