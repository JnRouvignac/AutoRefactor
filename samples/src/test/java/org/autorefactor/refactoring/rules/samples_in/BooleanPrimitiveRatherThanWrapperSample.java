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

public class BooleanPrimitiveRatherThanWrapperSample {

    public Boolean doNotRefactorFields = Boolean.TRUE;

    public boolean booleanField;

    public Boolean wrapperField;

    public Object objectField;

    public void replaceWrapper(boolean b) {
        // Keep this comment
        Boolean alwaysInitializedVar = Boolean.TRUE;
        if (alwaysInitializedVar && b) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(boolean b) {
        // Keep this comment
        java.lang.Boolean alwaysInitializedVar = Boolean.FALSE;
        if (alwaysInitializedVar && b) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInCast() {
        // Keep this comment
        Boolean alwaysInitializedVar = Boolean.FALSE;
        if ((boolean) alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInParenthesis() {
        // Keep this comment
        Boolean alwaysInitializedVar = Boolean.FALSE;
        if ((alwaysInitializedVar)) {
            System.out.println("True!");
        }
    }

    public void replaceGreaterWrapper(int i) {
        // Keep this comment
        Boolean greaterVar = i > 0;
        if (greaterVar) {
            System.out.println("True!");
        }
    }

    public void replaceLesserWrapper(int i) {
        // Keep this comment
        Boolean lesserVar = i < 0;
        if (lesserVar) {
            System.out.println("True!");
        }
    }

    public void replaceAndWrapper(boolean b1, boolean b2) {
        // Keep this comment
        Boolean andVar = b1 && b2;
        if (andVar) {
            System.out.println("True!");
        }
    }

    public void replaceOrWrapper(boolean b1, boolean b2) {
        // Keep this comment
        Boolean orVar = b1 || b2;
        if (orVar) {
            System.out.println("True!");
        }
    }

    public void replaceOppositeWrapper(boolean b) {
        // Keep this comment
        Boolean oppositeVar = !b;
        if (oppositeVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperFromValueOf(boolean b1) {
        // Keep this comment
        Boolean varFromValueOf = Boolean.valueOf(b1);
        if (varFromValueOf) {
            System.out.println("True!");
        }
    }

    public void replaceParentherizedWrapper(boolean b1, boolean b2) {
        // Keep this comment
        Boolean parentherizedVar = (b1 || b2);
        if (parentherizedVar) {
            System.out.println("True!");
        }
    }

    public void replaceComplexExprWrapper(boolean b1, boolean b2, boolean b3, boolean b4) {
        // Keep this comment
        Boolean complexVar = b1 ? !b2 : (b3 || b4);
        if (complexVar) {
            System.out.println("True!");
        }
    }

    public void replaceCastWrapper(Boolean b) {
        // Keep this comment
        Boolean castVar = (boolean) b;
        if (castVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInPrefixExpr() {
        // Keep this comment
        Boolean alwaysInitializedVar = Boolean.TRUE;
        if (!alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInIf() {
        // Keep this comment
        Boolean alwaysInitializedVar = Boolean.TRUE;
        if (alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInWhile() {
        // Keep this comment
        Boolean alwaysInitializedVar = true;
        while (alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInDoWhile() {
        // Keep this comment
        Boolean alwaysInitializedVar = false;
        do {
            System.out.println("True!");
        } while (alwaysInitializedVar);
    }

    public String replaceWrapperInConditionalExpr() {
        // Keep this comment
        Boolean alwaysInitializedVar = Boolean.TRUE;
        return alwaysInitializedVar ? "foo" : "bar";
    }

    public boolean replaceReturnedWrapper() {
        // Keep this comment
        Boolean returnedBoolean = Boolean.TRUE;
        return returnedBoolean;
    }

    public Object doNotBreakAutoboxing() {
        Boolean returnedObject = Boolean.TRUE;
        return returnedObject;
    }

    public boolean replaceMultiReturnedWrapper(int i) {
        // Keep this comment
        Boolean returnedBoolean = Boolean.TRUE;
        if (i > 0) {
            System.out.println("Positive");
            return returnedBoolean;
        } else {
            System.out.println("Negative");
            return returnedBoolean;
        }
    }

    public Boolean replaceReturnedAutoBoxedWrapper(int i) {
        // Keep this comment
        Boolean returnedBoolean = Boolean.FALSE;
        if (i > 0) {
            System.out.println("Positive");
            return returnedBoolean;
        } else {
            System.out.println("Negative");
            return returnedBoolean;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        Boolean reassignedBoolean = Boolean.TRUE;
        reassignedBoolean = Boolean.FALSE;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        Boolean multiReassignedBoolean = Boolean.TRUE;
        multiReassignedBoolean = Boolean.FALSE;
        multiReassignedBoolean = Boolean.TRUE;
    }

    public void doNotReplaceNullWrapper() {
        Boolean reassignedBoolean = Boolean.TRUE;
        reassignedBoolean = null;
    }

    public void doNotReplaceWrapperPassedAsObject(Map<Boolean, Observable> obsByBoolean) {
        Boolean reassignedBoolean = Boolean.TRUE;
        obsByBoolean.get(reassignedBoolean).notifyObservers();
    }

    public void replaceAssignedWrapper() {
        // Keep this comment
        Boolean assignedBoolean = Boolean.TRUE;
        Boolean anotherBoolean = assignedBoolean;
    }

    public void replaceWrapperAssignedOnBooleanField() {
        // Keep this comment
        Boolean assignedBoolean = Boolean.TRUE;
        booleanField = assignedBoolean;
    }

    public void replaceWrapperAssignedOnWrapperField() {
        // Keep this comment
        Boolean assignedBoolean = Boolean.TRUE;
        wrapperField = assignedBoolean;
    }

    public void doNotReplaceWrapperAssignedOnObjectField() {
        Boolean assignedBoolean = Boolean.TRUE;
        objectField = assignedBoolean;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Boolean assignedBoolean = Boolean.TRUE;
        Boolean anotherBoolean = assignedBoolean;
        Boolean yetAnotherBoolean = assignedBoolean;
    }

    public void replaceBitAssignedWrapper(Boolean aBoolean, Boolean anotherBoolean,
            Boolean yetAnotherBoolean) {
        // Keep this comment
        Boolean assignedBoolean = Boolean.TRUE;
        aBoolean &= assignedBoolean;
        anotherBoolean |= assignedBoolean;
        yetAnotherBoolean ^= assignedBoolean;
    }

    public Boolean doNotReplaceMultiAutoBoxedWrapper() {
        Boolean assignedBoolean = Boolean.TRUE;
        Boolean anotherBoolean = assignedBoolean;
        return assignedBoolean;
    }

    public void doNotBreakAutoboxingOnAssignment() {
        Boolean returnedObject = Boolean.TRUE;
        Object anotherObject = returnedObject;
    }

    public void doNotReplaceRessignedWrapper(Boolean b) {
        Boolean returnedObject = Boolean.TRUE;
        try {
            returnedObject = b;
        } catch (Exception e) {
            System.out.println("Error!");
        }
    }

    public Boolean doNotReplaceAssignedAndReturnedWrapper(Boolean b) {
        Boolean returnedObject = Boolean.FALSE;
        returnedObject = b;
        return returnedObject;
    }
}
