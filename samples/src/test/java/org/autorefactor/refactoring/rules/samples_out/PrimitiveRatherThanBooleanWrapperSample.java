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

public class PrimitiveRatherThanBooleanWrapperSample {

    public Boolean doNotRefactorFields = Boolean.TRUE;

    public void replaceWrapper(boolean b) {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        if (alwaysInitializedVar && b) {
            System.out.println("True!");
        }
    }

    public void replaceFullyQualifiedWrapper(boolean b) {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        if (alwaysInitializedVar && b) {
            System.out.println("True!");
        }
    }

    public void replaceGreaterWrapper(int i) {
        // Keep this comment
        boolean greaterVar = i > 0;
        if (greaterVar) {
            System.out.println("True!");
        }
    }

    public void replaceLesserWrapper(int i) {
        // Keep this comment
        boolean lesserVar = i < 0;
        if (lesserVar) {
            System.out.println("True!");
        }
    }

    public void replaceAndWrapper(boolean b1, boolean b2) {
        // Keep this comment
        boolean andVar = b1 && b2;
        if (andVar) {
            System.out.println("True!");
        }
    }

    public void replaceOrWrapper(boolean b1, boolean b2) {
        // Keep this comment
        boolean orVar = b1 || b2;
        if (orVar) {
            System.out.println("True!");
        }
    }

    public void replaceOppositeWrapper(boolean b) {
        // Keep this comment
        boolean oppositeVar = !b;
        if (oppositeVar) {
            System.out.println("True!");
        }
    }

    public void replaceParentherizedWrapper(boolean b1, boolean b2) {
        // Keep this comment
        boolean parentherizedVar = (b1 || b2);
        if (parentherizedVar) {
            System.out.println("True!");
        }
    }

    public void replaceCastWrapper(Boolean b) {
        // Keep this comment
        boolean castVar = (boolean) b;
        if (castVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInPrefixExpr() {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        if (!alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInIf() {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        if (alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInWhile() {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        while (alwaysInitializedVar) {
            System.out.println("True!");
        }
    }

    public void replaceWrapperInDoWhile() {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        do {
            System.out.println("True!");
        } while (alwaysInitializedVar);
    }

    public String replaceWrapperInConditionalExpr() {
        // Keep this comment
        boolean alwaysInitializedVar = Boolean.TRUE;
        return alwaysInitializedVar ? "foo" : "bar";
    }

    public boolean replaceReturnedWrapper() {
        // Keep this comment
        boolean returnedBoolean = Boolean.TRUE;
        return returnedBoolean;
    }

    public boolean replaceMultiReturnedWrapper(boolean b) {
        // Keep this comment
        boolean returnedBoolean = Boolean.TRUE;
        if (b) {
            System.out.println("True!");
            return returnedBoolean;
        } else {
            System.out.println("False!");
            return returnedBoolean;
        }
    }

    public void replaceReassignedWrapper() {
        // Keep this comment
        boolean reassignedBoolean = Boolean.TRUE;
        reassignedBoolean = Boolean.FALSE;
    }

    public void replaceMultiReassignedWrapper() {
        // Keep this comment
        boolean multiReassignedBoolean = Boolean.TRUE;
        multiReassignedBoolean = Boolean.FALSE;
        multiReassignedBoolean = Boolean.TRUE;
    }

    public void doNotReplaceNullWrapper() {
        Boolean reassignedBoolean = Boolean.TRUE;
        reassignedBoolean = null;
    }

    public void doNotReplaceMultiAssignedWrapper() {
        Boolean assignedBoolean = Boolean.TRUE;
        Boolean anotherBoolean = assignedBoolean;
        Boolean yetAnotherBoolean = assignedBoolean;
    }
}
