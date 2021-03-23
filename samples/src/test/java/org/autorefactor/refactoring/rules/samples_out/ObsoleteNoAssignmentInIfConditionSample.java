/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2019 Fabrice Tiercelin - Change the parsing of condition
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

import java.util.Queue;

public class ObsoleteNoAssignmentInIfConditionSample {
    public void moveLeftHandSideAssignmentBeforeIf(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        i = q.poll();
        // Keep this comment
        if (i != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveRightHandSideAssignmentBeforeIf(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        i = q.poll();
        // Keep this comment
        if (null != i) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentBeforeIfMultipleParenthesesToRemove(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        i = q.poll();
        // Keep this comment
        if (i != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentBeforeIfAndMergeWithDeclaration(Queue<Integer> q) {
        Integer i= q.poll();
        // Keep this comment
        if (i != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentBelowDeclaration(Queue<Integer> q) {
        Integer i = q.poll();
        i = q.poll();
        // Keep this comment
        if (i != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void erasePassiveValue(Queue<Integer> q) {
        Integer i = q.poll();
        // Keep this comment
        if (i != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentWithoutParenthesis(Queue<Boolean> q) {
        Boolean b= q.poll();
        // Keep this comment
        if (b) {
            System.out.println("Value=" + b);
        } else {
            System.out.println("Empty");
        }
    }

    public void doNotRefactor(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        // Keep this comment
        if (q == null) {
            System.out.println("Null queue");
        } else if ((i = q.poll()) != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentBeforeIfAtConditionOfTernaryExpression(String s, int i) {
        final char c= s.charAt(i);
        // Keep this comment
        if (c == 'A' ? c == 'B' : c == 'C') {
            System.out.println("A, B or C");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public void doNotMoveAssignmentBeforeIfAtLeftOperandOfTernaryExpression(String s, int i, char c) {
        if (c == 'A' ? (c = s.charAt(i)) == 'B' : c == 'C') {
            System.out.println("Found");
        } else {
            System.out.println("Not found");
        }
    }

    public void doNotMoveAssignmentBeforeIfAtRightOperandOfTernaryExpression(String s, int i, char c) {
        if (c == 'A' ? c == 'B' : (c = s.charAt(i)) == 'C') {
            System.out.println("Found");
        } else {
            System.out.println("Not found");
        }
    }

    public void moveAssignmentBeforeIfAtStartOfInfixExpression(String s, int i) {
        final char c= s.charAt(i);
        // Keep this comment
        if (c == 'A' || c == 'B' || c == 'C') {
            System.out.println("A, B or C");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public void moveNotConditionalAssignment(String s, int i, boolean isValid) {
        final char c= s.charAt(i);
        // Keep this comment
        if (isValid | c == 'A') {
            System.out.println("valid or A");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public void moveAssignmentInComplexExpression(String s, int i, boolean isValid) {
        final char c= s.charAt(i);
        // Keep this comment
        if (!(isValid | (i == 10 & c == 'A'))) {
            System.out.println("valid or A");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public void doNotMoveAssignmentBeforeIfInsideInfixExpression(String s, int i, char c) {
        if (c == 'A' || (c = s.charAt(i)) == 'A' || c == 'B' || c == 'C') {
            System.out.println("A, B or C");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public void doNotMoveAssignmentAfterActiveCondition(String s, int i, char c) {
        if (i++ == 10 || (c = s.charAt(i)) == 'A' || c == 'B' || c == 'C') {
            System.out.println("A, B or C");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public boolean refactorSingleStatementBlock(int i, int j) {
        if (i > 0) {
            i = j;
            if (i < 10)
                return true;
        }
        return false;
    }

    public void doNotRefactorConditionalAnd(Queue<Boolean> q, boolean isValid) {
        Boolean i;
        if (isValid && (i = q.poll())) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveLeftHandSideAssignmentInSwitch(Queue<Integer> q, int discriminant) {
        Integer i;
        System.out.println("Before polling");
        switch (discriminant) {
        case 0:
            i = q.poll();
            // Keep this comment
            if (i != null) {
                System.out.println("Value=" + i);
            } else {
                System.out.println("Empty");
            }
        case 1:
            System.out.println("Another case");
        }
    }
}
