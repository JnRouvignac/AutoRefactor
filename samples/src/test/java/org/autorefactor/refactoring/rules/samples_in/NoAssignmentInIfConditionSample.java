/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Queue;

public class NoAssignmentInIfConditionSample {

    public void moveLeftHandSideAssignmentBeforeIf(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        // Keep this comment
        if ((i = q.poll()) != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveRightHandSideAssignmentBeforeIf(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        // Keep this comment
        if (null != (i = q.poll())) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentBeforeIfMultipleParenthesesToRemove(Queue<Integer> q) {
        Integer i;
        System.out.println("Before polling");
        // Keep this comment
        if ((((i = q.poll()))) != null) {
            System.out.println("Value=" + i);
        } else {
            System.out.println("Empty");
        }
    }

    public void moveAssignmentBeforeIfAndMergeWithDeclaration(Queue<Integer> q) {
        Integer i;
        // Keep this comment
        if ((i = q.poll()) != null) {
            System.out.println("Value=" + i);
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

    public void moveAssignmentBeforeIfAtStartOfInfixExpression(String s, int i) {
        final char c;
        // Keep this comment
        if ((c = s.charAt(i)) == 'A' || c == 'B' || c == 'C') {
            System.out.println("A, B or C");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public void doNotMoveAssignmentBeforeIfInsideInfixExpression(String s, int i, char c) {
        // Keep this comment
        if (c == 'A' || (c = s.charAt(i)) == 'A' || c == 'B' || c == 'C') {
            System.out.println("A, B or C");
        } else {
            System.out.println("Not A, B or C");
        }
    }

    public boolean doNotRefactorSingleStatementBlock(int i, int j) {
        if (i > 0)
            if ((i = j) < 10)
                return true;
        return false;
    }
}
