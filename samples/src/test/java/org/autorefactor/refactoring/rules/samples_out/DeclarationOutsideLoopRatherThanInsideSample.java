/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.List;

public class DeclarationOutsideLoopRatherThanInsideSample {
    public String moveObjectDecl(int count) {
        StringBuilder concat = new StringBuilder();

        String newNumber;
        for (int i = 0; i < count; i++) {
            // Keep this comment
            newNumber = String.valueOf(i);
            concat.append(newNumber);
            concat.append(";");
        }

        return concat.toString();
    }

    public String moveObjectDeclFromLoops(int count) {
        StringBuilder concat = new StringBuilder();

        String newNumber;
        String anotherNewNumber;
        for (int i = 0; i < count; i++) {
            // Keep this comment
            newNumber = String.valueOf(i);
            concat.append(newNumber);
            concat.append(";");

            for (int j = 0; j < count; j++) {
                // Keep this comment
                anotherNewNumber = String.valueOf(j);
                concat.append(anotherNewNumber);
                concat.append(";");
            }
        }

        return concat.toString();
    }

    public int moveFromEnhancedLoop(List<String> myList) {
        int total = 0;

        int newNumber;
        for (String number : myList) {
            // Keep this comment
            newNumber = Integer.parseInt(number);
            total += newNumber;
        }

        return total;
    }

    public int moveFromWhile(int max) {
        int result = 1;

        int newNumber;
        while (result < max) {
            // Keep this comment
            newNumber = result * result;
            result += newNumber;
        }

        return result;
    }

    public int moveFromDoWhile(int max) {
        int result = 1;

        int newNumber;
        do {
            // Keep this comment
            newNumber = result * result;
            result += newNumber;
        } while (result < max);

        return result;
    }

    public int moveDeclWithoutInit(List<String> myList) {
        int total = 0;

        // Keep this comment
        int newNumber;
        for (String number : myList) {
            newNumber = Integer.parseInt(number);
            total += newNumber;
        }

        return total;
    }

    public int movePrimitiveTypeDecl(List<String> myList) {
        int total = 0;

        int newNumber;
        for (String number : myList) {
            // Keep this comment
            newNumber = Integer.parseInt(number);
            total += newNumber;
        }

        return total;
    }

    public String moveObjectDecls(List<Integer> myList1, List<Integer> myList2) {
        StringBuilder concat = new StringBuilder();

        String newNumber;
        for (Integer number : myList1) {
            // Keep this comment
            newNumber = String.valueOf(number);
            concat.append(newNumber);
        }

        String anotherNumber;
        for (Integer number : myList2) {
            // Keep this comment
            anotherNumber = String.valueOf(number);
            concat.append(anotherNumber);
        }

        return concat.toString();
    }

    public String doNotMoveFinalDecl(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        for (Integer number : myList) {
            final String newNumber = String.valueOf(number);
            concat.append(newNumber);
        }

        return concat.toString();
    }

    public String doNotMoveMultiFragments(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        for (Integer number : myList) {
            String newNumber, sameNumber = String.valueOf(number);
            newNumber = "0";
            concat.append(newNumber);
            concat.append("<");
            concat.append(sameNumber);
        }

        return concat.toString();
    }

    public String doNotMoveWhenConflicts(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        for (Integer number : myList) {
            String myNumber = String.valueOf(number);
            concat.append(myNumber);
        }

        String myNumber = " and 10";

        return concat.toString() + myNumber;
    }

    public String doNotMoveWhenConflictsInLoopParameter(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        for (Integer number : myList) {
            String myNumber = String.valueOf(number);
            concat.append(myNumber);
        }

        for (Integer myNumber : myList) {
            concat.append(myNumber);
        }

        return concat.toString();
    }

    public String moveWhenNoConflictsBefore(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        {
            String theNumber = "10 and ";
            System.out.println(theNumber);
        }

        String theNumber;
        for (Integer number : myList) {
            // Keep this comment
            theNumber = String.valueOf(number);
            concat.append(theNumber);
        }

        return concat.toString();
    }

    public String doNotMoveWhenConflictsAfter(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        for (Integer number : myList) {
            String oneNumber = String.valueOf(number);
            concat.append(oneNumber);
        }

        {
            String oneNumber = " and 10";
            System.out.println(oneNumber);
        }

        return concat.toString();
    }

    public String doNotMoveDeclFromOtherScope(List<Integer> myList) {
        StringBuilder concat = new StringBuilder();

        for (Integer number : myList) {
            if (number > 0) {
                String aNumber = String.valueOf(number);
                concat.append(aNumber);
            }
        }

        return concat.toString();
    }

    public String doNotMoveAnnotatedDecl(List<Object> texts) {
        StringBuilder concat = new StringBuilder();

        for (Object object : texts) {
            @SuppressWarnings("unchecked")
            List<String> text = (List<String>) object;
            concat.append(text);
            concat.append(";");
        }

        return concat.toString();
    }
}
