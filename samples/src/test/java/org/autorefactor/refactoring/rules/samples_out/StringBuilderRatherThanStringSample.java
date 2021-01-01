/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.ArrayList;
import java.util.List;

public class StringBuilderRatherThanStringSample {
    private String field= "";

    public static String useStringBuilder() {
        // Keep this comment
        StringBuilder variable= new StringBuilder();
        // Keep this comment also
        variable.append("foo");
        // Keep this comment too
        return variable.toString();
    }

    public static String useStringBuilderWithInitializer() {
        // Keep this comment
        StringBuilder variable= new StringBuilder("foo");
        // Keep this comment also
        variable.append("bar");
        // Keep this comment too
        return variable.toString();
    }

    public static String doNotRefactorNullString() {
        String variable= null;
        variable+= "foo";
        return variable;
    }

    public static String doNotRefactorMultideclaration() {
        String variable= "", anotherVariable= "";
        variable+= "foo";
        return variable;
    }

    public static String doNotRefactorStringUsedAsExpression() {
        String variable= "foo";
        if ((variable+= "bar").contains("i")) {
            return "foobar";
        }
        return variable;
    }

    public static String useStringBuilderOnBasicAssignment() {
        // Keep this comment
        StringBuilder variable= new StringBuilder();
        // Keep this comment also
        variable.append("foo");
        // Keep this comment too
        return variable.toString();
    }

    public static String doNotUseStringBuilderWithoutAppending() {
        String variable= "";
        variable= "foo" + variable;
        return variable;
    }

    public static String doNotRefactorWrongAssignmentOperator() {
        String variable= "";
        variable= "foo";
        return variable;
    }

    public static String doNotRefactorBadAssignmentOperator() {
        String variable= "";
        variable+= variable + "foo";
        return variable;
    }

    public static String doNotUseStringBuilderWithoutConcatenation() {
        String variable= "";
        return variable;
    }

    public static void doNotRefactorStringChangedAfterUse(String text) {
        String variable= "";
        variable+= text + "foo";
        System.out.println(variable);
        variable= variable + text + "bar";
    }

    public static String useStringBuilderWithExtendedOperation(String text) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();
        // Keep this comment also
        variable.append(text).append("foo");
        variable.append(text).append("bar");
        // Keep this comment too
        return variable.toString();
    }

    public static String useStringBuilderWithDifferentAssignment() {
        // Keep this comment
        StringBuilder variable= new StringBuilder();
        // Keep this comment also
        variable.append("foo");
        variable.append("bar");
        // Keep this comment too
        return variable.toString();
    }

    public static String doNotBuildStringSeveralTimes() {
        String variable= "";
        variable+= "foo";
        variable= variable + "bar";
        return variable + variable;
    }

    public static String useStringBuilderWithBlock(boolean isEnabled) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();

        if (isEnabled) {
            // Keep this comment also
            variable.append("foo");
            variable.append("bar");
        }

        // Keep this comment too
        return variable.toString();
    }

    public static String useStringBuilderWithLoop(List<String> texts) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();

        for (String text : texts) {
            // Keep this comment also
            variable.append(text);
            variable.append(",");
        }

        // Keep this comment too
        return variable.toString();
    }

    public static List<String> doNotStringifySeveralTimes(List<String> texts) {
        String variable= "";
        List<String> output= new ArrayList<>();

        for (String text : texts) {
            variable+= text;
            variable= variable + ",";
            output.add(variable);
        }

        return output;
    }

    public static String useStringBuilderWithWhile(String text, int i) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();

        while (i-- > 0) {
            // Keep this comment also
            variable.append(text);
            variable.append(",");
        }

        // Keep this comment too
        return variable.toString();
    }

    public static String useStringBuilderWithTry(String number, int i) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();

        try {
            while (i-- > 0) {
                // Keep this comment also
                variable.append(Integer.parseInt(number)).append(1);
                variable.append(",");
            }
        } catch (NumberFormatException e) {
            return "0";
        }

        // Keep this comment too
        return variable.toString();
    }

    public static String useStringBuilderWithFinally(String number) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();
        int i= 123;

        try {
            i+= Integer.parseInt(number);
        } catch (NumberFormatException e) {
            System.out.println("error");
        } finally {
            // Keep this comment also
            variable.append("foo");
            variable.append("bar");
        }

        // Keep this comment too
        return variable.toString() + i;
    }

    public static void doNotStringifySeveralTimesToo(List<String> texts) {
        String variable= "";
        variable+= "foo";
        variable= variable + "bar";

        for (String text : texts) {
            System.out.println(variable);
        }
    }

    public static String useStringBuilderWithConditionalRead(boolean isEnabled) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();

        if (isEnabled) {
            // Keep this comment also
            variable.append("foo");
            variable.append("bar");
            // Keep this comment too
            return variable.toString();
        }

        return "";
    }

    public static String useStringBuilderInElse(boolean isEnabled) {
        // Keep this comment
        StringBuilder variable= new StringBuilder();

        if (isEnabled) {
            return "OK";
        } else {
            // Keep this comment also
            variable.append("foo");
            variable.append("bar");
            // Keep this comment too
            return variable.toString();
        }
    }

    public static String useTwoStringBuilders(boolean isEnabled) {
        // Keep this comment
        StringBuilder variable1= new StringBuilder("First variable");
        StringBuilder variable2= new StringBuilder("Second variable");

        if (isEnabled) {
            // Keep this comment also
            variable2.append("foo");
            variable2.append("bar");
        } else {
            // Keep this comment also
            variable1.append("foo");
            variable1.append("bar");
        }

        // Keep this comment too
        return variable1.toString() + variable2.toString();
    }

    public static String doNotRefactorStringsWithoutConcatenation(boolean isEnabled) {
        String variable1= "First variable";
        String variable2= "Second variable";

        if (isEnabled) {
            variable1+= "foo";
            variable1= variable2 + "bar";
        } else {
            variable2+= "foo";
            variable2= variable1 + "bar";
        }

        return variable1 + variable2;
    }

    public static String doNotUseStringBuilderOnParameter(String variable) {
        variable+= "foo";
        return variable;
    }

    public String doNotUseStringBuilderOnField() {
        field+= "foo";
        return field;
    }
}
