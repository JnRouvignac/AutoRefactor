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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.ArrayList;
import java.util.List;

public class StringBuilderRatherThanStringSample {
    private String field= "";

    public static String useStringBuilder() {
        // Keep this comment
        String variable= "";
        // Keep this comment also
        variable+= "foo";
        // Keep this comment too
        return variable;
    }

    public static String useStringBuilderWithInitializer() {
        // Keep this comment
        String variable= "foo";
        // Keep this comment also
        variable+= "bar";
        // Keep this comment too
        return variable;
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
        String variable= "";
        // Keep this comment also
        variable= variable + "foo";
        // Keep this comment too
        return variable;
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
        String variable= "";
        // Keep this comment also
        variable+= text + "foo";
        variable= variable + text + "bar";
        // Keep this comment too
        return variable;
    }

    public static String useStringBuilderWithDifferentAssignment() {
        // Keep this comment
        String variable= "";
        // Keep this comment also
        variable+= "foo";
        variable= variable + "bar";
        // Keep this comment too
        return variable;
    }

    public static String doNotBuildStringSeveralTimes() {
        String variable= "";
        variable+= "foo";
        variable= variable + "bar";
        return variable + variable;
    }

    public static String useStringBuilderWithBlock(boolean isEnabled) {
        // Keep this comment
        String variable= "";

        if (isEnabled) {
            // Keep this comment also
            variable+= "foo";
            variable= variable + "bar";
        }

        // Keep this comment too
        return variable;
    }

    public static String useStringBuilderWithLoop(List<String> texts) {
        // Keep this comment
        String variable= "";

        for (String text : texts) {
            // Keep this comment also
            variable+= text;
            variable= variable + ",";
        }

        // Keep this comment too
        return variable;
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
        String variable= "";

        while (i-- > 0) {
            // Keep this comment also
            variable+= text;
            variable= variable + ",";
        }

        // Keep this comment too
        return variable;
    }

    public static String useStringBuilderWithTry(String number, int i) {
        // Keep this comment
        String variable= "";

        try {
            while (i-- > 0) {
                // Keep this comment also
                variable+= (Integer.parseInt(number) + 1);
                variable= variable + ",";
            }
        } catch (NumberFormatException e) {
            return "0";
        }

        // Keep this comment too
        return variable;
    }

    public static String useStringBuilderWithFinally(String number) {
        // Keep this comment
        String variable= "";
        int i= 123;

        try {
            i+= Integer.parseInt(number);
        } catch (NumberFormatException e) {
            System.out.println("error");
        } finally {
            // Keep this comment also
            variable+= "foo";
            variable= variable + "bar";
        }

        // Keep this comment too
        return variable + i;
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
        String variable= "";

        if (isEnabled) {
            // Keep this comment also
            variable+= "foo";
            variable= variable + "bar";
            // Keep this comment too
            return variable;
        }

        return "";
    }

    public static String useStringBuilderInElse(boolean isEnabled) {
        // Keep this comment
        String variable= "";

        if (isEnabled) {
            return "OK";
        } else {
            // Keep this comment also
            variable+= "foo";
            variable= variable + "bar";
            // Keep this comment too
            return variable;
        }
    }

    public static String useTwoStringBuilders(boolean isEnabled) {
        // Keep this comment
        String variable1= "First variable";
        String variable2= "Second variable";

        if (isEnabled) {
            // Keep this comment also
            variable2+= "foo";
            variable2= variable2 + "bar";
        } else {
            // Keep this comment also
            variable1+= "foo";
            variable1= variable1 + "bar";
        }

        // Keep this comment too
        return variable1 + variable2;
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
