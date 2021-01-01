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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules.samples_out;

import java.util.Arrays;
import java.util.regex.Pattern;

public class PatternRatherThanRegExStringSample {
    private String dateValidation= ".*";

    public boolean usePattern(String date1, String date2) {
        // Keep this comment
        Pattern dateValidation= Pattern.compile("\\d{4}\\-\\d{2}\\-\\d{2}");

        // Keep this comment too
        return dateValidation.matcher(date1).matches() && dateValidation.matcher(date2).matches();
    }

    public boolean usePatternAmongStatements(String date1, String date2) {
        // Keep this comment
        Pattern dateValidation= Pattern.compile("\\d{4}\\-\\d{2}\\-\\d{2}");
        System.out.println("Do other things");

        // Keep this comment too
        return dateValidation.matcher(date1).matches() && dateValidation.matcher(date2).matches();
    }

    public boolean doNotUsePatternForOneUse(String date) {
        String dateValidation= "\\d{4}\\-\\d{2}\\-\\d{2}";

        return date.matches(dateValidation);
    }

    public boolean doNotUsePatternWithOtherUse(String date1, String date2) {
        String dateValidation= "\\d{4}\\-\\d{2}\\-\\d{2}";
        System.out.println("The pattern is: " + dateValidation);

        return date1.matches(dateValidation) && date2.matches(dateValidation);
    }

    public boolean doNotUsePatternWithOtherMethod(String date1, String date2) {
        String dateValidation= "\\d{4}\\-\\d{2}\\-\\d{2}";

        return date1.matches(dateValidation) && "".equals(date2.replace(dateValidation, ""));
    }

    public String usePatternForReplace(String date1, String date2) {
        // Keep this comment
        Pattern dateValidation= Pattern.compile("\\d{4}\\-\\d{2}\\-\\d{2}");

        // Keep this comment too
        String dateText1= dateValidation.matcher(date1).replaceFirst("0000-00-00");
        // Keep this comment also
        String dateText2= dateValidation.matcher(date2).replaceAll("0000-00-00");

        return dateText1 + dateText2;
    }

    public String usePatternForSplit(String speech1, String speech2) {
        // Keep this comment
        Pattern line= Pattern.compile("\\r?\\n");

        // Keep this comment too
        String[] phrases1= line.split(speech1);
        // Keep this comment also
        String[] phrases2= line.split(speech2, 123);

        return Arrays.toString(phrases1) + Arrays.toString(phrases2);
    }

    public boolean doNotUsePatternInMultiDeclaration(String date1, String date2) {
        String dateValidation= "\\d{4}\\-\\d{2}\\-\\d{2}", foo= "bar";

        return date1.matches(dateValidation) && date2.matches(dateValidation);
    }

    public String usePatternForLocalVariableOnly(String date1, String date2, String date3) {
        String dateText1= date1.replaceFirst(dateValidation, "0000-00-00");
        // Keep this comment
        Pattern dateValidation= Pattern.compile("\\d{4}\\-\\d{2}\\-\\d{2}");

        // Keep this comment too
        String dateText2= dateValidation.matcher(date2).replaceFirst("0000-00-00");
        // Keep this comment also
        String dateText3= dateValidation.matcher(date3).replaceAll("0000-00-00");

        return dateText1 + dateText2 + dateText3;
    }

    public boolean doNotUsePatternOnMisplacedUse(String date1, String date2) {
        String dateValidation= "\\d{4}\\-\\d{2}\\-\\d{2}";

        return dateValidation.matches(date1) && dateValidation.matches(date2);
    }

    public String doNotUsePatternOnMisplacedParameter(String date1, String date2) {
        String dateValidation= "\\d{4}\\-\\d{2}\\-\\d{2}";

        String dateText1= date1.replaceFirst("0000-00-00", dateValidation);
        String dateText2= date2.replaceAll("0000-00-00", dateValidation);

        return dateText1 + dateText2;
    }

    public boolean usePatternFromVariable(String regex, String date1, String date2) {
        // Keep this comment
        Pattern dateValidation= Pattern.compile(regex);

        // Keep this comment too
        return dateValidation.matcher(date1).matches() && "".equals(dateValidation.matcher(date2).replaceFirst(""));
    }
}
