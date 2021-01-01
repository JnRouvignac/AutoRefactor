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

public class OptimizeRegExSample {
    public boolean optimizeParameter(String text) {
        // Keep this comment
        return text.matches("\\d{4}\\-\\d{3}\\-\\d{2}");
    }

    public boolean optimizeField(String text) {
        // Keep this comment
        String dateValidation= "\\d{4}\\-\\d{3}\\-\\d{2}";

        return text.matches(dateValidation);
    }

    public boolean doNotTouchInvalidPattern(String text) {
        return text.matches("[\\[0-9]");
    }

    public boolean optimizeWithReassignment(String text) {
        // Keep this comment
        String dateValidation= "\\d{4}\\-\\d{3}\\-\\d{2}";
        dateValidation = "foo";

        return text.matches(dateValidation);
    }

    public boolean doNotTouchConcatenatedPattern(String text) {
        String dateValidation= "[0-9]{4}\\-\\d{3}\\-\\d{2}";
        dateValidation += "foo";

        return text.matches(dateValidation);
    }

    public boolean optimizeOccurrence(String text) {
        // Keep this comment
        return text.matches("\\d{4}\\-\\d{3}\\-\\d*");
    }

    public boolean optimizeWithPlus(String text) {
        // Keep this comment
        return text.matches("\\d{4}\\-\\d{3}\\-\\d+");
    }

    public boolean optimizeWithQuestionDot(String text) {
        // Keep this comment
        return text.matches("\\d{4}\\-\\d{3}\\-\\d?");
    }

    public boolean doNotTouchRegExWithComment(String text) {
        return text.matches("\\d{4}\\-\\d{3}\\-(?#\\d{0,1})");
    }

    public boolean mergeDuplicateBracket(String text) {
        // Keep this comment
        return text.matches("\\d{4}[\\-\\d]{2}");
    }

    public boolean mergeDuplicateRegions(String text) {
        // Keep this comment
        return text.matches("\\d{4}(?:\\-\\d{2}){2}");
    }
}
