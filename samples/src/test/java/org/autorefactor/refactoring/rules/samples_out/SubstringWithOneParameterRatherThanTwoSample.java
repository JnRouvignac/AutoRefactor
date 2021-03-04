/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice TIERCELIN - initial API and implementation
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

public class SubstringWithOneParameterRatherThanTwoSample {
    private String textInInstance = "foo";

    public String reduceSubstring(String text) {
        // Keep this comment
        return text.substring(2);
    }

    public String reduceSubstringOnField() {
        // Keep this comment
        return textInInstance.substring(3);
    }

    public String reduceSubstringOnExpression(String text) {
        // Keep this comment
        return (textInInstance + text).substring(4);
    }

    public String doNotReduceSubstringOnOtherExpression(String text) {
        return text.substring(5, text.hashCode());
    }

    public String doNotReduceSubstringOnConstant(String text) {
        return text.substring(6, 123);
    }

    public String doNotReduceSubstringOnDifferentVariable(String text1, String text2) {
        return text1.substring(7, text2.length());
    }

    public String doNotReduceSubstringOnActiveExpression(List<String> texts) {
        return texts.remove(0).substring(7, texts.remove(0).length());
    }
}