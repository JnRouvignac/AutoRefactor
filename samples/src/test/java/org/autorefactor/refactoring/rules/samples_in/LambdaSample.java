/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - Initial API and implementation
 * Copyright (C) 2018 Jean-Noël Rouvignac - fix NPE
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

import static java.util.Calendar.getInstance;
import static java.util.Calendar.getAvailableLocales;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.Vector;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class LambdaSample extends Date {
    public String changeableText = "foo";

    public Function<String, String> removeParentheses() {
        return (someString) -> someString.trim().toLowerCase();
    }

    public Function<String, String> doNotRemoveParenthesesWithSingleVariableDeclaration() {
        return (String someString) -> someString.trim().toLowerCase();
    }

    public BiFunction<String, String, Integer> doNotRemoveParenthesesWithTwoParameters() {
        return (someString, anotherString) -> someString.trim().compareTo(anotherString.trim());
    }

    public Supplier<Boolean> doNotRemoveParenthesesWithNoParameter() {
        return () -> {System.out.println("foo");return true;};
    }

    public Function<String, String> removeReturnAndBrackets() {
        return someString -> {return someString.trim().toLowerCase();};
    }

    public Function<String, String> removeReturnAndBracketsWithParentheses() {
        return someString -> {return someString.trim().toLowerCase() + "bar";};
    }

    public Function<String, String> doNotRemoveReturnWithSeveralStmts() {
        return someString -> {String trimmed = someString.trim();
        return trimmed.toLowerCase();};
    }

    public Supplier<ArrayList<String>> useCreationReference() {
        return () -> new ArrayList<>();
    }

    public Function<Integer, ArrayList<String>> useCreationReferenceWithParameter() {
        return capacity -> new ArrayList<>(capacity);
    }

    public Function<Integer, ArrayList<String>> useCreationReferenceWithParameterAndType() {
        // TODO this can be refactored like useCreationReferenceWithParameter
        return (Integer capacity) -> new ArrayList<>(capacity);
    }

    public Function<Integer, ArrayList<String>> doNotRefactorWithExpressions() {
        return capacity -> new ArrayList<>(capacity + 1);
    }

    public BiFunction<Integer, Integer, Vector<String>> useCreationReferenceWithParameters() {
        return (initialCapacity, capacityIncrement) -> new Vector<>(initialCapacity, capacityIncrement);
    }

    public BiFunction<Integer, Integer, Vector<String>> doNotRefactorShuffledParams() {
        return (initialCapacity, capacityIncrement) -> new Vector<>(capacityIncrement, initialCapacity);
    }

    public Function<Date, Long> useMethodReference() {
        return date -> date.getTime();
    }

    public BiFunction<Date, Date, Integer> useMethodReferenceWithParameter() {
        return (date, anotherDate) -> date.compareTo(anotherDate);
    }

    public Function<String, Long> useTypeReference() {
        return numberInText -> Long.getLong(numberInText);
    }

    public static Function<Instant, Date> useTypeReferenceOnClassMethod() {
        return instant -> from(instant);
    }

    public static Function<Locale, Calendar> useTypeReferenceOnImportedMethod() {
        return locale -> getInstance(locale);
    }

    public static Supplier<Locale[]> useTypeReferenceAsSupplier() {
        return () -> getAvailableLocales();
    }

    public Function<String, Integer> useExpressionMethodReferenceOnLiteral() {
        return textToSearch -> "AutoRefactor".indexOf(textToSearch);
    }

    public Function<String, Integer> doNotUseExpressionMethodReferenceOnVariable() {
        return textToSearch -> this.changeableText.indexOf(textToSearch);
    }

    public Function<Date, Integer> useThisMethodReference() {
        return anotherDate -> this.compareTo(anotherDate);
    }

    public Function<Date, Integer> useThisMethodReferenceAddThis() {
        return anotherDate -> compareTo(anotherDate);
    }

    public Function<Date, Integer> useSuperMethodReference() {
        return anotherDate -> super.compareTo(anotherDate);
    }

    public Function<Integer, String> doNotUseConflictingMethodReference() {
        return numberToPrint -> numberToPrint.toString();
    }

    public Function<Integer, String> doNotUseConflictingStaticMethodReference() {
        return numberToPrint -> Integer.toString(numberToPrint);
    }
}
