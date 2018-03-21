/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - Initial API and implementation
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
import java.util.Date;
import java.util.Vector;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class LambdaSample extends Date {

    public Function<String, String> removeParentheses() {
        return someString -> someString.trim().toLowerCase();
    }

    public BiFunction<String, String, Integer> doNotRemoveParenthesesWithTwoParameters() {
        return (someString, anotherString) -> someString.trim().compareTo(anotherString.trim());
    }

    public Supplier<Boolean> doNotRemoveParenthesesWithNoParameter() {
        return () -> {System.out.println("foo");return true;};
    }

    public Function<String, String> removeReturnAndBrackets() {
        return someString -> someString.trim().toLowerCase();
    }

    public Function<String, String> removeReturnAndBracketsWithParentheses() {
        return someString -> (someString.trim().toLowerCase() + "bar");
    }

    public Function<String, String> doNotRemoveReturnWithSeveralStmts() {
        return someString -> {String trimmed = someString.trim();
        return trimmed.toLowerCase();};
    }

    public Supplier<ArrayList<String>> useCreationReference() {
        return ArrayList::new;
    }

    public Function<Integer, ArrayList<String>> useCreationReferenceWithParameter() {
        return ArrayList::new;
    }

    public Function<Integer, ArrayList<String>> doNotRefactorWithExpressions() {
        return capacity -> new ArrayList<>(capacity + 1);
    }

    public BiFunction<Integer, Integer, Vector<String>> useCreationReferenceWithParameters() {
        return Vector::new;
    }

    public BiFunction<Integer, Integer, Vector<String>> doNotRefactorShuffledParams() {
        return (initialCapacity, capacityIncrement) -> new Vector<>(capacityIncrement, initialCapacity);
    }

    public Function<Date, Long> useMethodReference() {
        return Date::getTime;
    }

    public BiFunction<Date, Date, Integer> useMethodReferenceWithParameter() {
        return Date::compareTo;
    }

    public Function<String, Long> useTypeReference() {
        return Long::getLong;
    }

    public Function<String, Integer> useExpressionMethodReference() {
        return "AutoRefactor"::indexOf;
    }

    public Function<Date, Integer> useSuperMethodReference() {
        return super::compareTo;
    }

    public Function<Date, Integer> useThisMethodReference() {
        return this::compareTo;
    }

    public Function<Integer, String> doNotUseConflictingMethodReference() {
        return numberToPrint -> numberToPrint.toString();
    }

    public Function<Integer, String> doNotUseConflictingStaticMethodReference() {
        return numberToPrint -> Integer.toString(numberToPrint);
    }
}
