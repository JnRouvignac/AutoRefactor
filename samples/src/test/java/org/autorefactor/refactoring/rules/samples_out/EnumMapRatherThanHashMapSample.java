/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EnumMapRatherThanHashMapSample {

    enum Example {
        ONE, TWO, THREE
    }

    public Map<Example, String> refactorVariableDeclarationStatement() {
        // Keep this comment
        Map<Example, String> map = new java.util.EnumMap<Example, String>(Example.class);
        return map;
    }

    public Map<Example, String> refactorVariableDeclarationStatementWithDiamond() {
        // Keep this comment
        Map<Example, String> map = new java.util.EnumMap<>(Example.class);
        return map;
    }

    public Map<Example, String> refactorReturnStatement() {
        // Keep this comment
        return new java.util.EnumMap<Example, String>(Example.class);
    }

    public Map<Example, List<String>> refactorReturnStatementWithParameterizedType() {
        // Keep this comment
        return new java.util.EnumMap<Example, List<String>>(Example.class);
    }

    public Map<Example, String> refactorReturnStatementWithDiamondOperator() {
        // Keep this comment
        return new java.util.EnumMap<>(Example.class);
    }

    public Map<Example, String> refactorVariableDeclarationStatementWithParentheses() {
        // Keep this comment
        Map<Example, String> map = ((new java.util.EnumMap<Example, String>(Example.class)));
        return map;
    }

    public Map<Example, long[]> refactorAssignment() {
        // Keep this comment
        Map<Example, long[]> map;
        map = new java.util.EnumMap<>(Example.class);
        return map;
    }

    public Map<Example, String> refactorConditionalAssignment() {
        // Keep this comment
        Map<Example, String> map;
        map = true ? new java.util.EnumMap<Example, String>(Example.class)
                : new java.util.EnumMap<Example, String>(Example.class);
        return map;
    }

    public Map<Example, String> refactorConstrutorWithInt() {
        // Keep this comment
        Map<Example, String> map = new java.util.EnumMap<Example, String>(Example.class);
        return map;
    }

    public Map<Example, String> refactorConstrutorWithIntAndFloat() {
        // Keep this comment
        Map<Example, String> map = new java.util.EnumMap<Example, String>(Example.class);
        return map;
    }

    public Map<Example, String> doNotRefactorConstrutorWithMap(
            Map<Example, String> m) {
        Map<Example, String> map = new HashMap<Example, String>(m);
        return map;
    }

    public Map<Example, String> refactorConstrutorWithEnumMap(
            EnumMap<Example, String> m) {
        // Keep this comment
        Map<Example, String> map = new java.util.EnumMap<Example, String>(m);
        return map;
    }

    public void doNotRefactorMethodArgument() {
        Map<Example, String> map = Collections
                                              .synchronizedMap(new HashMap<Example, String>());
        System.out.println(map);
    }

    public Map<Example, String> doNotRefactor() {
        Map<Example, String> map = new EnumMap<>(Example.class);
        map.putAll(new EnumMap<Example, String>(Example.class));
        return map;
    }

    public Map<Example, String> doNotRefactorAnonymousClass() {
        return new HashMap<Example, String>() {
            @Override
            public String toString() {
                return super.toString();
            }
        };
    }

}