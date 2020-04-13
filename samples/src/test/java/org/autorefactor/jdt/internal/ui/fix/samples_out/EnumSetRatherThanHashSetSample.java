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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.time.DayOfWeek;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

public class EnumSetRatherThanHashSetSample {

    enum Example {
        ONE, TWO, THREE
    }

    public Set<DayOfWeek> refactorVariableDeclarationStatement() {
        // Keep this comment
        Set<DayOfWeek> set = EnumSet.noneOf(DayOfWeek.class);
        return set;
    }

    public Set<DayOfWeek> refactorVariableDeclarationStatementWithDiamond() {
        // Keep this comment
        Set<DayOfWeek> set = EnumSet.noneOf(DayOfWeek.class);
        return set;
    }

    public Set<DayOfWeek> doNotRefactorRawVariableDeclarationStatement() {
        Set set = new HashSet<DayOfWeek>();
        return set;
    }

    public Set<DayOfWeek> refactorReturnStatement() {
        // Keep this comment
        return EnumSet.noneOf(DayOfWeek.class);
    }

    public Set<Example> refactorReturnStatementWithDiamondOperator() {
        // Keep this comment
        return EnumSet.noneOf(Example.class);
    }

    public Set doNotRefactorRawReturnStatement() {
        return new HashSet<Example>();
    }

    public Set<Example> refactorVariableDeclarationStatementWithParentheses() {
        // Keep this comment
        Set<Example> set = ((EnumSet.noneOf(Example.class)));
        return set;
    }

    public Set<Example> refactorAssignment() {
        // Keep this comment
        Set<Example> set;
        set = EnumSet.noneOf(Example.class);
        return set;
    }

    public Set<Example> doNotRefactorRawAssignment() {
        Set set;
        set = new HashSet<Example>();
        return set;
    }

    public Set<Example> refactorConditionalAssignment() {
        // Keep this comment
        Set<Example> set;
        set = true ? EnumSet.noneOf(Example.class) : EnumSet
                                                            .noneOf(Example.class);
        return set;
    }

    public Set<Example> refactorConstrutorWithInt() {
        // Keep this comment
        Set<Example> set = EnumSet.noneOf(Example.class);
        return set;
    }

    public Set<Example> refactorConstrutorWithIntAndFloat() {
        // Keep this comment
        Set<Example> set = EnumSet.noneOf(Example.class);
        return set;
    }

    public Set<Example> doNotRefactorConstrutorWithCollection(
            Collection<Example> col) {
        Set<Example> set = new HashSet<Example>(col);
        return set;
    }

    public Set<Example> refactorConstrutorWithEnumSet(java.util.EnumSet<Example> s) {
        // Keep this comment
        Set<Example> set = EnumSet.copyOf(s);
        return set;
    }

    public void doNotRefactorMethodArgument() {
        Set<Example> set = Collections
                                      .synchronizedSet(new HashSet<Example>());
        System.out.println(set);
    }

    public Set<Example> doNotRefactor() {
        Set<Example> set = java.util.EnumSet.noneOf(Example.class);
        set.addAll(java.util.EnumSet.allOf(Example.class));
        return set;
    }

    public Set<Example> doNotRefactorAnonymousClass() {
        return new HashSet<Example>() {
            @Override
            public String toString() {
                return super.toString();
            }
        };
    }
}
