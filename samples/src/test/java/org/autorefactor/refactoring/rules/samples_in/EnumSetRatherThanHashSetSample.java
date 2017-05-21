/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;

public class EnumSetRatherThanHashSetSample {

    enum Example {
        ONE, TWO, THREE
    }

    public Set<Example> refactorVariableDeclarationStatement() {
        Set<Example> set = new HashSet<Example>();
        return set;
    }

    public Set<Example> refactorVariableDeclarationStatementWithDiamond() {
        Set<Example> set = new HashSet<>();
        return set;
    }

    public Set<Example> refactorReturnStatement() {
        return new HashSet<Example>();
    }

    public Set<Example> refactorReturnStatement2() {
        return new HashSet<>();
    }

    public Set<Example> refactorVariableDeclarationStatementWithParentheses() {
        Set<Example> set = ((new HashSet<Example>()));
        return set;
    }

    public Set<Example> refactorAssignment() {
        Set<Example> set;
        set = new HashSet<Example>();
        return set;
    }

    public Set<Example> refactorConditionalAssignment() {
        Set<Example> set;
        set = true ? new HashSet<Example>() : new HashSet<Example>();
        return set;
    }

    public Set<Example> refactorConstrutorWithInt() {
        Set<Example> set = new HashSet<Example>(1);
        return set;
    }

    public Set<Example> refactorConstrutorWithIntAndFloat() {
        Set<Example> set = new HashSet<Example>(1, 0.75F);
        return set;
    }

    public Set<Example> doNotRefactorConstrutorWithCollection(
            Collection<Example> col) {
        Set<Example> set = new HashSet<Example>(col);
        return set;
    }

    public Set<Example> refactorConstrutorWithEnumSet(EnumSet<Example> s) {
        Set<Example> set = new HashSet<Example>(s);
        return set;
    }

    public void doNotRefactorMethodArgument() {
        Set<Example> set = Collections
                .synchronizedSet(new HashSet<Example>());
        System.out.println(set);
    }

    public Set<Example> doNotRefactor() {
        Set<Example> set = EnumSet.noneOf(Example.class);
        set.addAll(EnumSet.allOf(Example.class));
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
