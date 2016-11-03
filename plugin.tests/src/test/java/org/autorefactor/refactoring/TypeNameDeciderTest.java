/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.NavigableSet;
import java.util.TreeSet;

import org.eclipse.jdt.core.dom.ITypeBinding;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import static org.junit.Assert.*;

@SuppressWarnings("javadoc")
@RunWith(value = Parameterized.class)
public class TypeNameDeciderTest {

    private String qualifiedName;
    private NavigableSet<String> imports;
    private String expectedResult;

    public TypeNameDeciderTest(
            String qualifiedName, NavigableSet<String> imports, String expectedResult) {
        this.qualifiedName = qualifiedName;
        this.imports = imports;
        this.expectedResult = expectedResult;
    }

    @Parameters// (name = "{0} + imports{1} => {2}")
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {
                new Object[] { "java.util.Map", imports(), "java.util.Map" },
                new Object[] { "java.util.Map", imports("java.util.Map"), "Map" },
                new Object[] { "java.util.Map", imports("java.util.*"), "Map" },
                new Object[] { "java.util.Map.Entry", imports(), "java.util.Map.Entry" },
                new Object[] { "java.util.Map.Entry", imports("java.util.Map"), "Map.Entry" },
                new Object[] { "java.util.Map.Entry", imports("java.util.*"), "Map.Entry" },
                new Object[] { "java.util.Map.Entry", imports("java.util.Map.Entry"), "Entry" },
                new Object[] { "java.util.Map.Entry", imports("java.util.Map.*"), "Entry" },
                new Object[] { "java.util.concurrent.Callable", imports("java.util.*"), "java.util.concurrent.Callable" },
        });
    }

    @Test
    public void testUseSimplestPossibleName() throws Exception {
        assertEquals(expectedResult, useSimplestPossibleName(qualifiedName));
    }

    private String useSimplestPossibleName(String qualifiedName) {
        return new TypeNameDecider(new TypeNameDecider.ResolveTypeBindingStrategy() {
            @Override
            public ITypeBinding resolveTypeBinding(String fullyQualifiedName) {
                return new TypeBindingStub(fullyQualifiedName);
            }
        }, imports).useSimplestPossibleName(qualifiedName);
    }

    private static NavigableSet<String> imports(String... imports) {
        final TreeSet<String> results = new TreeSet<String>();
        Collections.addAll(results, imports);
        return results;
    }
}
