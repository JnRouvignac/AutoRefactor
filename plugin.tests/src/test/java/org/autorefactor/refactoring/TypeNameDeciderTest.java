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

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import java.util.concurrent.Callable;

import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class TypeNameDeciderTest {
	private final String qualifiedName;
	private final TreeSet<String> imports;
	private final String expectedResult;

	public TypeNameDeciderTest(String qualifiedName, TreeSet<String> imports, String expectedResult) {
		this.qualifiedName= qualifiedName;
		this.imports= imports;
		this.expectedResult= expectedResult;
	}

	@Parameters // (name = "{0} + imports{1} => {2}")
	public static Collection<Object[]> data() {
		return Arrays.asList(new Object[][] { { Map.class.getCanonicalName(), imports(), Map.class.getCanonicalName() },
				{ Map.class.getCanonicalName(), imports(Map.class.getCanonicalName()), "Map" }, //$NON-NLS-1$
				{ Map.class.getCanonicalName(), imports("java.util.*"), "Map" }, //$NON-NLS-1$ //$NON-NLS-2$
				{ Entry.class.getCanonicalName(), imports(), Entry.class.getCanonicalName() },
				{ Entry.class.getCanonicalName(), imports(Map.class.getCanonicalName()), "Map.Entry" }, //$NON-NLS-1$
				{ Entry.class.getCanonicalName(), imports("java.util.*"), "Map.Entry" }, //$NON-NLS-1$ //$NON-NLS-2$
				{ Entry.class.getCanonicalName(), imports(Entry.class.getCanonicalName()), "Entry" }, //$NON-NLS-1$
				{ Entry.class.getCanonicalName(), imports("java.util.Map.*"), "Entry" }, //$NON-NLS-1$ //$NON-NLS-2$
				{ Callable.class.getCanonicalName(), imports("java.util.*"), Callable.class.getCanonicalName() }, }); //$NON-NLS-1$
	}

	@Test
	public void testUseSimplestPossibleName() throws Exception {
		assertEquals(expectedResult, useSimplestPossibleName(qualifiedName));
	}

	private String useSimplestPossibleName(String qualifiedName) {
		return new TypeNameDecider(TypeBindingStub::new, imports)
				.useSimplestPossibleName(qualifiedName);
	}

	private static TreeSet<String> imports(String... imports) {
		final TreeSet<String> results= new TreeSet<>();
		Collections.addAll(results, imports);
		return results;
	}
}
