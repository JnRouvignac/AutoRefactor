/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.cfg;

import static org.autorefactor.cfg.test.TestUtils.*;
import static org.junit.Assert.*;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(value = Parameterized.class)
public class CFGBuilderTest {

	private String testName;
	private int methodDeclarationNb;

	public CFGBuilderTest(String testName, int methodDeclarationNb) {
		this.testName = testName;
		this.methodDeclarationNb = methodDeclarationNb;
	}

	@Override
	public String toString() {
		return getClass().getSimpleName()
				+ "[" + testName + ", methodNb=" + methodDeclarationNb + "]";
	}

	@Parameters(name = "{index}: {0}")
	public static Collection<Object[]> data() {
		return Arrays.asList(new Object[][] {
				{ "ForWithIfToEndLoopSample", 0 },
				{ "IfElseIfSample", 0 },
				{ "LabelsSample", 0 },
				{ "SwitchSample", 0 },
				{ "WhileLoopsSample", 2 },
		});
	}

	@Test
	public void testCFGBuilder() throws Exception {
		final File javaFile = new File("src/test/java/org/autorefactor/cfg", testName + ".java");
		assertTrue(testName + ": sample in java file " + javaFile + " should exist", javaFile.exists());
		final File dotFile = new File("src/test/resources/org/autorefactor/cfg", testName + ".dot");
		assertTrue(testName + ": sample out dot file " + dotFile + " should exist", dotFile.exists());

		final String dotSource = readAll(dotFile).replaceAll(testName, "FakeClass").trim();
		final String javaSource = readAll(javaFile);

		final ASTParser parser = ASTParser.newParser(AST.JLS4);
		parser.setSource(javaSource.toCharArray());
		parser.setResolveBindings(true);

		final CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);
		final CFGBuilder builder = new CFGBuilder(javaSource, 4);
		final List<CFGBasicBlock> blocks = builder.buildCFG(astRoot);

		final CFGBasicBlock block = blocks.get(methodDeclarationNb);
		final String actual = new CFGDotPrinter().toDot(block).trim();
		assertEquals(testName + ": wrong output;", dotSource, actual);
	}

}
