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
import java.io.FilenameFilter;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.junit.Test;

public class CFGBuilderTest {

	private static final class TestCase {

		private String testName;
		private File dotFile;
		private File javaFile;

		public TestCase(String testName) {
			this.testName = testName;
		}

		@Override
		public String toString() {
			return getClass().getSimpleName() + " testName=" + testName
					+ ", dotFile=" + dotFile + ", javaFile=" + javaFile;
		}
	}

	private static final class FileExtensionFilter implements FilenameFilter {

		private String extension;

		public FileExtensionFilter(String extension) {
			this.extension = extension;
		}

		@Override
		public boolean accept(File dir, String name) {
			return name.endsWith(extension);
		}
	}

	public Collection<TestCase> collectTestCases() throws Exception {
		final Map<String, TestCase> testCases = new HashMap<String, TestCase>();
		final File f = new File("src/test/java/");
		final File cfgDir = new File(f, "org/autorefactor/cfg");
		final File[] dotFiles = cfgDir.listFiles(new FileExtensionFilter(".dot"));
		for (File dotFile : dotFiles) {
			final String key = dotFile.getName().replace(".dot", "");
			final TestCase tc = new TestCase(key);
			tc.dotFile = dotFile;
			testCases.put(key, tc);
		}

		final File javaDir = new File("src/test/java/org/autorefactor/cfg");
		final File[] javaFiles = javaDir.listFiles(new FileExtensionFilter(".java"));
		for (File javaFile : javaFiles) {
			final String key = javaFile.getName().replace(".java", "");
			final TestCase tc = testCases.get(key);
			if (tc != null) {
				tc.javaFile = javaFile;
			}
		}

		return testCases.values();
	}

	@Test
	public void testProut() throws Exception {
		for (TestCase tc : collectTestCases()) {
			System.out.println("    +-- " + tc.testName + ".java");
			final String javaSource = readAll(tc.javaFile);
			String dotSource = readAll(tc.dotFile);

			final ASTParser parser = ASTParser.newParser(AST.JLS4);
			parser.setSource(javaSource.toCharArray());
			parser.setResolveBindings(true);

			final CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);
			final CFGBuilder builder = new CFGBuilder(javaSource, 4);
			List<CFGBasicBlock> blocks = builder.buildCFG(astRoot);

			dotSource = dotSource.replaceAll(tc.testName, "FakeClass").trim();
			String actual = new CFGDotPrinter().toDot(blocks.get(0)).trim();
			assertEquals(dotSource, actual);
		}
	}

}
