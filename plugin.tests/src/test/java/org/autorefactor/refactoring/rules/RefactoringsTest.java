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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.cfg.test.TestUtils.*;
import static org.junit.Assert.*;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;

import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Release;
import org.autorefactor.ui.AutoRefactorHandler;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(value = Parameterized.class)
public class RefactoringsTest {

	private String testName;

	public RefactoringsTest(String testName) {
		this.testName = testName;
	}

	@Parameters(name = "{0}Refactoring")
	public static Collection<Object[]> data() {
		return Arrays.asList(new Object[][] {
				{ "AddBracketsToControlStatement" },
				{ "BigDecimal" },
				{ "Boolean" },
				{ "CollapseIfStatement" },
				{ "Comments" },
				{ "CommonCodeInIfElseStatement" },
				{ "DeadCodeElimination" },
				{ "IfStatement" },
				{ "InvertEquals" },
				{ "PrimitiveWrapperCreation" },
				// { "ReduceVariableScope" }, // To be completed
				{ "RemoveUnnecessaryLocalBeforeReturn" },
				// { "RemoveUselessModifiers" },
				{ "SimplifyExpression" },
				{ "StringBuilder" },
				{ "String" },
				// { "VectorOldToNewAPI" },
		});
	}

	@Test
	public void testRefactoring() throws Exception {
		final String sampleName = testName + "Sample.java";
		final File samplesDir = new File("src/test/java/org/autorefactor");
		final File sampleIn = new File(samplesDir, "samples_in/" + sampleName);
		assertTrue(testName + ": sample in file " + sampleIn + " should exist", sampleIn.exists());
		final File sampleOut = new File(samplesDir, "samples_out/" + sampleName);
		assertTrue(testName + ": sample out file " + sampleOut + " should exist", sampleOut.exists());

		final String refactoringClassname = testName + "Refactoring";
		final AutoRefactorHandler handler = new AutoRefactorHandler();
		final IRefactoring refactoring = getRefactoringClass(refactoringClassname, handler);
		assertNotNull(testName + ": refactoring class " + refactoringClassname + " should exist", refactoring);

		final String sampleInSource = readAll(sampleIn);
		final String sampleOutSource = readAll(sampleOut);

		final IPackageFragment packageFragment = JavaCoreHelper.getPackageFragment();
		final ICompilationUnit cu = packageFragment.createCompilationUnit(
				sampleName, sampleInSource, true, null);
		cu.getBuffer().setContents(sampleInSource);
		cu.save(null, true);

		final IDocument doc = new Document(sampleInSource);
		autoRefactorHandler_ApplyRefactoring(
				doc, cu,
				Release.javaSE("1.5.0"), 4,
				new AggregateASTVisitor(refactoring));

		final String actual = normalize(
				doc.get().replaceAll("samples_in", "samples_out"));
		final String expected = normalize(sampleOutSource);
		assertEquals(testName + ": wrong output;", expected, actual);
	}

	private void autoRefactorHandler_ApplyRefactoring(Object... params) throws Exception {
		final Method m = AutoRefactorHandler.class.getDeclaredMethod("applyRefactoring",
				IDocument.class, ICompilationUnit.class,
				Release.class,
				Integer.TYPE,
				AggregateASTVisitor.class);
		m.setAccessible(true);
		m.invoke(null, params);
	}

	private IRefactoring getRefactoringClass(final String refactoringClassName,
			final AutoRefactorHandler handler) throws Exception {
		Collection<IRefactoring> refactorings = getAllRefactorings(handler);
		for (IRefactoring refactoring : refactorings) {
			if (refactoring.getClass().getSimpleName().equals(refactoringClassName)) {
				return refactoring;
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private <T> T getAllRefactorings(Object obj) throws Exception {
		final Method m = obj.getClass().getDeclaredMethod("getAllRefactorings");
		m.setAccessible(true);
		return (T) m.invoke(obj);
	}

	private String normalize(String s) {
		return s.replaceAll("\t", "    ")
				.replaceAll("(\r\n|\r|\n)", "\n")
				.trim();
	}
}
