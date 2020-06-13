/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix;

import static org.autorefactor.test.TestHelper.TEST_ENVIRONMENT;
import static org.autorefactor.test.TestHelper.newJavaProjectOptions;
import static org.autorefactor.test.TestHelper.normalizeJavaSourceCode;
import static org.autorefactor.test.TestHelper.readAll;
import static org.autorefactor.test.TestHelper.runTest;
import static org.autorefactor.test.TestHelper.samples;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.concurrent.Callable;

import org.autorefactor.jdt.internal.corext.dom.ApplyRefactoringsJob;
import org.autorefactor.jdt.internal.corext.dom.RefactoringRule;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Tests each refactoring rule in isolation. Each cleanup rule is run in a
 * loop until it cannot apply any more changes to the sample file.
 */
@RunWith(value= Parameterized.class)
public class RefactoringRulesTest {
	private static final String SAMPLES_BASE_DIR= "../samples/src/test/java/org/autorefactor/jdt/internal/ui/fix/"; //$NON-NLS-1$
	private static final String PACKAGE_NAME= "org.autorefactor.jdt.internal.ui.fix.samples_in"; //$NON-NLS-1$

	/** If not empty, then only run the refactorings present in this collection. */
	private static final Collection<Class<?>> WHITELIST= Arrays.<Class<?>>asList(/**/);
	/**
	 * When {@link #WHITELIST} is empty, the cleanups present in this collection
	 * will never be run.
	 */
	private static final Collection<Class<?>> BLACKLIST= Arrays.asList(ReduceVariableScopeCleanUp.class);

	private final String testName;

	public RefactoringRulesTest(String testName) {
		this.testName= testName;
	}

	@Parameters(name= "{0}Refactoring")
	public static Collection<Object[]> data() {
		Collection<Object[]> samples= samples(SAMPLES_BASE_DIR, WHITELIST, BLACKLIST);

		for (Object[] sample : samples) {
			sample[0]= ((String) sample[0]).replace("Sample.java", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return samples;
	}

	@Test
	public void testRefactoring() throws Exception {
		try {
			runTest(new Callable<Void>() {
				/**
				 * Call.
				 *
				 * @return the void.
				 */
				public Void call() throws Exception {
					testRefactoring0();
					return null;
				}
			});
		} catch (Exception e) {
			// Log stacktrace here because otherwise useful details are missing.
			System.err.println("executing " + testName + " failed"); //$NON-NLS-1$ //$NON-NLS-2$
			e.printStackTrace(System.err);
			throw e;
		}
	}

	private void testRefactoring0() throws Exception {
		String sampleName= testName + "Sample.java"; //$NON-NLS-1$

		File sampleIn= new File(SAMPLES_BASE_DIR, "samples_in/" + sampleName); //$NON-NLS-1$
		assertTrue(testName + ": sample in file " + sampleIn + " should exist", sampleIn.exists()); //$NON-NLS-1$ //$NON-NLS-2$

		File sampleOut= new File(SAMPLES_BASE_DIR, "samples_out/" + sampleName); //$NON-NLS-1$
		assertTrue(testName + ": sample out file " + sampleOut + " should exist", sampleOut.exists()); //$NON-NLS-1$ //$NON-NLS-2$

		String refactoringClassname= testName + "CleanUp"; //$NON-NLS-1$
		RefactoringRule refactoring= getRefactoringClass(refactoringClassname);
		assertNotNull(testName + ": refactoring class " + refactoringClassname + " should exist.\n" //$NON-NLS-1$ //$NON-NLS-2$
				+ "Make sure you added it to the method getAllCleanUpRules() " + "of the " + AllCleanUpRules.class //$NON-NLS-1$ //$NON-NLS-2$
				+ ".", refactoring); //$NON-NLS-1$

		String sampleInSource= readAll(sampleIn);
		String sampleOutSource= readAll(sampleOut);

		given(sampleInSource, sampleOutSource);

		IDocument doc= when(sampleName, refactoring, sampleInSource);

		then(sampleOutSource, doc);
	}

	private void given(final String sampleInSource, final String sampleOutSource) {
		String actual= normalizeJavaSourceCode(sampleInSource.replaceAll("samples_in", "samples_out")); //$NON-NLS-1$ //$NON-NLS-2$
		String expected= normalizeJavaSourceCode(sampleOutSource);
		assertNotEquals(testName + ": verify nothing;", expected, actual); //$NON-NLS-1$
	}

	private IDocument when(final String sampleName, final RefactoringRule refactoring, final String sampleInSource)
			throws Exception {
		IPackageFragment packageFragment= JavaCoreHelper.getPackageFragment(PACKAGE_NAME);
		ICompilationUnit cu= packageFragment.createCompilationUnit(sampleName, sampleInSource, true, null);
		cu.getBuffer().setContents(sampleInSource);
		cu.save(null, true);

		IDocument doc= new Document(sampleInSource);
		new ApplyRefactoringsJob(null, null, TEST_ENVIRONMENT).applyRefactoring(doc, cu,
				new AggregateASTVisitor(Arrays.asList(refactoring)), newJavaProjectOptions(Release.javaSE("1.8.0"), 4), //$NON-NLS-1$
				SubMonitor.convert(new NullProgressMonitor()), true);
		return doc;
	}

	private void then(final String sampleOutSource, final IDocument doc) {
		String actual= normalizeJavaSourceCode(doc.get().replaceAll("samples_in", "samples_out")); //$NON-NLS-1$ //$NON-NLS-2$
		String expected= normalizeJavaSourceCode(sampleOutSource);
		assertEquals(testName + ": wrong output;", expected, actual); //$NON-NLS-1$
	}

	private RefactoringRule getRefactoringClass(final String refactoringClassName) {
		Collection<RefactoringRule> refactorings= AllCleanUpRules.getAllCleanUpRules();

		for (RefactoringRule refactoring : refactorings) {
			if (refactoring.getClass().getSimpleName().equals(refactoringClassName)) {
				return refactoring;
			}
		}

		return null;
	}
}
