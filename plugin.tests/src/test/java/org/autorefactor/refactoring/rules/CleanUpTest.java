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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.test.TestHelper.TEST_ENVIRONMENT;
import static org.autorefactor.test.TestHelper.newJavaProjectOptions;
import static org.autorefactor.test.TestHelper.normalizeJavaSourceCode;
import static org.autorefactor.test.TestHelper.readAll;
import static org.autorefactor.test.TestHelper.runTest;
import static org.autorefactor.test.TestHelper.samples;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;

import org.autorefactor.jdt.internal.corext.dom.ApplyRefactoringsJob;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.ui.fix.AggregateASTVisitor;
import org.autorefactor.jdt.internal.ui.fix.AllCleanUpRules;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Tests all cleanup rules at the same time. This test verifies that all the
 * cleanup rules work together and do not introduce problems.
 */
@RunWith(Parameterized.class)
public class CleanUpTest {
	private static final String SAMPLES_ALL_BASE_DIR= "../samples/src/test/java/org/autorefactor/refactoring/rules/all"; //$NON-NLS-1$
	private static final String PACKAGE_NAME= "org.autorefactor.refactoring.rules.all.samples_in"; //$NON-NLS-1$

	/** If not empty, then only run the test samples present in this collection. */
	private static final Collection<Class<?>> WHITELIST= Arrays.<Class<?>>asList();
	/**
	 * When {@link #WHITELIST} is empty, the test samples present in this collection
	 * will never be run.
	 */
	private static final Collection<Class<?>> BLACKLIST= Arrays.<Class<?>>asList();

	private final String sampleName;

	public CleanUpTest(String testName) {
		this.sampleName= testName;
	}

	@Parameters(name= "{0}CleanUp")
	public static Collection<Object[]> data() {
		return samples(SAMPLES_ALL_BASE_DIR, WHITELIST, BLACKLIST);
	}

	@Test
	public void testRefactoring() throws Exception {
		runTest(() -> {
			testRefactoring0();
			return null;
		});
	}

	private void testRefactoring0() throws Exception {
		final File samplesDir= new File(SAMPLES_ALL_BASE_DIR);
		final File sampleIn= new File(samplesDir, "samples_in/" + sampleName); //$NON-NLS-1$
		assertTrue(sampleName + ": sample in file " + sampleIn + " should exist", sampleIn.exists()); //$NON-NLS-1$ //$NON-NLS-2$
		final File sampleOut= new File(samplesDir, "samples_out/" + sampleName); //$NON-NLS-1$
		assertTrue(sampleName + ": sample out file " + sampleOut + " should exist", sampleOut.exists()); //$NON-NLS-1$ //$NON-NLS-2$

		final String sampleInSource= readAll(sampleIn);
		final String sampleOutSource= readAll(sampleOut);

		given(sampleInSource, sampleOutSource);

		final IDocument doc= when(sampleInSource);

		then(sampleOutSource, doc);
	}

	private void given(final String sampleInSource, final String sampleOutSource) {
		final String actual= normalizeJavaSourceCode(sampleInSource.replace("samples_in", "samples_out")); //$NON-NLS-1$ //$NON-NLS-2$
		final String expected= normalizeJavaSourceCode(sampleOutSource);
		assertNotEquals(sampleName + ": verify nothing;", expected, actual); //$NON-NLS-1$
	}

	private IDocument when(final String sampleInSource) throws Exception, JavaModelException {
		final IPackageFragment packageFragment= JavaCoreHelper.getPackageFragment(PACKAGE_NAME);
		final ICompilationUnit cu= packageFragment.createCompilationUnit(sampleName, sampleInSource, true, null);
		cu.getBuffer().setContents(sampleInSource);
		cu.save(null, true);

		final IDocument doc= new Document(sampleInSource);
		new ApplyRefactoringsJob(null, null, TEST_ENVIRONMENT).applyRefactoring(doc, cu,
				new AggregateASTVisitor(AllCleanUpRules.getAllCleanUpRules()),
				newJavaProjectOptions(Release.javaSE("1.8.0"), 4), SubMonitor.convert(new NullProgressMonitor()), true); //$NON-NLS-1$
		return doc;
	}

	private void then(final String sampleOutSource, final IDocument doc) {
		final String actual= normalizeJavaSourceCode(doc.get().replace("samples_in", "samples_out")); //$NON-NLS-1$ //$NON-NLS-2$
		final String expected= normalizeJavaSourceCode(sampleOutSource);
		assertEquals(sampleName + ": wrong output;", expected, actual); //$NON-NLS-1$
	}
}
