/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.Callable;

import org.autorefactor.refactoring.RefactoringRule;
import org.autorefactor.refactoring.Release;
import org.autorefactor.ui.ApplyRefactoringsJob;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import static org.autorefactor.test.TestHelper.*;
import static org.junit.Assert.*;

/**
 * Tests each refactoring rule in isolation. Each refactoring rule is run in a
 * loop until it cannot apply any more changes to the sample file.
 */
@RunWith(value = Parameterized.class)
public class RefactoringRulesTest {

    private static final String SAMPLES_BASE_DIR = "../samples/src/test/java/org/autorefactor/refactoring/rules/";

    /** If not empty, then only run the refactorings present in this collection. */
    private static final Collection<Class<?>> WHITELIST = Arrays.<Class<?>> asList(
    );
    /** When {@link #WHITELIST} is empty, the refactorings present in this collection will never be run. */
    private static final Collection<Class<?>> BLACKLIST = Arrays.<Class<?>> asList(
            ReduceVariableScopeRefactoring.class
    );

    private final String testName;

    public RefactoringRulesTest(String testName) {
        this.testName = testName;
    }

    @Parameters//(name = "{0}Refactoring") // requires junit 4.11
    public static Collection<Object[]> data() {
        Collection<Object[]> samples = samples(SAMPLES_BASE_DIR, WHITELIST, BLACKLIST);
        for (Object[] sample : samples) {
            sample[0] = ((String) sample[0]).replace("Sample.java", "");
        }
        return samples;
    }

    @Test
    public void testRefactoring() throws Exception {
        runTest(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                testRefactoring0();
                return null;
            }
        });
    }

    private void testRefactoring0() throws Exception {
        final String sampleName = testName + "Sample.java";
        final File sampleIn = new File(SAMPLES_BASE_DIR, "samples_in/" + sampleName);
        assertTrue(testName + ": sample in file " + sampleIn + " should exist", sampleIn.exists());
        final File sampleOut = new File(SAMPLES_BASE_DIR, "samples_out/" + sampleName);
        assertTrue(testName + ": sample out file " + sampleOut + " should exist", sampleOut.exists());

        final String refactoringClassname = testName + "Refactoring";
        final RefactoringRule refactoring = getRefactoringClass(refactoringClassname);
        assertNotNull(testName + ": refactoring class " + refactoringClassname + " should exist.\n"
                + "Make sure you added it to the method getAllRefactoringRules() "
                + "of the " + AllRefactoringRules.class + ".",
                refactoring);

        final String sampleInSource = readAll(sampleIn);
        final String sampleOutSource = readAll(sampleOut);

        final IPackageFragment packageFragment = JavaCoreHelper.getPackageFragment();
        final ICompilationUnit cu = packageFragment.createCompilationUnit(
                sampleName, sampleInSource, true, null);
        cu.getBuffer().setContents(sampleInSource);
        cu.save(null, true);

        final IDocument doc = new Document(sampleInSource);
        new ApplyRefactoringsJob(null, null).applyRefactoring(
                doc, cu,
                new AggregateASTVisitor(Arrays.asList(refactoring)),
                newJavaProjectOptions(Release.javaSE("1.7.0"), 4),
                new NullProgressMonitor());

        final String actual = normalizeJavaSourceCode(
                doc.get().replaceAll("samples_in", "samples_out"));
        final String expected = normalizeJavaSourceCode(sampleOutSource);
        assertEquals(testName + ": wrong output;", expected, actual);
    }

    private RefactoringRule getRefactoringClass(final String refactoringClassName) throws Exception {
        Collection<RefactoringRule> refactorings = AllRefactoringRules.getAllRefactoringRules();
        for (RefactoringRule refactoring : refactorings) {
            if (refactoring.getClass().getSimpleName().equals(refactoringClassName)) {
                return refactoring;
            }
        }
        return null;
    }
}
