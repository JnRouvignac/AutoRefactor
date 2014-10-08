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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Release;
import org.autorefactor.ui.ApplyRefactoringsJob;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import static org.autorefactor.cfg.test.TestUtils.*;
import static org.junit.Assert.*;

/**
 * Tests each refactoring rule in isolation. Each refactoring rule is run in a
 * loop until it cannot apply any more changes to the sample file.
 */
@RunWith(value = Parameterized.class)
public class RefactoringsTest {

    private final String testName;

    public RefactoringsTest(String testName) {
        this.testName = testName;
    }

    @Parameters(name = "{0}Refactoring")
    public static Collection<Object[]> data() {
        final File samplesDir = new File("src/test/java/org/autorefactor/samples_in");
        final File[] sampleFiles = samplesDir.listFiles(new EndsWithFileFilter("Sample.java"));
        Arrays.sort(sampleFiles);

        final List<Object[]> output = new ArrayList<Object[]>(sampleFiles.length);
        for (File file : sampleFiles) {
            final String fileName = file.getName();
            if (!"ReduceVariableScopeSample.java".equals(fileName)) { // To be completed
                output.add(new Object[] { fileName.replace("Sample.java", "") });
            }
        }
        return output;
    }

    @Test
    public void testRefactoring() throws Exception {
        AutoRefactorPlugin.turnDebugModeOn();
        try {
            testRefactoring0();
        } catch (RuntimeException e) {
            if ("Unexpected exception".equals(e.getMessage())) {
                throw (Exception) e.getCause();
            }
            throw e;
        }
    }

    private void testRefactoring0() throws Exception {
        final String sampleName = testName + "Sample.java";
        final File samplesDir = new File("src/test/java/org/autorefactor");
        final File sampleIn = new File(samplesDir, "samples_in/" + sampleName);
        assertTrue(testName + ": sample in file " + sampleIn + " should exist", sampleIn.exists());
        final File sampleOut = new File(samplesDir, "samples_out/" + sampleName);
        assertTrue(testName + ": sample out file " + sampleOut + " should exist", sampleOut.exists());

        final String refactoringClassname = testName + "Refactoring";
        final IRefactoring refactoring = getRefactoringClass(refactoringClassname);
        assertNotNull(testName + ": refactoring class " + refactoringClassname + " should exist", refactoring);

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
                Release.javaSE("1.5.0"), 4,
                new AggregateASTVisitor(Arrays.asList(refactoring)));

        final String actual = normalize(
                doc.get().replaceAll("samples_in", "samples_out"));
        final String expected = normalize(sampleOutSource);
        assertEquals(testName + ": wrong output;", expected, actual);
    }

    private IRefactoring getRefactoringClass(final String refactoringClassName) throws Exception {
        Collection<IRefactoring> refactorings = AllRefactorings.getAllRefactorings();
        for (IRefactoring refactoring : refactorings) {
            if (refactoring.getClass().getSimpleName().equals(refactoringClassName)) {
                return refactoring;
            }
        }
        return null;
    }

    private String normalize(String s) {
        return s.replaceAll("\t", "    ")
                .replaceAll("(\r\n|\r|\n)", "\n")
                .trim();
    }
}
