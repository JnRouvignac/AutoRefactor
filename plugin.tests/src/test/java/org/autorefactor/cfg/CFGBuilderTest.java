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
package org.autorefactor.cfg;

import static org.autorefactor.test.TestHelper.readAll;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.JavaProjectOptionsImpl;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.rules.JavaCoreHelper;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
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
                { "TryCatchThrowSample", 0 },
        });
    }

    @Test
    public void testCFGBuilder() throws Exception {
        final String sampleName = testName + ".java";
        final File javaFile = new File("src/test/java/org/autorefactor/cfg", sampleName);
        assertTrue(testName + ": sample in java file " + javaFile + " should exist", javaFile.exists());
        final File dotFile = new File("src/test/resources/org/autorefactor/cfg", testName + ".dot");
        assertTrue(testName + ": sample out dot file " + dotFile + " should exist", dotFile.exists());

        final String dotSource = readAll(dotFile).trim();
        final String javaSource = readAll(javaFile);

        final IPackageFragment packageFragment = JavaCoreHelper.getPackageFragment("org.autorefactor.cfg");
        final ICompilationUnit cu = packageFragment.createCompilationUnit(
                sampleName, javaSource, true, null);
        cu.getBuffer().setContents(javaSource);
        cu.save(null, true);

        final JavaProjectOptions options = newJavaProjectOptions(Release.javaSE("1.8"), 4);
        final ASTParser parser = ASTParser.newParser(AST.JLS8);
        autoRefactorHandlerResetParser(cu, parser, options);

        final CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);
        final CFGBuilder builder = new CFGBuilder(javaSource, options);
        final List<CFGBasicBlock> blocks = builder.buildCFG(astRoot);

        final CFGBasicBlock block = blocks.get(methodDeclarationNb);
        final String actual = new CFGDotPrinter().toDot(block).trim();
        final File dotFileOut = new File("src/test/resources/org/autorefactor/cfg", testName + "_out.dot");
        writeAll(dotFileOut, actual);
        assertEquals(testName + ": wrong output;", dotSource, actual);
    }

    private void writeAll(File file, String fileContent) throws Exception {
        FileOutputStream os = null;
        Writer writer = null;
        try {
            os = new FileOutputStream(file);
            writer = new BufferedWriter(new OutputStreamWriter(os));
            writer.append(fileContent);
        } finally {
            if (writer != null) {
                writer.close();
            }
            if (os != null) {
                os.close();
            }
        }
    }

    private JavaProjectOptions newJavaProjectOptions(Release javaSERelease, int tabSize) {
        JavaProjectOptionsImpl options = new JavaProjectOptionsImpl();
        options.setJavaSERelease(javaSERelease);
        options.setTabSize(tabSize);
        return options;
    }

    private void autoRefactorHandlerResetParser(ICompilationUnit cu, ASTParser parser,
            JavaProjectOptions options) throws Exception {
        final Method m = ApplyRefactoringsJob.class.getDeclaredMethod(
                "resetParser", ICompilationUnit.class, ASTParser.class, JavaProjectOptions.class);
        m.setAccessible(true);
        m.invoke(null, cu, parser, options);
    }

}
