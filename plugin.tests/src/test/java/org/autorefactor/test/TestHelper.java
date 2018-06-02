/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.test;

import static org.eclipse.jdt.core.JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM;
import static org.eclipse.jdt.core.JavaCore.COMPILER_COMPLIANCE;
import static org.eclipse.jdt.core.JavaCore.COMPILER_SOURCE;
import static org.eclipse.jdt.core.JavaCore.VERSION_1_7;
import static org.eclipse.jdt.core.ToolFactory.createCodeFormatter;
import static org.eclipse.jdt.core.formatter.CodeFormatter.K_COMPILATION_UNIT;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import org.autorefactor.environment.Environment;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.JavaProjectOptionsImpl;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.rules.EndsWithFileFilter;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.formatter.CodeFormatter;
import org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.TextEdit;

public final class TestHelper {

    /** Environment for unit tests. */
    public static final Environment TEST_ENVIRONMENT = new Environment(new CurrentThreadEvenLoop(),
                                                                       null,
                                                                       new ThrowingLogger(),
                                                                       null);

    private TestHelper() {
    }

    public static void runTest(Callable<Void> test) throws Exception {
        try {
            test.call();
        } catch (RuntimeException e) {
            if (e.getClass().getName().equals("org.autorefactor.util.UnhandledException")
                    || "Unexpected exception".equals(e.getMessage())) {
                throw (Exception) e.getCause();
            }
            throw e;
        }
    }

    public static String readAll(File file) throws IOException {
        final StringBuilder sb = new StringBuilder();
        FileInputStream input = null;
        Reader reader = null;
        try {
            input = new FileInputStream(file);
            reader = new InputStreamReader(input, "UTF-8");
            int c = 0;
            while ((c = reader.read()) != -1) {
                sb.append((char) c);
            }
        } finally {
            if (input != null) {
                input.close();
            }
            if (reader != null) {
                reader.close();
            }
        }
        return sb.toString();
    }

    public static JavaProjectOptions newJavaProjectOptions(Release javaSE, int tabSize) {
        final JavaProjectOptionsImpl options = new JavaProjectOptionsImpl();
        options.setTabSize(tabSize);
        options.setJavaSERelease(javaSE);
        return options;
    }

    public static String normalizeJavaSourceCode(String source) {
        final CodeFormatter codeFormatter = createCodeFormatter(getJava7Options());

        final TextEdit edit = codeFormatter.format(K_COMPILATION_UNIT,
            source, 0, source.length(), // source to format
            0, System.getProperty("line.separator") // initial indentation and line separator
        );

        try {
            final IDocument document = new Document(source);
            edit.apply(document);
            return document.get();
        } catch (MalformedTreeException e) {
            throw new RuntimeException(e);
        } catch (BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    private static Map<String, String> getJava7Options() {
        Map<String, String> options = DefaultCodeFormatterConstants.getEclipseDefaultSettings();
        options.put(COMPILER_COMPLIANCE, VERSION_1_7);
        options.put(COMPILER_CODEGEN_TARGET_PLATFORM, JavaCore.VERSION_1_7);
        options.put(COMPILER_SOURCE, VERSION_1_7);
        return options;
    }

    public static Collection<Object[]> samples(String samplesBaseDir, Collection<Class<?>> whitelistRules,
            Collection<Class<?>> blacklistRules) {
        final File samplesDir = new File(samplesBaseDir, "samples_in");
        final File[] sampleFiles = samplesDir.listFiles(new EndsWithFileFilter("Sample.java"));
        Arrays.sort(sampleFiles);

        final Collection<String> whitelist = toSampleNames(whitelistRules);
        final Collection<String> blacklist = toSampleNames(blacklistRules);

        final List<Object[]> output = new ArrayList<Object[]>(sampleFiles.length);
        for (File file : sampleFiles) {
            final String fileName = file.getName();

            if (!whitelist.isEmpty()
                    ? whitelist.contains(fileName)
                    : !blacklist.contains(fileName)) {
                output.add(new Object[] { fileName });
            }
        }
        return output;
    }

    private static Collection<String> toSampleNames(Collection<Class<?>> clazzes) {
        final Collection<String> results = new ArrayList<String>();
        for (Class<?> clazz : clazzes) {
            final String name = clazz.getSimpleName();
            results.add(name.substring(0, name.lastIndexOf("Refactoring")) + "Sample.java");
        }
        return results;
    }
}
