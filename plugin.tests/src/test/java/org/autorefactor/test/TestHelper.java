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
package org.autorefactor.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.rules.EndsWithFileFilter;
import org.autorefactor.ui.JavaProjectOptionsImpl;

public final class TestHelper {

    private TestHelper() {
    }

    public static void runTest(Callable<Void> test) throws Exception {
        AutoRefactorPlugin.turnDebugModeOn();
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
        // FIXME Java 7 version of this method:
        // return new String(Files.readAllBytes(file.toPath()), "UTF8");
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(file);
            final InputStreamReader reader = new InputStreamReader(fis);
            final StringBuilder sb = new StringBuilder();
            final char[] buf = new char[4096];
            int nbRead;
            while ((nbRead = reader.read(buf)) != -1) {
                sb.append(buf, 0, nbRead);
            }
            return sb.toString();
        } finally {
            if (fis != null) {
                fis.close();
            }
        }
    }
    
    public static JavaProjectOptions newJavaProjectOptions(Release javaSE, int tabSize) {
        final JavaProjectOptionsImpl options = new JavaProjectOptionsImpl();
        options.setTabSize(tabSize);
        options.setJavaSERelease(javaSE);
        return options;
    }

    public static String normalizeJavaSourceCode(String s) {
        return s.replaceAll("\t", "    ")
                .replaceAll("(\r\n|\r|\n)", "\n")
                .trim();
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
