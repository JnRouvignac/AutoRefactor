package org.autorefactor.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.Callable;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.Release;
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
}
