/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.HashMap;
import java.util.Map;

import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants;

/** Implementation of {@link JavaProjectOptions} for Eclipse JDT. */
public class JavaProjectOptionsImpl implements JavaProjectOptions {
    private final Map<String, String> options;
    private Release javaSERelease;

    /** Builds a new instance of this class. */
    public JavaProjectOptionsImpl() {
        this.options= new HashMap<>();
    }

    /**
     * Builds a new instance of this class with the supplied options.
     *
     * @param options the java project options
     */
    public JavaProjectOptionsImpl(final Map<String, String> options) {
        this.options= options;
        this.javaSERelease= Release.javaSE(options.get(JavaCore.COMPILER_SOURCE));
    }

    private Integer asInteger(final String preference) {
        try {
            String value= options.get(preference);
            if (value != null) {
                return Integer.parseInt(value);
            }

            return null;
        } catch (NumberFormatException e) {
            throw new UnhandledException(null, e);
        }
    }

    /**
     * Get the compiler options.
     *
     * @return the compiler options.
     */
    public Map<String, String> getCompilerOptions() {
        Map<String, String> options= JavaCore.getOptions();
        String v= javaSERelease.getMajorVersion() + "." + javaSERelease.getMinorVersion(); //$NON-NLS-1$
        JavaCore.setComplianceOptions(v, options);
        return options;
    }

    /**
     * Get the JavaSE Release.
     *
     * @return the JavaSE Release.
     */
    public Release getJavaSERelease() {
        return javaSERelease;
    }

    /**
     * Get the tab size.
     *
     * @return the tab size.
     */
    public Integer getTabSize() {
        return asInteger(DefaultCodeFormatterConstants.FORMATTER_INDENTATION_SIZE);
    }

    /**
     * Get the comment line length.
     *
     * @return the comment line length.
     */
    public int getCommentLineLength() {
        Integer result= asInteger(DefaultCodeFormatterConstants.FORMATTER_COMMENT_LINE_LENGTH);
        if (result == null) {
            result= asInteger(DefaultCodeFormatterConstants.FORMATTER_LINE_SPLIT);
        }

        return result != null ? result : 80;
    }

    /**
     * Sets the Java SE release.
     *
     * @param javaSERelease the Java SE release
     */
    public void setJavaSERelease(final Release javaSERelease) {
        this.javaSERelease= javaSERelease;
    }

    /**
     * Sets the tabulation size.
     *
     * @param tabSize the tabulation Size to set
     */
    public void setTabSize(final int tabSize) {
        options.put(DefaultCodeFormatterConstants.FORMATTER_INDENTATION_SIZE, String.valueOf(tabSize));
    }
}
