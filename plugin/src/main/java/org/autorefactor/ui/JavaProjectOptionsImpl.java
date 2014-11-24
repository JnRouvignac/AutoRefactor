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
package org.autorefactor.ui;

import java.util.HashMap;
import java.util.Map;

import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.Release;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.JavaCore;

import static org.eclipse.jdt.core.JavaCore.*;
import static org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants.*;

/**
 * Implementation of {@link JavaProjectOptions} for Eclipse JDT.
 */
public class JavaProjectOptionsImpl implements JavaProjectOptions {

    private final Map<String, String> options;
    private Release javaSERelease;
    private int tabSize;

    /** Builds a new instance of this class. */
    public JavaProjectOptionsImpl() {
        this.options = new HashMap<String, String>();
    }

    /**
     * Builds a new instance of this class with the supplied options.
     *
     * @param options the java project options
     */
    public JavaProjectOptionsImpl(Map<String, String> options) {
        this.options = options;
        this.javaSERelease = Release.javaSE(options.get(COMPILER_SOURCE));
        this.tabSize = computeTabSize();
    }

    private int computeTabSize() {
        String tabSize = options.get(FORMATTER_INDENTATION_SIZE);
        try {
            return Integer.valueOf(tabSize);
        } catch (NumberFormatException e) {
            throw new UnhandledException(null, e);
        }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    @Override
    public Map<String, String> getCompilerOptions() {
        final Map<String, String> options = JavaCore.getOptions();
        final String v = javaSERelease.getMajorVersion() + "." + javaSERelease.getMinorVersion();
        JavaCore.setComplianceOptions(v, options);
        return options;
    }

    /** {@inheritDoc} */
    @Override
    public Release getJavaSERelease() {
        return javaSERelease;
    }

    /** {@inheritDoc} */
    @Override
    public int getTabSize() {
        return tabSize;
    }
    /**
     * Sets the Java SE release.
     *
     * @param javaSERelease the Java SE release
     */
    public void setJavaSERelease(Release javaSERelease) {
        this.javaSERelease = javaSERelease;
    }

    /**
     * Sets the tabulation size.
     *
     * @param tabSize the tabulation Size to set
     */
    public void setTabSize(int tabSize) {
        this.tabSize = tabSize;
    }


}
