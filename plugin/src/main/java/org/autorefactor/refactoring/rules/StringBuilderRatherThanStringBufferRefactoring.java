/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017 Jean-Noël Rouvignac - minor changes
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

import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class StringBuilderRatherThanStringBufferRefactoring extends AbstractClassSubstituteRefactoring {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "StringBuilder rather than StringBuffer";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace StringBuffer by StringBuilder when possible.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the time performance.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 5;
    }

    @Override
    protected boolean canBeSharedInOtherThread() {
        return false;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] {"java.lang.StringBuffer"};
    }

    @Override
    protected String getSubstitutingClassName(String origRawType) {
        return "StringBuilder";
    }

    @Override
    protected boolean isMethodReturningExistingClass(final MethodInvocation mi) {
        return isMethod(mi, "java.lang.StringBuffer", "append", "boolean")
                || isMethod(mi, "java.lang.StringBuffer", "append", "char")
                || isMethod(mi, "java.lang.StringBuffer", "append", "char[]")
                || isMethod(mi, "java.lang.StringBuffer", "append", "char[]", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.CharSequence")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.CharSequence", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "append", "double")
                || isMethod(mi, "java.lang.StringBuffer", "append", "float")
                || isMethod(mi, "java.lang.StringBuffer", "append", "int")
                || isMethod(mi, "java.lang.StringBuffer", "append", "long")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.Object")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.String")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.StringBuffer")
                || isMethod(mi, "java.lang.StringBuffer", "appendCodePoint", "int")
                || isMethod(mi, "java.lang.StringBuffer", "delete", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "deleteCharAt", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "boolean")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "char")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "char[]")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "char[]", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.CharSequence")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.CharSequence", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "double")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "float")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "long")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.Object")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.String")
                || isMethod(mi, "java.lang.StringBuffer", "replace", "int", "int", "java.lang.String")
                || isMethod(mi, "java.lang.StringBuffer", "reverse")
                || isMethod(mi, "java.lang.StringBuffer", "ensureCapacity", "int")
                || isMethod(mi, "java.lang.StringBuffer", "getChars", "int", "int", "char[]", "int");
    }
}
