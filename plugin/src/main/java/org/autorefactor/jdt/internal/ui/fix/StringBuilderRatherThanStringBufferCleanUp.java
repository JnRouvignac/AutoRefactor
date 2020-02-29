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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class StringBuilderRatherThanStringBufferCleanUp extends AbstractClassSubstituteCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderRatherThanStringBufferCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderRatherThanStringBufferCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderRatherThanStringBufferCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 5;
    }

    @Override
    protected boolean canBeSharedInOtherThread() {
        return false;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] { StringBuffer.class.getCanonicalName() };
    }

    @Override
    protected String getSubstitutingClassName(final String origRawType) {
        return StringBuilder.class.getSimpleName();
    }

    @Override
    protected boolean isMethodReturningExistingClass(final MethodInvocation mi) {
        return ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", boolean.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", char.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", "char[]") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", "char[]", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", CharSequence.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", CharSequence.class.getCanonicalName(), int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", double.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", float.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", String.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "append", StringBuffer.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "appendCodePoint", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "delete", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "deleteCharAt", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), boolean.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), char.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), "char[]") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), "char[]", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), CharSequence.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), CharSequence.class.getCanonicalName(), int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), double.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), float.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), long.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "insert", int.class.getSimpleName(), String.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "replace", int.class.getSimpleName(), int.class.getSimpleName(), String.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "reverse") //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "ensureCapacity", int.class.getSimpleName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(mi, StringBuffer.class.getCanonicalName(), "getChars", int.class.getSimpleName(), int.class.getSimpleName(), "char[]", int.class.getSimpleName()); //$NON-NLS-1$ //$NON-NLS-2$
    }
}
