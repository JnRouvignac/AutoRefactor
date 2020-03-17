/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.autorefactor.preferences.Preferences;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * Interface that all cleanup rules must implement.
 * <p>
 * First, {@link #setRefactoringContext(CompilationUnitRewrite)} is called, then
 * {@link #getRefactorings(CompilationUnit)} is called next.
 */
public interface RefactoringRule {
    /**
     * Returns the name of the cleanup rule suitable for displaying to the user.
     *
     * @return the name
     */
    String getName();

    /**
     * Returns a description of the cleanup rule suitable for displaying to the
     * user.
     *
     * @return the description
     */
    String getDescription();

    /**
     * Returns the motivation of the cleanup rule suitable for displaying to the
     * user.
     *
     * @return the motivation
     */
    String getReason();

    /**
     * Returns all the cleanups determined for the provided compilation unit
     * after analysis.
     *
     * @param astRoot the compilation unit to refactor
     * @return all the determined cleanups
     */
    ASTRewrite getRefactorings(CompilationUnit astRoot);

    /**
     * True if the cleanup is pre-configured.
     *
     * @return True if the cleanup is pre-configured.
     */
    boolean isByDefault();

    /**
     * Returns whether the current cleanup is enabled by the preferences.
     *
     * @param preferences the preferences
     * @return true if the current cleanup is enabled by the preferences, false
     *         otherwise.
     */
    boolean isEnabled(Preferences preferences);

    /**
     * Sets the cleanup context before analysis.
     *
     * @param cuRewrite the cleanup context
     */
    void setRefactoringContext(CompilationUnitRewrite cuRewrite);
}
