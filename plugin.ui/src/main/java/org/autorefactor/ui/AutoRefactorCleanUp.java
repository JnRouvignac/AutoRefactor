/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.ui.cleanup.CleanUpContext;
import org.eclipse.jdt.ui.cleanup.CleanUpOptions;
import org.eclipse.jdt.ui.cleanup.CleanUpRequirements;
import org.eclipse.jdt.ui.cleanup.ICleanUp;
import org.eclipse.jdt.ui.cleanup.ICleanUpFix;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;

/** AutoRefactorCleanUp. */
public class AutoRefactorCleanUp implements ICleanUp {
    private static final String KEY = "org.autorefactor.ui.autorefactor_clean_up";

    private CleanUpOptions fOptions;

    private RefactoringStatus fStatus;

    /**
     * Create the fix.
     *
     * @param context context
     * @return Clean up fix
     * @throws CoreException CoreException
     */
    public ICleanUpFix createFix(CleanUpContext context) throws CoreException {
        CompilationUnit compilationUnit = context.getAST();
        if (compilationUnit != null && fOptions.isEnabled(KEY)) {
            return AutoRefactorFix.createCleanUp(compilationUnit, fOptions.isEnabled(KEY), fOptions);
        }

        return null;
    }

    /**
     * Get the requirements.
     *
     * @return Clean up requirements
     */
    public CleanUpRequirements getRequirements() {
        return new CleanUpRequirements(true, true, true, new HashMap<String, String>());
    }

    /**
     * Get the step descriptions.
     *
     * @return The step descriptions.
     */
    public String[] getStepDescriptions() {
        if (fOptions.isEnabled(KEY)) {
            return new String[] {"AutoRefactor (only 1 pass)"};
        } else {
            return new String[0];
        }
    }

    /**
     * Set the options.
     *
     * @param options options
     */
    public void setOptions(CleanUpOptions options) {
        Assert.isLegal(options != null);
        Assert.isTrue(fOptions == null);
        fOptions = options;
    }

    /**
     * Check the pre-conditions.
     *
     * @param project project
     * @param compilationUnits compilation units
     * @param monitor monitor
     * @return Refactoring status
     * @throws CoreException CoreException
     */
    public RefactoringStatus checkPreConditions(IJavaProject project, ICompilationUnit[] compilationUnits,
            IProgressMonitor monitor) throws CoreException {
        if (fOptions.isEnabled(KEY)) {
            fStatus = new RefactoringStatus();
        }
        return new RefactoringStatus();
    }

    /**
     * Check the post-conditions.
     *
     * @param monitor monitor
     * @return Refactoring Status
     * @throws CoreException CoreException
     */
    public RefactoringStatus checkPostConditions(IProgressMonitor monitor) throws CoreException {
        try {
            if (fStatus == null || fStatus.isOK()) {
                return new RefactoringStatus();
            } else {
                return fStatus;
            }
        } finally {
            fStatus = null;
        }
    }
}
