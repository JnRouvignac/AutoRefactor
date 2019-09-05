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

import static org.autorefactor.AutoRefactorPlugin.getEnvironment;

import java.util.Iterator;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.environment.Environment;
import org.autorefactor.jdt.internal.corext.dom.ApplyRefactoringsJob;
import org.autorefactor.jdt.internal.corext.dom.JavaProjectOptions;
import org.autorefactor.jdt.internal.corext.dom.JavaProjectOptionsImpl;
import org.autorefactor.jdt.internal.corext.dom.PrepareApplyRefactoringsJob;
import org.autorefactor.jdt.internal.corext.dom.RefactoringRule;
import org.autorefactor.jdt.internal.corext.dom.RefactoringUnit;
import org.autorefactor.jdt.internal.ui.fix.AggregateASTVisitor;
import org.autorefactor.jdt.internal.ui.fix.AllCleanUpRules;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.internal.corext.fix.CleanUpRefactoring.CleanUpChange;
import org.eclipse.jdt.internal.corext.refactoring.util.TextEditUtil;
import org.eclipse.jdt.ui.cleanup.CleanUpOptions;
import org.eclipse.jdt.ui.cleanup.ICleanUpFix;
import org.eclipse.text.edits.TextEdit;

/** AutoRefactorFix. */
@SuppressWarnings("restriction")
public class AutoRefactorFix implements ICleanUpFix {
    private CleanUpChange cleanUpChange;

    /**
     * Create the clean up.
     *
     * @param compilationUnit compilation unit
     * @param enabled         enabled
     * @param fOptions        options
     * @return Clean up fix
     */
    public static ICleanUpFix createCleanUp(final CompilationUnit compilationUnit, final boolean enabled,
            final CleanUpOptions fOptions) {
        boolean hasChanges= false;
        final ICompilationUnit iCompilationUnit= (ICompilationUnit) compilationUnit.getJavaElement();
        final CleanUpChange cleanUpChange= new CleanUpChange("AutoRefactor", iCompilationUnit); //$NON-NLS-1$
        TextEdit allEdits= null;

        if (enabled) {
            final IJavaProject javaProject= PrepareApplyRefactoringsJob.getIJavaProject(iCompilationUnit);
            final JavaProjectOptions options= new JavaProjectOptionsImpl(javaProject.getOptions(true));

            final Environment environment= getEnvironment();
            final List<RefactoringRule> refactoringRules= getConfiguredRefactoringRules(fOptions);
            final SubMonitor loopMonitor= SubMonitor.convert(null, 1);
            final Queue<RefactoringUnit> refactoringUnits= new ConcurrentLinkedQueue<>();
            refactoringUnits.add(new RefactoringUnit(iCompilationUnit, options));

            final ApplyRefactoringsJob applyRefactoringsJob= new ApplyRefactoringsJob(refactoringUnits,
                    refactoringRules, environment);
            final AggregateASTVisitor visitor= new AggregateASTVisitor(refactoringRules);
            try {
                List<TextEdit> textEdits= applyRefactoringsJob.applyRefactoring(iCompilationUnit, visitor, options,
                        loopMonitor, false);

                for (TextEdit textEdit : textEdits) {
                    if (hasChanges) {
                        allEdits= TextEditUtil.merge(allEdits, textEdit);
                    } else {
                        hasChanges= true;
                        allEdits= textEdit;
                    }
                }
            } catch (Exception e) {
                if (!hasChanges) {
                    return null;
                }
            }
        }

        if (!hasChanges) {
            return null;
        }

        cleanUpChange.setEdit(allEdits);
        AutoRefactorFix autoRefactorFix= new AutoRefactorFix();
        autoRefactorFix.cleanUpChange= cleanUpChange;
        return autoRefactorFix;
    }

    /**
     * Returns the cleanup rules which have been enabled from the Eclipse
     * preferences.
     *
     * @param options the options
     * @return the cleanup rules which have been enabled from the Eclipse
     *         preferences
     */
    public static List<RefactoringRule> getConfiguredRefactoringRules(final CleanUpOptions options) {
        final List<RefactoringRule> refactorings= AllCleanUpRules.getAllCleanUpRules();
        for (final Iterator<RefactoringRule> iter= refactorings.iterator(); iter.hasNext();) {
            final RefactoringRule refactoring= iter.next();
            final String cleanupPropertyName= AutoRefactorTabPage.getCleanupPropertyName(refactoring);

            if (!AutoRefactorPlugin.getDefault().getPreferenceStore().contains(cleanupPropertyName)
                    || !CleanUpOptions.TRUE.equals(
                            AutoRefactorPlugin.getDefault().getPreferenceStore().getString(cleanupPropertyName))) {
                iter.remove();
            }
        }
        return refactorings;
    }

    /**
     * Create the change.
     *
     * @param progressMonitor progress monitor
     * @return Compilation unit change
     * @throws CoreException Core exception
     */
    public CleanUpChange createChange(final IProgressMonitor progressMonitor) throws CoreException {
        return cleanUpChange;
    }
}
