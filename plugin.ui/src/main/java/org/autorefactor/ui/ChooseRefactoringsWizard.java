/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2017 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.PrepareApplyRefactoringsJob;
import org.autorefactor.refactoring.RefactoringRule;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.wizard.Wizard;

import static org.autorefactor.AutoRefactorPlugin.*;

/** Wizard which allows the user to choose which refactorings to apply to the selected java elements. */
public class ChooseRefactoringsWizard extends Wizard {
    private final ChooseRefactoringWizardPage chooseRefactoringsPage = new ChooseRefactoringWizardPage();
    private final List<IJavaElement> javaElements;

    /**
     * Builds an instance of this class, with the provided java element.
     *
     * @param javaElements the java elements from where to extract the project options
     */
    public ChooseRefactoringsWizard(List<IJavaElement> javaElements) {
        setNeedsProgressMonitor(true);
        this.javaElements = javaElements;
    }

    @Override
    public String getWindowTitle() {
        return "Choose refactorings...";
    }

    @Override
    public void addPages() {
        addPage(chooseRefactoringsPage);
    }

    @Override
    public boolean performFinish() {
        final List<RefactoringRule> refactoringRules = chooseRefactoringsPage.getSelectedRefactorings();
        new PrepareApplyRefactoringsJob(javaElements, refactoringRules, getEnvironment()).schedule();
        return !refactoringRules.isEmpty();
    }
}
