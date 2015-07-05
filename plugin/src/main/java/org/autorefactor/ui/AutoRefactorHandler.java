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
package org.autorefactor.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.refactoring.SourceLocation;
import org.autorefactor.refactoring.rules.AllRefactoringRules;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.handlers.HandlerUtil;

import static org.autorefactor.AutoRefactorPlugin.*;
import static org.eclipse.jface.dialogs.MessageDialog.*;

/**
 * This is the Eclipse handler for launching the automated refactorings. This is
 * invoked from the Eclipse UI.
 *
 * @see <a
 * href="http://www.vogella.com/articles/EclipsePlugIn/article.html#contribute"
 * >Extending Eclipse - Plug-in Development Tutorial</a>
 */
public class AutoRefactorHandler extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        new PrepareApplyRefactoringsJob(
                getSelectedJavaElements(event),
                getSelectedSourceLocation(event),
                AllRefactoringRules.getConfiguredRefactoringRules()).schedule();

        // TODO JNR provide a maven plugin
        // TODO JNR provide a gradle plugin
        // TODO JNR provide an ant task
        // @see http://stackoverflow.com/questions/2113865/jdt-without-eclipse

        // TODO JNR provide from the UI the ability to execute groovy (other
        // scripts? rhino?) scripts for refactoring.

        // <p> Extract method: Live variable analysis - READ WRITE variable
        // analysis (including method params).If variable used in extracted
        // method and WRITE first in selected text => do not pass it down as
        // parameter.Use ASTMatcher and do not compare content of expressions,
        // compare just resolvedTypeBinding().
        return null;
    }

    static List<IJavaElement> getSelectedJavaElements(ExecutionEvent event) {
        final String activePartId = HandlerUtil.getActivePartId(event);
        if ("org.eclipse.jdt.ui.CompilationUnitEditor".equals(activePartId)) {
            return getFromJavaEditor(event);
        } else if ("org.eclipse.jdt.ui.PackageExplorer".equals(activePartId)) {
            return getFromPackageExplorer(event);
        } else {
            logWarning("Code is not implemented for activePartId '" + activePartId + "'.");
            return Collections.emptyList();
        }
    }

    private static List<IJavaElement> getFromJavaEditor(ExecutionEvent event) {
        final Shell shell = HandlerUtil.getActiveShell(event);
        final IEditorInput editorInput = HandlerUtil.getActiveEditor(event).getEditorInput();
        final IJavaElement javaElement = JavaUI.getEditorInputJavaElement(editorInput);
        if (javaElement instanceof ICompilationUnit) {
            return Collections.singletonList(javaElement);
        }
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                openInformation(shell, "Info", "This action only works on Java source files");
            }
        });
        return Collections.emptyList();
    }

    private static List<IJavaElement> getFromPackageExplorer(ExecutionEvent event) {
        final Shell shell = HandlerUtil.getActiveShell(event);
        final IStructuredSelection selection = (IStructuredSelection) HandlerUtil.getCurrentSelection(event);
        final List<IJavaElement> results = new ArrayList<IJavaElement>();
        final Iterator<?> it = selection.iterator();
        while (it.hasNext()) {
            final Object el = it.next();
            if (el instanceof ICompilationUnit
                    || el instanceof IPackageFragment
                    || el instanceof IPackageFragmentRoot
                    || el instanceof IJavaProject) {
                results.add((IJavaElement) el);
            } else {
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        openInformation(shell, "Info",
                            "Please select a Java source file, Java package or Java project");
                    }
                });
            }
        }
        return results;
    }

    static SourceLocation getSelectedSourceLocation(ExecutionEvent event) {
        final String activePartId = HandlerUtil.getActivePartId(event);
        if (!"org.eclipse.jdt.ui.CompilationUnitEditor".equals(activePartId)) {
            logWarning("Code is not implemented for activePartId '" + activePartId + "'.");
            return null;
        }
        final ISelection selection = HandlerUtil.getCurrentSelection(event);
        if (selection instanceof TextSelection) {
            final TextSelection sel = (TextSelection) selection;
            return new SourceLocation(sel.getOffset(), sel.getLength());
        }
        return null;
    }
}
