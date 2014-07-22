/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.Arrays;
import java.util.List;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.rules.*;
import org.autorefactor.ui.preferences.PreferenceHelper;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * This is the Eclipse handler for launching the automated refactorings. This is
 * invoked from the Eclipse UI.
 *
 * @see <a
 * href="http://www.vogella.com/articles/EclipsePlugIn/article.html#contribute"
 * >Extending Eclipse - Plug-in Development Tutorial</a>
 */
public class AutoRefactorHandler extends AbstractHandler {

	public Object execute(final ExecutionEvent event) throws ExecutionException {
		new ApplyRefactoringsJob(
				getSelectedJavaElement(event),
				getAllRefactorings()).run(null);

		// TODO JNR provide a maven plugin
		// TODO JNR provide a gradle plugin
		// TODO JNR provide an ant task
		// @see http://stackoverflow.com/questions/2113865/jdt-without-eclipse

		// TODO JNR provide from the UI the ability to execute groovy (other
		// scripts? rhino?) scripts for refactoring.

		//
		// <p> Extract method: Live variable analysis - READ WRITE variable
		// analysis (including method params).If variable used in extracted
		// method and WRITE first in selected text => do not pass it down as
		// parameter.Use ASTMatcher and do not compare content of expressions,
		// compare just resolvedTypeBinding().
		return null;
	}

	static IJavaElement getSelectedJavaElement(ExecutionEvent event) {
		final Shell shell = HandlerUtil.getActiveShell(event);
		final String activePartId = HandlerUtil.getActivePartId(event);
		if ("org.eclipse.jdt.ui.CompilationUnitEditor".equals(activePartId)) {
			IEditorPart activeEditor = HandlerUtil.getActiveEditor(event);
			IJavaElement javaElement = JavaUI
					.getEditorInputJavaElement(activeEditor.getEditorInput());
			if (javaElement instanceof ICompilationUnit) {
				return javaElement;
			}
			Display.getDefault().asyncExec(new Runnable() {

				public void run() {
					MessageDialog.openInformation(shell, "Info",
							"This action only works on Java source files");
				}
			});
		} else if ("org.eclipse.jdt.ui.PackageExplorer".equals(activePartId)) {
			final ISelection sel = HandlerUtil.getCurrentSelection(event);
			final IStructuredSelection selection = (IStructuredSelection) sel;
			final Object firstElement = selection.getFirstElement();
			if (firstElement instanceof ICompilationUnit) {
				return (ICompilationUnit) firstElement;
			} else if (firstElement instanceof IPackageFragment) {
				return (IPackageFragment) firstElement;
			} else if (firstElement instanceof IJavaProject) {
				return (IJavaProject) firstElement;
			} else {
				Display.getDefault().asyncExec(new Runnable() {

					public void run() {
						MessageDialog.openInformation(shell, "Info",
								"Please select a Java source file, Java package or Java project");
					}

				});
			}
		}
		return null;
	}

	private static List<IRefactoring> getAllRefactorings() {
		PreferenceHelper prefs = AutoRefactorPlugin.getPreferenceHelper();
		return new ArrayList<IRefactoring>(Arrays.asList(
				new VectorOldToNewAPIRefactoring(),
				new PrimitiveWrapperCreationRefactoring(),
				new BooleanRefactoring(),
				new AddBracketsToControlStatementRefactoring(prefs.addAngleBracketsToStatementBodies()),
				new InvertEqualsRefactoring(),
				new SimplifyExpressionRefactoring(prefs.removeThisForNonStaticMethodAccess()),
				new StringRefactoring(),
				new BigDecimalRefactoring(),
				// TODO JNR implement
				// new ForeachRefactoring(),
				new DeadCodeEliminationRefactoring(),
				new CollapseIfStatementRefactoring(),
				new CommonCodeInIfElseStatementRefactoring(),
				// TODO JNR complete it
				// new GenerecizeRefactoring(),
				new CollectionAddAllRefactoring(),
				new IfStatementRefactoring(),
				// TODO JNR implement
				// new RemoveStupidIdiomaticPatternRefactoring(),
				// TODO JNR test for Eclipse bug fix - remove
				// new ExtractMethodTestRefactoring(),
				// TODO JNR - to be completed
				// new ReduceVariableScopeRefactoring(),
				new StringBuilderRefactoring(),
				new CommentsRefactoring(),
				new RemoveFieldsDefaultValuesRefactoring(),
				new RemoveUnnecessaryLocalBeforeReturnRefactoring(),
				new RemoveUselessModifiersRefactoring(),
				new HotSpotIntrinsicedAPIsRefactoring()));
	}

}
