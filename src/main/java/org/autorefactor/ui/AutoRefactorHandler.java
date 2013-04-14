/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Refactorings.Insert;
import org.autorefactor.refactoring.Refactorings.InsertType;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.rules.AddBracketsToControlStatementRefactoring;
import org.autorefactor.refactoring.rules.BigDecimalRefactorings;
import org.autorefactor.refactoring.rules.BooleanRefactoring;
import org.autorefactor.refactoring.rules.CollapseIfStatementRefactoring;
import org.autorefactor.refactoring.rules.CommonCodeInIfElseStatementRefactoring;
import org.autorefactor.refactoring.rules.InvertEqualsRefactoring;
import org.autorefactor.refactoring.rules.PrimitiveWrapperCreationRefactoring;
import org.autorefactor.refactoring.rules.RemoveEmptyCommentsRefactoring;
import org.autorefactor.refactoring.rules.RemoveUnnecessaryLocalBeforeReturnRefactoring;
import org.autorefactor.refactoring.rules.RemoveUselessModifiersRefactoring;
import org.autorefactor.refactoring.rules.SimplifyExpressionRefactoring;
import org.autorefactor.refactoring.rules.StringBuilderRefactoring;
import org.autorefactor.refactoring.rules.StringRefactorings;
import org.autorefactor.util.Pair;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * This is the Eclipse handler for launching the automated refactorings. This is
 * invoked from the Eclipse UI.
 * 
 * @see <a
 *      href="http://www.vogella.com/articles/EclipsePlugIn/article.html#contribute">Extending
 *      Eclipse - Plug-in Development Tutorial</a>
 */
public class AutoRefactorHandler extends AbstractHandler {

	private static class ApplyRefactoringsJob extends Job {

		private final ExecutionEvent event;

		private ApplyRefactoringsJob(ExecutionEvent event) {
			super("Auto Refactor");
			this.event = event;
		}

		@Override
		protected IStatus run(IProgressMonitor monitor) {
			final IJavaElement javaElement = getSelectedJavaElement(event);
			if (javaElement == null) {
				// No java project exists.
				return Status.OK_STATUS;
			}
			final List<ICompilationUnit> compilationUnits = collectCompilationUnits(javaElement);
			final String javaSourceCompatibility = getJavaSourceCompatibility(javaElement);

			monitor.beginTask("", compilationUnits.size());
			try {
				for (final ICompilationUnit compilationUnit : compilationUnits) {
					try {
						final String elName = compilationUnit.getElementName();
						final String simpleName = elName.substring(0,
								elName.lastIndexOf('.'));
						final String className = compilationUnit.getParent()
								.getElementName() + "." + simpleName;
						monitor.subTask("Applying refactorings to " + className);
						applyRefactorings(compilationUnit,
								Release.javaSE(javaSourceCompatibility), false);
					} catch (Exception e) {
						throw new RuntimeException(e);
					} finally {
						monitor.worked(1);
					}
				}
			} finally {
				monitor.done();
			}
			return Status.OK_STATUS;
		}

		/**
		 * Does not work:
		 * 
		 * <pre>
		 * Caused by: java.lang.IllegalArgumentException: This API can only be used if the AST is created from a compilation unit or class file
		 * 	at org.eclipse.jdt.core.dom.rewrite.ASTRewrite.rewriteAST(ASTRewrite.java:272)
		 * 	at org.autorefactor.ui.AutoRefactorHandler.applyRefactorings(RefactorHandler.java:367)
		 * </pre>
		 */
		private void testWithSamples() {
			final Shell shell = HandlerUtil.getActiveShell(event);
			try {
				final IJavaProject javaProject = getIJavaProject(getSelectedJavaElement(event));
				final String javaSourceCompatibility = getJavaSourceCompatibility(javaProject);
				for (IPackageFragmentRoot packageFragmentRoot : javaProject
						.getPackageFragmentRoots()) {
					final List<ICompilationUnit> samplesIn = getSamples(
							packageFragmentRoot, "org.autorefactor.samples_in");
					final List<ICompilationUnit> samplesOut = getSamples(
							packageFragmentRoot, "org.autorefactor.samples_out");
					if (samplesIn.size() != samplesOut.size()) {
						MessageDialog
								.openInformation(shell, "Error",
										"Different number of samples in and samples out. Cannot validate anything.");
					} else {
						for (ICompilationUnit compilationUnit : samplesIn) {
							final String elementName = compilationUnit
									.getElementName();
							final String className = elementName
									.substring(elementName.indexOf('.'));
							final ICompilationUnit sampleOut = getSampleOut(
									samplesOut, className);
							if (sampleOut == null) {
								MessageDialog.openInformation(shell, "Error",
										"Could not find a sample out for class name "
												+ className);
								continue;
							}
							// final String newContent = applyRefactorings(
							// compilationUnit,
							// Release.javaSE(javaSourceCompatibility),
							// true);
							// if (!newContent.equals(sampleOut.getSource())) {
							// MessageDialog.openInformation(shell, "Error",
							// "Refactorings did not provide expected output for class name "
							// + className);
							// continue;
							// }
						}
					}
				}
			} catch (Exception e) {
				throw new RuntimeException("Unexpected exception", e);
			}
		}

		private ICompilationUnit getSampleOut(
				List<ICompilationUnit> samplesOut, String className) {
			for (ICompilationUnit compilationUnit : samplesOut) {
				final String elementName = compilationUnit.getElementName();
				if (className.equals(elementName.substring(elementName
						.indexOf('.')))) {
					return compilationUnit;
				}
			}
			return null;
		}

		private List<ICompilationUnit> getSamples(
				IPackageFragmentRoot packageFragmentRoot, String packageName) {
			final IPackageFragment packag = packageFragmentRoot
					.getPackageFragment(packageName);
			if (packag != null) {
				return collectCompilationUnits(packag);
			}
			return Collections.emptyList();
		}
	}

	public Object execute(final ExecutionEvent event) throws ExecutionException {
		// new ApplyRefactoringsJob(event).testWithSamples();
		// if (true) {
		// return Status.OK_STATUS;
		// }

		new ApplyRefactoringsJob(event).schedule();

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

	private static IJavaElement getSelectedJavaElement(ExecutionEvent event) {
		final Shell shell = HandlerUtil.getActiveShell(event);
		final String activePartId = HandlerUtil.getActivePartId(event);
		if ("org.eclipse.jdt.ui.CompilationUnitEditor".equals(activePartId)) {
			IEditorPart activeEditor = HandlerUtil.getActiveEditor(event);
			IJavaElement javaElement = JavaUI
					.getEditorInputJavaElement(activeEditor.getEditorInput());
			if (javaElement instanceof ICompilationUnit) {
				return javaElement;
			} else {
				MessageDialog.openInformation(shell, "Info",
						"This action only works on Java source files");
			}
		} else if ("org.eclipse.jdt.ui.PackageExplorer".equals(activePartId)) {
			final ISelection sel = HandlerUtil.getCurrentSelection(event);
			final IStructuredSelection selection = (IStructuredSelection) sel;
			final Object firstElement = selection.getFirstElement();
			if (firstElement instanceof ICompilationUnit) {
				return (ICompilationUnit) firstElement;
			} else if (firstElement instanceof IPackageFragment) {
				return (IPackageFragment) firstElement;
			} else if (firstElement instanceof IJavaProject) {
				// project.isNatureEnabled("org.eclipse.jdt.core.javanature") ?
				return (IJavaProject) firstElement;
			} else {
				MessageDialog
						.openInformation(shell, "Info",
								"Please select a Java source file, Java package or Java project");
			}
		}
		return null;
	}

	private static List<ICompilationUnit> collectCompilationUnits(
			IJavaElement javaElement) {
		try {
			final List<ICompilationUnit> results = new LinkedList<ICompilationUnit>();
			if (javaElement instanceof ICompilationUnit) {
				results.add((ICompilationUnit) javaElement);
			} else if (javaElement instanceof IPackageFragment) {
				final IPackageFragment pf = (IPackageFragment) javaElement;
				addAll(results, pf.getCompilationUnits());
			} else if (javaElement instanceof IJavaProject) {
				IJavaProject javaProject = (IJavaProject) javaElement;
				for (IPackageFragment pf : javaProject.getPackageFragments()) {
					addAll(results, pf.getCompilationUnits());
				}
			}
			return results;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private static void addAll(final List<ICompilationUnit> results,
			ICompilationUnit[] compilationUnits) throws JavaModelException {
		for (ICompilationUnit cu : compilationUnits) {
			if (!cu.isConsistent()) {
				cu.makeConsistent(null);
			}
			if (!cu.isReadOnly()) {// is consistent?
				results.add(cu);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static String getJavaSourceCompatibility(IJavaElement javaElement) {
		final IJavaProject javaProject = getIJavaProject(javaElement);
		final Map<String, String> options = javaProject.getOptions(true);
		return options.get(JavaCore.COMPILER_SOURCE);
	}

	private static IJavaProject getIJavaProject(IJavaElement javaElement) {
		if (javaElement instanceof ICompilationUnit
				|| javaElement instanceof IPackageFragment
				|| javaElement instanceof IPackageFragmentRoot) {
			return getIJavaProject(javaElement.getParent());
		} else if (javaElement instanceof IJavaProject) {
			return (IJavaProject) javaElement;
		}
		throw new RuntimeException("Not implemented for "
				+ (javaElement != null ? javaElement.getClass() : "null"));
	}

	/**
	 * @param compilationUnit
	 * @param javaSERelease
	 * @param isInMemory
	 * @return
	 * @throws Exception
	 * @see <a
	 *      href="http://help.eclipse.org/indigo/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Fguide%2Fjdt_api_manip.htm">Eclipse
	 *      JDT core - Manipulating Java code</a>
	 * 
	 * @see <a
	 *      href="http://help.eclipse.org/indigo/index.jsp?topic=/org.eclipse.platform.doc.isv/guide/workbench_cmd_menus.htm">
	 *      Eclipse Platform Plug-in Developer Guide > Plugging into the
	 *      workbench > Basic workbench extension points using commands >
	 *      org.eclipse.ui.menus </a>
	 */
	private static void applyRefactorings(ICompilationUnit compilationUnit,
			Release javaSERelease, boolean isInMemory) throws Exception {
		// creation of DOM/AST from a ICompilationUnit
		final ASTParser parser = ASTParser.newParser(AST.JLS4);
		parser.setSource(compilationUnit);
		parser.setResolveBindings(true);

		CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);
		// final List<CFGBasicBlock> basicBlocks = new
		// CFGBuilder(astRoot.getAST())
		// .buildCFG(astRoot);

		// TODO JNR add testing for the refactorings, can it be done like in the
		// Main class of coccimain_javacc project?
		final IDocument document = new Document(compilationUnit.getSource());
		for (IRefactoring refactoring : getAllRefactorings()) {
			try {
				refactoring.setAST(astRoot.getAST());
				// TODO JNR pass down Java version or library version
				if (refactoring instanceof IJavaRefactoring) {
					((IJavaRefactoring) refactoring)
							.setJavaSERelease(javaSERelease);
					// }else if (refactoring instanceof
					// IApacheCommonsRefactoring) {
					// }else if (refactoring instanceof IGuavaRefactoring) {
				}
				final Refactorings refactorings = refactoring
						.getRefactorings(astRoot);
				if (refactorings.hasRefactorings()) {
					// Describing the rewrite
					final ASTRewrite rewrite = getASTRewrite(astRoot,
							refactorings);

					// apply the text edits and save the compilation unit
					final TextEdit edits = rewrite.rewriteAST();
					edits.apply(document);
					final String newSource = document.get();
					if (!isInMemory) {
						boolean hadUnsavedChanges = compilationUnit.hasUnsavedChanges();
						compilationUnit.getBuffer().setContents(newSource);
						if (!hadUnsavedChanges) {
							compilationUnit.save(null, true);
						}
					}

					// I did not find any other way to directly modify the AST
					// while
					// still keeping the resolved type bindings working.
					// Using astRoot.recordModifications() did not work:
					// type bindings were lost. Is there a way to recover them?
					// FIXME we should find a way to apply all the changes at
					// the AST level only (transactional-like feature) and
					// refresh the bindings
					parser.setSource(compilationUnit);
					parser.setResolveBindings(true);
					astRoot = (CompilationUnit) parser.createAST(null);
				}
			} catch (Exception e) {
				// TODO JNR add UI error reporting
				throw new RuntimeException("Unexpected exception", e);
			}
		}
	}

	private static ASTRewrite getASTRewrite(final CompilationUnit astRoot,
			final Refactorings refactorings) {
		final ASTRewrite rewrite = ASTRewrite.create(astRoot.getAST());
		if (!refactorings.getInserts().isEmpty()) {
			for (Entry<ChildListPropertyDescriptor, List<Insert>> entry : refactorings
					.getInserts().entrySet()) {
				for (final Insert insert : entry.getValue()) {
					final ListRewrite listRewrite = rewrite.getListRewrite(
							insert.getElement().getParent(), entry.getKey());
					if (InsertType.BEFORE.equals(insert.getInsertType())) {
						listRewrite.insertBefore(insert.getNodeToInsert(),
								insert.getElement(), null);
					} else if (InsertType.AFTER.equals(insert.getInsertType())) {
						listRewrite.insertAfter(insert.getNodeToInsert(),
								insert.getElement(), null);
					}
				}
			}
		}
		for (Pair<ASTNode, ASTNode> entry : refactorings.getReplacements()) {
			rewrite.replace(entry.getFirst(), entry.getSecond(), null);
		}
		for (ASTNode toRemove : refactorings.getRemovals()) {
			rewrite.remove(toRemove, null);
		}
		return rewrite;
	}

	private static List<IRefactoring> getAllRefactorings() {
		return Arrays.<IRefactoring> asList(
				new PrimitiveWrapperCreationRefactoring(),
				new BooleanRefactoring(),
				new AddBracketsToControlStatementRefactoring(),
				new InvertEqualsRefactoring(),
				new SimplifyExpressionRefactoring(),
				new StringRefactorings(),
				new BigDecimalRefactorings(),
				// TODO JNR implement
				// new ForeachRefactoring(),
				// TODO JNR implement
				// new DeadCodeEliminationRefactoring(),
				new CollapseIfStatementRefactoring(),
				new CommonCodeInIfElseStatementRefactoring(),
				// TODO JNR complete it
				// new GenerecizeRefactoring(),
				// TODO JNR implement
				// new IfStatementRefactoring(),
				// TODO JNR implement
				// new RemoveStupidIdiomaticPatternRefactoring(),
				// TODO JNR test for Eclipse bug fix - remove
				// new ExtractMethodTestRefactoring(),
				// TODO JNR - to be completed
				// new ReduceVariableScopeRefactoring(),
				new StringBuilderRefactoring(),
				new RemoveEmptyCommentsRefactoring(),
				new RemoveUnnecessaryLocalBeforeReturnRefactoring(),
				new RemoveUselessModifiersRefactoring());
	}

}
