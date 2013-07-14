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

import static org.eclipse.jdt.core.JavaCore.*;
import static org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants.*;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.autorefactor.cfg.CFGBuilder;
import org.autorefactor.refactoring.ASTCommentRewriter;
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
import org.autorefactor.refactoring.rules.DeadCodeEliminationRefactoring;
import org.autorefactor.refactoring.rules.InvertEqualsRefactoring;
import org.autorefactor.refactoring.rules.PrimitiveWrapperCreationRefactoring;
import org.autorefactor.refactoring.rules.RefactoringContext;
import org.autorefactor.refactoring.rules.RemoveEmptyCommentsRefactoring;
import org.autorefactor.refactoring.rules.RemoveUnnecessaryLocalBeforeReturnRefactoring;
import org.autorefactor.refactoring.rules.RemoveUselessModifiersRefactoring;
import org.autorefactor.refactoring.rules.SimplifyExpressionRefactoring;
import org.autorefactor.refactoring.rules.StringBuilderRefactoring;
import org.autorefactor.refactoring.rules.StringRefactorings;
import org.autorefactor.ui.GrowableArrayList.GrowableListIterator;
import org.autorefactor.util.Pair;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBufferManager;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jdt.internal.core.ExternalPackageFragmentRoot;
import org.eclipse.jdt.internal.core.JarPackageFragmentRoot;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
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
			final Map<String, String> options = getJavaProjectOptions(javaElement);
			final String javaSourceCompatibility = options.get(COMPILER_SOURCE);
			final int tabSize = getTabSize(options);

			if (monitor != null) {
				monitor.beginTask("", compilationUnits.size());
			}
			try {
				final Release javaSERelease = Release.javaSE(javaSourceCompatibility);
				for (final ICompilationUnit compilationUnit : compilationUnits) {
					try {
						final String elName = compilationUnit.getElementName();
						final String simpleName = elName.substring(0,
								elName.lastIndexOf('.'));
						final String className = compilationUnit.getParent()
								.getElementName() + "." + simpleName;
						if (monitor != null) {
							monitor.subTask("Applying refactorings to " + className);
						}
						applyRefactorings(compilationUnit, javaSERelease,
								tabSize, getAllRefactorings());
					} catch (Exception e) {
						throw new RuntimeException(e);
					} finally {
						if (monitor != null) {
							monitor.worked(1);
						}
					}
				}
			} finally {
				if (monitor != null) {
					monitor.done();
				}
			}
			return Status.OK_STATUS;
		}

		private int getTabSize(final Map<String, String> options) {
			String tabSize = options.get(FORMATTER_INDENTATION_SIZE);
			try {
				return Integer.valueOf(tabSize);
			} catch (NumberFormatException e) {
				throw new RuntimeException("Unhandled exception");
			}
		}

		private void testWithSamples() {
			try {
				final IJavaProject javaProject = getIJavaProject(getSelectedJavaElement(event));
				final Map<String, String> options = getJavaProjectOptions(javaProject);
				final String javaSourceCompatibility = options.get(COMPILER_SOURCE);
				final int tabSize = getTabSize(options);
				final Release javaSERelease = Release.javaSE(javaSourceCompatibility);
				for (IPackageFragmentRoot packageFragmentRoot : javaProject
						.getPackageFragmentRoots()) {
					if (packageFragmentRoot instanceof JarPackageFragmentRoot
							|| packageFragmentRoot instanceof ExternalPackageFragmentRoot) {
						continue;
					}

					String samplesInPkg = "org.autorefactor.samples_in";
					String samplesOutPkg = "org.autorefactor.samples_out";
					final List<ICompilationUnit> samplesIn = getSamples(
							packageFragmentRoot, samplesInPkg);
					final List<ICompilationUnit> samplesOut = getSamples(
							packageFragmentRoot, samplesOutPkg);

					Collection<TestCase> testCases = buildTestCases(samplesIn, samplesOut);
					runTests(testCases, samplesInPkg, samplesOutPkg,
							javaSERelease, tabSize);
				}
			} catch (Exception e) {
				throw new RuntimeException("Unexpected exception", e);
			}
		}

		private void runTests(Collection<TestCase> testCases,
				String samplesInPkg, String samplesOutPkg,
				final Release javaSERelease, int tabSize) throws Exception {
			boolean success = true;
			final StringBuilder result = new StringBuilder();

			for (TestCase testCase : testCases) {
				result.append(testCase.sampleName).append(".java: ");

				if (testCase.refactoring == null
						|| testCase.sampleIn == null
						|| testCase.sampleOut == null) {
					result.append("MISSING ");
					if (testCase.sampleIn == null || testCase.sampleOut == null) {
						result.append(testCase.getINAndOUT(false));
						if (testCase.refactoring == null) {
							result.append(" and Refactoring");
						}
					} else if (testCase.refactoring == null) {
						result.append("Refactoring");
					}
					result.append("\n");
					success = false;
					continue;
				}

				applyRefactorings(testCase.sampleIn, javaSERelease, tabSize,
						new GrowableArrayList<IRefactoring>(testCase.refactoring));

				// Change the package to be the same as the sampleOut
				// and ignore insignificant space characters
				String actualSource = testCase.sampleIn.getSource()
						.replaceAll(samplesInPkg, samplesOutPkg)
						.replaceAll("\\s\\s+", "\n");
				String expectedSource = testCase.sampleOut.getSource()
						.replaceAll("\\s\\s+", "\n");
				if (actualSource.equals(expectedSource)) {
					result.append("Success\n");
				} else {
					result.append("FAILURE\n");
					success = false;
				}
			}

			final Shell shell = HandlerUtil.getActiveShell(event);
			if (success) {
				MessageDialog.openInformation(shell, "Tests Success!!", result.toString());
			} else {
				MessageDialog.openError(shell, "Tests ERROR", result.toString());
			}
		}

		private Collection<TestCase> buildTestCases(
				final List<ICompilationUnit> samplesIn,
				final List<ICompilationUnit> samplesOut) {
			Map<String, TestCase> testCases = new TreeMap<String, TestCase>();
			for (ICompilationUnit sampleIn : samplesIn) {
				final String sampleName = getSampleName(sampleIn);
				TestCase testCase = getTestCase(testCases, sampleName);
				testCase.sampleIn = sampleIn;
			}
			for (ICompilationUnit sampleOut : samplesOut) {
				final String sampleName = getSampleName(sampleOut);
				TestCase testCase = getTestCase(testCases, sampleName);
				testCase.sampleOut = sampleOut;
			}
			for (IRefactoring refactoring : getAllRefactorings()) {
				String name = refactoring.getClass().getSimpleName();
				String sampleName = name.substring(0, name.indexOf("Refactoring")) + "Sample";
				TestCase testCase = getTestCase(testCases, sampleName);
				testCase.refactoring = refactoring;
			}
			return testCases.values();
		}

		private TestCase getTestCase(Map<String, TestCase> testContexts,
				String sampleName) {
			TestCase testCase = testContexts.get(sampleName);
			if (testCase == null) {
				testCase = new TestCase(sampleName);
				testContexts.put(sampleName, testCase);
			}
			return testCase;
		}

		private String getSampleName(ICompilationUnit compilationUnit) {
			final String elementName = compilationUnit.getElementName();
			return elementName.substring(0, elementName.indexOf('.'));
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
		// TODO JNR keep track of the job so it can be cancelled by the plugin on workspace exit

		boolean useTests = false; // local variable helps switching while debugging
		if (useTests) {
			new ApplyRefactoringsJob(event).testWithSamples();
			return Status.OK_STATUS;
		}

		new ApplyRefactoringsJob(event).run(null);

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
				// project.isNatureEnabled("org.eclipse.jdt.core.javanature") ?
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

	private static List<ICompilationUnit> collectCompilationUnits(
			IJavaElement javaElement) {
		try {
			final List<ICompilationUnit> results = new LinkedList<ICompilationUnit>();
			if (javaElement instanceof ICompilationUnit) {
				add(results, (ICompilationUnit) javaElement);
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
			add(results, cu);
		}
	}

	private static void add(final List<ICompilationUnit> results,
			ICompilationUnit cu) throws JavaModelException {
		if (!cu.isConsistent()) {
			cu.makeConsistent(null);
		}
		if (!cu.isReadOnly()) {
			results.add(cu);
		}
	}

	@SuppressWarnings("unchecked")
	private static Map<String, String> getJavaProjectOptions(
			IJavaElement javaElement) {
		final IJavaProject javaProject = getIJavaProject(javaElement);
		return javaProject.getOptions(true);
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
	 * @param tabSize
	 * @param isInMemory
	 * @throws Exception
	 * @see <a
	 *      href="http://help.eclipse.org/indigo/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Fguide%2Fjdt_api_manip.htm">Eclipse
	 *      JDT core - Manipulating Java code</a>
	 *
	 * @see <a
	 *      href="http://help.eclipse.org/indigo/index.jsp?topic=/org.eclipse.platform.doc.isv/guide/workbench_cmd_menus.htm">
	 *      Eclipse Platform Plug-in Developer Guide > Plugging into the
	 *      workbench > Basic workbench extension points using commands >
	 *      org.eclipse.ui.menus</a>
	 * @see <a
	 *      href="http://www.eclipse.org/articles/article.php?file=Article-JavaCodeManipulation_AST/index.html#sec-write-it-down">
	 *      Abstract Syntax Tree > Write it down</a>
	 */
	private static void applyRefactorings(ICompilationUnit compilationUnit,
			Release javaSERelease, int tabSize,
			GrowableArrayList<IRefactoring> refactoringsToApply) throws Exception {
		final ITextFileBufferManager bufferManager = FileBuffers.getTextFileBufferManager();
		final IPath path = compilationUnit.getPath();
		final LocationKind locationKind = LocationKind.NORMALIZE;
		try {
			bufferManager.connect(path, locationKind, null);
			final ITextFileBuffer textFileBuffer = bufferManager
					.getTextFileBuffer(path, locationKind);
			final IDocument document = textFileBuffer.getDocument();
			applyRefactorings(document, compilationUnit,
					javaSERelease, tabSize, refactoringsToApply);
			textFileBuffer.commit(null, false);
		} finally {
			bufferManager.disconnect(path, locationKind, null);
		}
	}

	private static void applyRefactorings(IDocument document,
			ICompilationUnit compilationUnit, Release javaSERelease,
			int tabSize, GrowableArrayList<IRefactoring> refactoringsToApply)
					throws JavaModelException {
		// creation of DOM/AST from a ICompilationUnit
		final ASTParser parser = ASTParser.newParser(AST.JLS4);
		parser.setSource(compilationUnit);
		parser.setResolveBindings(true);

		CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);

		// new CFGBuilder(compilationUnit.getSource(),
		// tabSize).buildCFG(astRoot);

		for (GrowableListIterator iter = refactoringsToApply.iterator(); iter.hasNext();) {
			IRefactoring refactoring = (IRefactoring) iter.next();
			try {
				final RefactoringContext ctx = new RefactoringContext(compilationUnit,
						astRoot.getAST(), javaSERelease);
				refactoring.setRefactoringContext(ctx);

				final Refactorings refactorings = refactoring.getRefactorings(astRoot);
				if (refactorings.hasRefactorings()) {
					// Describing the rewrite
					final Pair<ASTRewrite, ASTCommentRewriter> rewrites = getASTRewrite(astRoot, refactorings);
					final ASTCommentRewriter commentRewriter = rewrites.getSecond();

					// apply the text edits and save the compilation unit
					final TextEdit edits = rewrites.getFirst().rewriteAST(document, null);
					commentRewriter.addEdits(document, edits);
					edits.apply(document);
					boolean hadUnsavedChanges = compilationUnit.hasUnsavedChanges();
					compilationUnit.getBuffer().setContents(document.get());
					// http://wiki.eclipse.org/FAQ_What_is_a_working_copy%3F
					// compilationUnit.reconcile(AST.JLS4,
					// ICompilationUnit.ENABLE_BINDINGS_RECOVERY |
					// ICompilationUnit.ENABLE_STATEMENTS_RECOVERY |
					// ICompilationUnit.FORCE_PROBLEM_DETECTION
					// /** can be useful to back off a change that does not
					// compile*/
					// , null, null);
					if (!hadUnsavedChanges) {
						compilationUnit.save(null, true);
					}

					// I did not find any other way to directly modify the AST
					// while still keeping the resolved type bindings working.
					// Using astRoot.recordModifications() did not work:
					// type bindings were lost. Is there a way to recover them?
					// FIXME we should find a way to apply all the changes at
					// the AST level only (transactional-like feature) and
					// refresh the bindings
					parser.setSource(compilationUnit);
					parser.setResolveBindings(true);
					astRoot = (CompilationUnit) parser.createAST(null);

					// Append the already done refactorings to see if more refactorings
					// were enabled after applying the current refactoring.
					iter.reloop();
				}
			} catch (Exception e) {
				// TODO JNR add UI error reporting
				throw new RuntimeException("Unexpected exception", e);
			}
		}
	}

	private static Pair<ASTRewrite, ASTCommentRewriter> getASTRewrite(
			final CompilationUnit astRoot, final Refactorings refactorings) {
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
		final ASTCommentRewriter commentRewriter = new ASTCommentRewriter();
		for (ASTNode toRemove : refactorings.getCommentRemovals()) {
			commentRewriter.remove(toRemove);
		}
		for (BlockComment toJavadoc : refactorings.getBlockCommentToJavadoc()) {
			commentRewriter.toJavadoc(toJavadoc);
		}
		for (List<LineComment> toJavadoc : refactorings.getLineCommentsToJavadoc()) {
			commentRewriter.toJavadoc(toJavadoc);
		}
		return Pair.of(rewrite, commentRewriter);
	}

	private static GrowableArrayList<IRefactoring> getAllRefactorings() {
		return new GrowableArrayList<IRefactoring>(
				new PrimitiveWrapperCreationRefactoring(),
				new BooleanRefactoring(),
				new AddBracketsToControlStatementRefactoring(),
				new InvertEqualsRefactoring(),
				new SimplifyExpressionRefactoring(),
				new StringRefactorings(),
				new BigDecimalRefactorings(),
				// TODO JNR implement
				// new ForeachRefactoring(),
				new DeadCodeEliminationRefactoring(),
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
