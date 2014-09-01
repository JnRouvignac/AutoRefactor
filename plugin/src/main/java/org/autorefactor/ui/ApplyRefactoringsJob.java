/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.rules.AggregateASTVisitor;
import org.autorefactor.refactoring.rules.RefactoringContext;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.UnhandledException;
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
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.text.IDocument;

import static org.eclipse.jdt.core.JavaCore.*;
import static org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants.*;

/**
 * Eclipse job that applies the provided refactorings in background.
 */
public class ApplyRefactoringsJob extends Job {

    private final List<IJavaElement> javaElements;
    private final List<IRefactoring> refactoringsToApply;

    /**
     * Builds an instance of this class.
     *
     * @param javaElements a java element from where to extract the project options
     * @param refactoringsToApply the refactorings to apply
     */
    public ApplyRefactoringsJob(List<IJavaElement> javaElements, List<IRefactoring> refactoringsToApply) {
        super("Auto Refactor");
        setPriority(Job.LONG);
        this.javaElements = javaElements;
        this.refactoringsToApply = refactoringsToApply;
    }

    /** {@inheritDoc} */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        AutoRefactorPlugin.register(this);
        try {
            return run0(monitor);
        } catch (Exception e) {
            final String msg = "Error while applying refactorings.\n\n"
                    + "Please look at the Eclipse workspace logs and "
                    + "report the stacktrace to the AutoRefactor project.\n"
                    + "Please provide sample java code that triggers the error.\n\n";
            return new Status(IStatus.ERROR, AutoRefactorPlugin.PLUGIN_ID, msg, e);
        } finally {
            AutoRefactorPlugin.unregister(this);
        }
    }

    private IStatus run0(IProgressMonitor monitor) throws Exception {
        if (javaElements.isEmpty()) {
            // No java project exists.
            return Status.OK_STATUS;
        }
        final List<ICompilationUnit> compilationUnits = collectCompilationUnits(javaElements);
        final Map<String, String> options = getJavaProjectOptions(javaElements);
        final String javaSourceCompatibility = options.get(COMPILER_SOURCE);
        final int tabSize = getTabSize(options);

        monitor.beginTask("", compilationUnits.size());
        try {
            final Release javaSERelease = Release.javaSE(javaSourceCompatibility);
            for (final ICompilationUnit compilationUnit : compilationUnits) {
                if (monitor.isCanceled()) {
                    return Status.CANCEL_STATUS;
                }
                try {
                    final String elName = compilationUnit.getElementName();
                    final String simpleName = elName.substring(0, elName.lastIndexOf('.'));
                    final String className =
                        compilationUnit.getParent().getElementName() + "." + simpleName;

                    monitor.subTask("Applying refactorings to " + className);

                    AggregateASTVisitor refactoring = new AggregateASTVisitor(
                        refactoringsToApply, AutoRefactorPlugin.getPreferenceHelper().debugModeOn());
                    applyRefactoring(compilationUnit, javaSERelease, tabSize, refactoring);
                } finally {
                    monitor.worked(1);
                }
            }
        } finally {
            monitor.done();
        }
        return Status.OK_STATUS;
    }

    private int getTabSize(final Map<String, String> options) {
        String tabSize = options.get(FORMATTER_INDENTATION_SIZE);
        try {
            return Integer.valueOf(tabSize);
        } catch (NumberFormatException e) {
            throw new UnhandledException(e);
        }
    }

    private List<ICompilationUnit> collectCompilationUnits(List<IJavaElement> javaElements) {
        try {
            final List<ICompilationUnit> results = new LinkedList<ICompilationUnit>();
            for (IJavaElement javaElement : javaElements) {
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
            }
            return results;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void addAll(final List<ICompilationUnit> results, ICompilationUnit[] cus) throws JavaModelException {
        for (ICompilationUnit cu : cus) {
            add(results, cu);
        }
    }

    private void add(final List<ICompilationUnit> results, ICompilationUnit cu) throws JavaModelException {
        if (!cu.isConsistent()) {
            cu.makeConsistent(null);
        }
        if (!cu.isReadOnly()) {
            results.add(cu);
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, String> getJavaProjectOptions(List<IJavaElement> javaElements) {
        final IJavaProject javaProject = getIJavaProject(javaElements.get(0));
        return javaProject.getOptions(true);
    }

    private IJavaProject getIJavaProject(IJavaElement javaElement) {
        if (javaElement instanceof ICompilationUnit
                || javaElement instanceof IPackageFragment
                || javaElement instanceof IPackageFragmentRoot) {
            return getIJavaProject(javaElement.getParent());
        } else if (javaElement instanceof IJavaProject) {
            return (IJavaProject) javaElement;
        }
        throw new NotImplementedException(javaElement);
    }

    private void applyRefactoring(ICompilationUnit compilationUnit, Release javaSERelease, int tabSize,
            AggregateASTVisitor refactoringToApply) throws Exception {
        final ITextFileBufferManager bufferManager = FileBuffers.getTextFileBufferManager();
        final IPath path = compilationUnit.getPath();
        final LocationKind locationKind = LocationKind.NORMALIZE;
        try {
            bufferManager.connect(path, locationKind, null);
            final ITextFileBuffer textFileBuffer = bufferManager.getTextFileBuffer(path, locationKind);
            final IDocument document = textFileBuffer.getDocument();
            applyRefactoring(document, compilationUnit, javaSERelease, tabSize, refactoringToApply);
            textFileBuffer.commit(null, false);
        } finally {
            bufferManager.disconnect(path, locationKind, null);
        }
    }

    /**
     * Applies the refactorings provided inside the {@link AggregateASTVisitor} to the provided
     * {@link ICompilationUnit}.
     *
     * @param document the document where the compilation unit comes from
     * @param compilationUnit the compilation unit to refactor
     * @param javaSERelease the Java SE version used to compile the compilation unit
     * @param tabSize the tabulation size in use in the current java project
     * @param refactoring the {@link AggregateASTVisitor} to apply to the compilation unit
     * @throws Exception if any problem occurs
     *
     * @see <a
     * href="http://help.eclipse.org/indigo/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Fguide%2Fjdt_api_manip.htm"
     * >Eclipse JDT core - Manipulating Java code</a>
     * @see <a href="
     * http://help.eclipse.org/indigo/index.jsp?topic=/org.eclipse.platform.doc.isv/guide/workbench_cmd_menus.htm"
     * > Eclipse Platform Plug-in Developer Guide > Plugging into the workbench
     * > Basic workbench extension points using commands > org.eclipse.ui.menus</a>
     * @see <a
     * href="http://www.eclipse.org/articles/article.php?file=Article-JavaCodeManipulation_AST/index.html"
     * >Abstract Syntax Tree > Write it down</a>
     */
    public void applyRefactoring(IDocument document, ICompilationUnit compilationUnit, Release javaSERelease,
            int tabSize, AggregateASTVisitor refactoring) throws Exception {
        // creation of DOM/AST from a ICompilationUnit
        final ASTParser parser = ASTParser.newParser(AST.JLS4);
        resetParser(compilationUnit, parser, javaSERelease);

        CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);

        int totalNbLoops = 0;
        List<ASTVisitor> lastLoopVisitors = Collections.emptyList();
        int nbLoopsWithSameVisitors = 0;
        while (true) {
            if (totalNbLoops > 1000) {
                // Oops! Something went wrong.
                final String message = getPossibleCulprits(nbLoopsWithSameVisitors, lastLoopVisitors);
                throw new IllegalStateException("An infinite loop has been detected."
                        + " A possible cause is that code is being incorrectly"
                        + " refactored one way then refactored back to what it was."
                        + " Fix the code before pursuing."
                        + message);
            }

            final RefactoringContext ctx = new RefactoringContext(compilationUnit,
                    astRoot.getAST(), javaSERelease);
            refactoring.setRefactoringContext(ctx);

            final Refactorings refactorings = refactoring.getRefactorings(astRoot);
            if (!refactorings.hasRefactorings()) {
                // no new refactorings have been applied,
                // we are done with applying the refactorings.
                return;
            }

            // apply the refactorings and save the compilation unit
            refactorings.applyTo(document);
            final boolean hadUnsavedChanges = compilationUnit.hasUnsavedChanges();
            compilationUnit.getBuffer().setContents(document.get());
            // http://wiki.eclipse.org/FAQ_What_is_a_working_copy%3F
            // compilationUnit.reconcile(AST.JLS4,
            // ICompilationUnit.ENABLE_BINDINGS_RECOVERY |
            // ICompilationUnit.ENABLE_STATEMENTS_RECOVERY |
            // ICompilationUnit.FORCE_PROBLEM_DETECTION
            // /** can be useful to back out a change that does not compile */
            // , null, null);
            if (!hadUnsavedChanges) {
                compilationUnit.save(null, true);
            }
            // I did not find any other way to directly modify the AST
            // while still keeping the resolved type bindings working.
            // Using astRoot.recordModifications() did not work:
            // type bindings were lost. Is there a way to recover them?
            // FIXME we should find a way to apply all the changes at
            // the AST level and refresh the bindings
            resetParser(compilationUnit, parser, javaSERelease);
            astRoot = (CompilationUnit) parser.createAST(null);
            ++totalNbLoops;


            final List<ASTVisitor> thisLoopVisitors = refactoring.getVisitorsContributingRefactoring();
            if (!thisLoopVisitors.equals(lastLoopVisitors)) {
                lastLoopVisitors = new ArrayList<ASTVisitor>(thisLoopVisitors);
                nbLoopsWithSameVisitors = 0;
            } else {
                ++nbLoopsWithSameVisitors;
            }
        }
    }

    private static void resetParser(ICompilationUnit cu, ASTParser parser, Release javaSERelease) {
        parser.setSource(cu);
        parser.setResolveBindings(true);
        parser.setCompilerOptions(getCompilerOptions(javaSERelease));
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> getCompilerOptions(Release javaSERelease) {
        final Map<String, String> options = JavaCore.getOptions();
        final String v = javaSERelease.getMajorVersion() + "." + javaSERelease.getMinorVersion();
        JavaCore.setComplianceOptions(v, options);
        return options;
    }

    private String getPossibleCulprits(int nbLoopsWithSameVisitors, List<ASTVisitor> lastLoopVisitors) {
        if (nbLoopsWithSameVisitors < 100 || lastLoopVisitors.isEmpty()) {
            return "";
        }
        final StringBuilder sb = new StringBuilder(" Possible culprit ASTVisitor classes are: ");
        final Iterator<ASTVisitor> iter = lastLoopVisitors.iterator();
        sb.append(iter.next().getClass().getName());
        while (iter.hasNext()) {
            sb.append(", ").append(iter.next().getClass().getName());
        }
        return sb.toString();
    }
}
