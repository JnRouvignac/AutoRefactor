/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.RefactoringRule;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.rules.AggregateASTVisitor;
import org.autorefactor.refactoring.rules.RefactoringContext;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.OnEclipseVersionUpgrade;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBufferManager;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.text.IDocument;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Eclipse job that applies the provided refactoring rules in background.
 * Several such jobs might be started and run in parallel to form a worker pool,
 * with all workers accepting work items ({@link RefactoringUnit}) from a queue provided by the partitioner
 * ({@link PrepareApplyRefactoringsJob}).
 */
public class ApplyRefactoringsJob extends Job {
    private final Queue<RefactoringUnit> refactoringUnits;
    private final List<RefactoringRule> refactoringRulesToApply;

    /**
     * Builds an instance of this class.
     *
     * @param refactoringUnits the units to automatically refactor
     * @param refactoringRulesToApply the refactorings to apply
     */
    public ApplyRefactoringsJob(
            Queue<RefactoringUnit> refactoringUnits, List<RefactoringRule> refactoringRulesToApply) {
        super("Auto Refactor");
        setPriority(Job.LONG);
        this.refactoringUnits = refactoringUnits;
        this.refactoringRulesToApply = refactoringRulesToApply;
    }

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

    @OnEclipseVersionUpgrade({
            "Remove the check to monitor.isCanceled()",
            "Replace monitor.newChild(1) by monitor.split(1)" })
    private IStatus run0(IProgressMonitor monitor) throws Exception {
        if (refactoringUnits.isEmpty()) {
            // No java project exists.
            return Status.OK_STATUS;
        }

        final SubMonitor loopMonitor = SubMonitor.convert(monitor, refactoringUnits.size());
        try {
            RefactoringUnit toRefactor;
            while ((toRefactor = refactoringUnits.poll()) != null) {
                if (loopMonitor.isCanceled()) {
                    return Status.CANCEL_STATUS;
                }

                final ICompilationUnit compilationUnit = toRefactor.getCompilationUnit();
                final JavaProjectOptions options = toRefactor.getOptions();
                try {
                    loopMonitor.subTask("Applying refactorings to " + getClassName(compilationUnit));
                    final AggregateASTVisitor refactoring = new AggregateASTVisitor(refactoringRulesToApply);
                    applyRefactoring(compilationUnit, refactoring, options, loopMonitor.newChild(1));
                } catch (Exception e) {
                    final String msg = "Exception when applying refactorings to file \""
                            + compilationUnit.getPath() + "\": " + e.getMessage();
                    throw new UnhandledException(null, msg, e);
                }
            }
        } finally {
            loopMonitor.done();
        }
        return Status.OK_STATUS;
    }

    private String getClassName(final ICompilationUnit compilationUnit) {
        final String elName = compilationUnit.getElementName();
        final String simpleName = elName.substring(0, elName.lastIndexOf('.'));
        return compilationUnit.getParent().getElementName() + "." + simpleName;
    }

    private void applyRefactoring(ICompilationUnit compilationUnit, AggregateASTVisitor refactoringToApply,
            JavaProjectOptions options, IProgressMonitor monitor) throws Exception {
        final ITextFileBufferManager bufferManager = FileBuffers.getTextFileBufferManager();
        final IPath path = compilationUnit.getPath();
        final LocationKind locationKind = LocationKind.NORMALIZE;
        try {
            bufferManager.connect(path, locationKind, null);
            final ITextFileBuffer textFileBuffer = bufferManager.getTextFileBuffer(path, locationKind);
            if (!textFileBuffer.isSynchronized()) {
                /*
                 * Cannot read the source when a file is not synchronized,
                 * Let's ignore this file to avoid problems when:
                 * - doing string manipulation with the source text
                 * - applying automated refactorings to such files
                 */
                AutoRefactorPlugin.logError(
                    "File \"" + compilationUnit.getPath() + "\" is not synchronized with the file system."
                        + " Automated refactorings will not be applied to it.");
                return;
            }
            final IDocument document = textFileBuffer.getDocument();
            applyRefactoring(document, compilationUnit, refactoringToApply, options, monitor);
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
     * @param refactoring the {@link AggregateASTVisitor} to apply to the compilation unit
     * @param options the Java project options used to compile the project
     * @param monitor the progress monitor of the current job
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
    public void applyRefactoring(IDocument document, ICompilationUnit compilationUnit, AggregateASTVisitor refactoring,
            JavaProjectOptions options, IProgressMonitor monitor) throws Exception {
        // creation of DOM/AST from a ICompilationUnit
        final ASTParser parser = ASTParser.newParser(AST.JLS4);
        resetParser(compilationUnit, parser, options);

        CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);

        int totalNbLoops = 0;
        Set<ASTVisitor> lastLoopVisitors = Collections.emptySet();
        int nbLoopsWithSameVisitors = 0;
        while (true) {
            if (totalNbLoops > 100) {
                // Oops! Something went wrong.
                final String errorMsg = "An infinite loop has been detected for file "
                        + getFileName(astRoot) + "."
                        + " A possible cause is that code is being incorrectly"
                        + " refactored one way then refactored back to what it was."
                        + " Fix the code before pursuing."
                        + getPossibleCulprits(nbLoopsWithSameVisitors, lastLoopVisitors);
                AutoRefactorPlugin.logError(errorMsg, new IllegalStateException(astRoot, errorMsg));
                break;
            }

            final RefactoringContext ctx = new RefactoringContext(compilationUnit, astRoot, options, monitor);
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
            resetParser(compilationUnit, parser, options);
            astRoot = (CompilationUnit) parser.createAST(null);
            ++totalNbLoops;

            final Set<ASTVisitor> thisLoopVisitors = refactoring.getVisitorsContributingRefactoring();
            if (!thisLoopVisitors.equals(lastLoopVisitors)) {
                lastLoopVisitors = new HashSet<ASTVisitor>(thisLoopVisitors);
                nbLoopsWithSameVisitors = 0;
            } else {
                ++nbLoopsWithSameVisitors;
            }
        }
    }

    private static void resetParser(ICompilationUnit cu, ASTParser parser, JavaProjectOptions options) {
        parser.setSource(cu);
        parser.setResolveBindings(true);
        parser.setCompilerOptions(options.getCompilerOptions());
    }

    private String getPossibleCulprits(int nbLoopsWithSameVisitors, Set<ASTVisitor> lastLoopVisitors) {
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
