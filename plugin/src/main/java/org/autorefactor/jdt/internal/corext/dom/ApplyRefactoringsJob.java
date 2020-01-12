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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import org.autorefactor.environment.Environment;
import org.autorefactor.jdt.internal.ui.fix.AggregateASTVisitor;
import org.autorefactor.jdt.internal.ui.fix.RefactoringContext;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.filebuffers.FileBuffers;
import org.eclipse.core.filebuffers.ITextFileBuffer;
import org.eclipse.core.filebuffers.ITextFileBufferManager;
import org.eclipse.core.filebuffers.LocationKind;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.TextEdit;

/**
 * Eclipse job that applies the provided cleanup rules in background.
 * Several such jobs might be started and run in parallel to form a worker pool,
 * with all workers accepting work items ({@link RefactoringUnit}) from a queue
 * provided by the partitioner ({@link PrepareApplyRefactoringsJob}).
 */
public class ApplyRefactoringsJob extends AbstractRefactoringJob {
    private final Queue<RefactoringUnit> refactoringUnits;
    private final List<RefactoringRule> refactoringRulesToApply;
    private final Environment environment;

    /**
     * Builds an instance of this class.
     *
     * @param refactoringUnits        the units to automatically refactor
     * @param refactoringRulesToApply the cleanups to apply
     * @param environment             the environment
     */
    public ApplyRefactoringsJob(final Queue<RefactoringUnit> refactoringUnits, final List<RefactoringRule> refactoringRulesToApply,
            final Environment environment) {
        super("AutoRefactor"); //$NON-NLS-1$
        setPriority(Job.LONG);
        this.refactoringUnits= refactoringUnits;
        this.refactoringRulesToApply= refactoringRulesToApply;
        this.environment= environment;
    }

    @Override
    protected IStatus run(final IProgressMonitor monitor) {
        try {
            return run0(monitor);
        } catch (OperationCanceledException e) {
            throw e;
        } catch (Exception e) {
            final String msg= "Error while applying refactorings.\n\n" //$NON-NLS-1$
                    + "Please look at the Eclipse workspace logs and " //$NON-NLS-1$
                    + "report the stacktrace to the AutoRefactor project.\n" //$NON-NLS-1$
                    + "Please provide sample java code that triggers the error.\n\n"; //$NON-NLS-1$
            return new Status(IStatus.ERROR, PluginConstant.PLUGIN_ID, msg, e);
        }
    }

    private IStatus run0(final IProgressMonitor monitor) throws Exception {
        if (refactoringUnits.isEmpty()) {
            // No java project exists.
            return Status.OK_STATUS;
        }

        final SubMonitor loopMonitor= SubMonitor.convert(monitor, refactoringUnits.size());
        try {
            RefactoringUnit toRefactor;
            while ((toRefactor= refactoringUnits.poll()) != null && !loopMonitor.isCanceled()) {
                final ICompilationUnit compilationUnit= toRefactor.getCompilationUnit();
                final JavaProjectOptions options= toRefactor.getOptions();
                try {
                    loopMonitor.subTask("Applying refactorings to " + getClassName(compilationUnit)); //$NON-NLS-1$
                    final AggregateASTVisitor refactoring= new AggregateASTVisitor(refactoringRulesToApply);
                    applyRefactoring(compilationUnit, refactoring, options, loopMonitor.newChild(1), true);
                } catch (OperationCanceledException e) {
                    throw e;
                } catch (Exception e) {
                    final String msg= "Exception when applying refactorings to file \"" + compilationUnit.getPath() //$NON-NLS-1$
                            + "\": " + e.getMessage(); //$NON-NLS-1$
                    throw new UnhandledException(null, msg, e);
                }
            }
        } finally {
            loopMonitor.done();
        }

        return Status.OK_STATUS;
    }

    private String getClassName(final ICompilationUnit compilationUnit) {
        final String elName= compilationUnit.getElementName();
        final String simpleName= elName.substring(0, elName.lastIndexOf('.'));
        return compilationUnit.getParent().getElementName() + "." + simpleName; //$NON-NLS-1$
    }

    /**
     * Applies the cleanups provided inside the {@link AggregateASTVisitor} to
     * the provided {@link ICompilationUnit}.
     *
     * @param compilationUnit    the compilation unit to refactor
     * @param refactoringToApply the {@link AggregateASTVisitor} to apply to the
     *                           compilation unit
     * @param options            the Java project options used to compile the
     *                           project
     * @param monitor            the progress monitor of the current job
     * @param hasToSave          hasToSave
     *
     * @return TextEdit
     *
     * @throws Exception if any problem occurs
     */
    public List<TextEdit> applyRefactoring(final ICompilationUnit compilationUnit, final AggregateASTVisitor refactoringToApply,
            final JavaProjectOptions options, final SubMonitor monitor, final boolean hasToSave) throws Exception {
        final ITextFileBufferManager bufferManager= FileBuffers.getTextFileBufferManager();
        final IPath path= compilationUnit.getPath();
        final LocationKind locationKind= LocationKind.NORMALIZE;
        List<TextEdit> textEdits= null;
        try {
            bufferManager.connect(path, locationKind, null);
            final ITextFileBuffer textFileBuffer= bufferManager.getTextFileBuffer(path, locationKind);
            if (!textFileBuffer.isSynchronized()) {
                /*
                 * Cannot read the source when a file is not synchronized, Let's ignore this
                 * file to avoid problems when: - doing string manipulation with the source text
                 * - applying automated cleanups to such files
                 */
                environment.getLogger()
                        .error("File \"" + compilationUnit.getPath() + "\" is not synchronized with the file system." //$NON-NLS-1$ //$NON-NLS-2$
                                + " Automated refactorings will not be applied to it."); //$NON-NLS-1$
                return null;
            }
            final IDocument document= textFileBuffer.getDocument();
            textEdits= applyRefactoring(document, compilationUnit, refactoringToApply, options, monitor, hasToSave);
        } finally {
            bufferManager.disconnect(path, locationKind, null);
        }

        return textEdits;
    }

    /**
     * Applies the cleanups provided inside the {@link AggregateASTVisitor} to
     * the provided {@link ICompilationUnit}.
     *
     * @param document        the document where the compilation unit comes from
     * @param compilationUnit the compilation unit to refactor
     * @param refactoring     the {@link AggregateASTVisitor} to apply to the
     *                        compilation unit
     * @param options         the Java project options used to compile the project
     * @param monitor         the progress monitor of the current job
     * @param hasToSave       hasToSave
     * @return TextEdit
     * @throws Exception if any problem occurs
     *
     * @see <a href=
     *      "http://help.eclipse.org/indigo/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Fguide%2Fjdt_api_manip.htm"
     *      >Eclipse JDT core - Manipulating Java code</a>
     * @see <a href="
     *      http://help.eclipse.org/indigo/index.jsp?topic=/org.eclipse.platform.doc.isv/guide/workbench_cmd_menus.htm"
     *      > Eclipse Platform Plug-in Developer Guide > Plugging into the workbench
     *      > Basic workbench extension points using commands >
     *      org.eclipse.ui.menus</a>
     * @see <a href=
     *      "http://www.eclipse.org/articles/article.php?file=Article-JavaCodeManipulation_AST/index.html"
     *      >Abstract Syntax Tree > Write it down</a>
     */
    public List<TextEdit> applyRefactoring(final IDocument document, final ICompilationUnit compilationUnit,
            final AggregateASTVisitor refactoring, final JavaProjectOptions options, final SubMonitor monitor, final boolean hasToSave)
            throws Exception {
        // Creation of DOM/AST from a ICompilationUnit
        final ASTParser parser= ASTParser.newParser(AST.JLS8);

        final int maxIterations= 100;
        int iterationCount= 0;
        Set<ASTVisitor> lastLoopVisitors= Collections.emptySet();
        int nbLoopsWithSameVisitors= 0;

        List<TextEdit> textEdits= new ArrayList<>();

        monitor.setWorkRemaining(maxIterations);

        CompilationUnit astRoot;
        do {
            // I did not find any other way to directly modify the AST
            // while still keeping the resolved type bindings working.
            // Using astRoot.recordModifications() did not work:
            // type bindings were lost. Is there a way to recover them?
            // FIXME we should find a way to apply all the changes at
            // the AST level and refresh the bindings
            resetParser(compilationUnit, parser, options);
            astRoot= (CompilationUnit) parser.createAST(null);

            if (iterationCount > maxIterations) {
                // Oops! Something went wrong.
                final String errorMsg= "An infinite loop has been detected for file " + ASTNodes.getFileName(astRoot) + "." //$NON-NLS-1$ //$NON-NLS-2$
                        + " A possible cause is that code is being incorrectly" //$NON-NLS-1$
                        + " refactored one way then refactored back to what it was." + " Fix the code before pursuing." //$NON-NLS-1$ //$NON-NLS-2$
                        + getPossibleCulprits(nbLoopsWithSameVisitors, lastLoopVisitors);
                environment.getLogger().error(errorMsg, new IllegalStateException(astRoot, errorMsg));
                break;
            }

            final RefactoringContext ctx= new RefactoringContext(compilationUnit, astRoot, options, monitor,
                    environment);
            refactoring.setRefactoringContext(ctx);

            final Refactorings refactorings= refactoring.getRefactorings(astRoot);
            if (!refactorings.hasRefactorings()) {
                // No new cleanups have been applied,
                // We are done with applying the cleanups.
                break;
            }

            // Apply the cleanups and save the compilation unit
            refactorings.applyTo(document, hasToSave);
            textEdits.add(refactorings.getEdits());
            if (!hasToSave) {
                return textEdits;
            }
            final boolean hadUnsavedChanges= compilationUnit.hasUnsavedChanges();
            compilationUnit.getBuffer().setContents(document.get());
            // http://wiki.eclipse.org/FAQ_What_is_a_working_copy%3F
            // compilationUnit.reconcile(AST.JLS8,
            // ICompilationUnit.ENABLE_BINDINGS_RECOVERY |
            // ICompilationUnit.ENABLE_STATEMENTS_RECOVERY |
            // ICompilationUnit.FORCE_PROBLEM_DETECTION
            // /** can be useful to back out a change that does not compile */
            // , null, null);
            if (!hadUnsavedChanges && hasToSave) {
                compilationUnit.save(null, true);
            }
            iterationCount++;

            final Set<ASTVisitor> thisLoopVisitors= refactoring.getVisitorsContributingRefactoring();
            if (thisLoopVisitors.equals(lastLoopVisitors)) {
                nbLoopsWithSameVisitors++;
            } else {
                lastLoopVisitors= new HashSet<>(thisLoopVisitors);
                nbLoopsWithSameVisitors= 0;
            }
        } while (true);

        return textEdits;
    }

    private static void resetParser(final ICompilationUnit cu, final ASTParser parser, final JavaProjectOptions options) {
        parser.setSource(cu);
        parser.setResolveBindings(true);
        parser.setCompilerOptions(options.getCompilerOptions());
    }

    private String getPossibleCulprits(final int nbLoopsWithSameVisitors, final Set<ASTVisitor> lastLoopVisitors) {
        if (nbLoopsWithSameVisitors < 100 || lastLoopVisitors.isEmpty()) {
            return ""; //$NON-NLS-1$
        }
        final StringBuilder sb= new StringBuilder(" Possible culprit ASTVisitor classes are: "); //$NON-NLS-1$
        final Iterator<ASTVisitor> iter= lastLoopVisitors.iterator();
        sb.append(iter.next().getClass().getName());
        while (iter.hasNext()) {
            sb.append(", ").append(iter.next().getClass().getName()); //$NON-NLS-1$
        }

        return sb.toString();
    }
}
