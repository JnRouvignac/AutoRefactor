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
import java.util.Map.Entry;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.refactoring.ASTCommentRewriter;
import org.autorefactor.refactoring.IRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Refactorings.Insert;
import org.autorefactor.refactoring.Refactorings.InsertType;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.rules.AggregateASTVisitor;
import org.autorefactor.refactoring.rules.RefactoringContext;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
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
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.TextEdit;

import static org.eclipse.jdt.core.JavaCore.*;
import static org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants.*;

/**
 * TODO JNR keep track of the job so it can be cancelled by the plugin on workspace exit.
 */
public class ApplyRefactoringsJob extends Job {

    private final IJavaElement javaElement;
    private final List<IRefactoring> refactoringsToApply;

    public ApplyRefactoringsJob(IJavaElement javaElement, List<IRefactoring> refactoringsToApply) {
        super("Auto Refactor");
        this.javaElement = javaElement;
        this.refactoringsToApply = refactoringsToApply;
    }

    /** {@inheritDoc} */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        if (javaElement == null) {
            // No java project exists.
            return Status.OK_STATUS;
        }
        final List<ICompilationUnit> compilationUnits = collectCompilationUnits(javaElement);
        final Map<String, String> options = getJavaProjectOptions(javaElement);
        final String javaSourceCompatibility = options.get(COMPILER_SOURCE);
        final int tabSize = getTabSize(options);

        monitor.beginTask("", compilationUnits.size());
        try {
            final Release javaSERelease = Release.javaSE(javaSourceCompatibility);
            for (final ICompilationUnit compilationUnit : compilationUnits) {
                try {
                    final String elName = compilationUnit.getElementName();
                    final String simpleName = elName.substring(0, elName.lastIndexOf('.'));
                    final String className =
                        compilationUnit.getParent().getElementName() + "." + simpleName;

                    monitor.subTask("Applying refactorings to " + className);

                    AggregateASTVisitor refactoring = new AggregateASTVisitor(
                        refactoringsToApply, AutoRefactorPlugin.getPreferenceHelper().debugModeOn());
                    applyRefactoring(compilationUnit, javaSERelease, tabSize, refactoring);
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

    private int getTabSize(final Map<String, String> options) {
        String tabSize = options.get(FORMATTER_INDENTATION_SIZE);
        try {
            return Integer.valueOf(tabSize);
        } catch (NumberFormatException e) {
            throw new UnhandledException(e);
        }
    }

    private List<ICompilationUnit> collectCompilationUnits(IJavaElement javaElement) {
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

    private void addAll(final List<ICompilationUnit> results,
            ICompilationUnit[] compilationUnits) throws JavaModelException {
        for (ICompilationUnit cu : compilationUnits) {
            add(results, cu);
        }
    }

    private void add(final List<ICompilationUnit> results,
            ICompilationUnit cu) throws JavaModelException {
        if (!cu.isConsistent()) {
            cu.makeConsistent(null);
        }
        if (!cu.isReadOnly()) {
            results.add(cu);
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, String> getJavaProjectOptions(IJavaElement javaElement) {
        final IJavaProject javaProject = getIJavaProject(javaElement);
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

    /**
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
    private void applyRefactoring(ICompilationUnit compilationUnit, Release javaSERelease, int tabSize,
            AggregateASTVisitor refactoringToApply) throws Exception {
        final ITextFileBufferManager bufferManager = FileBuffers.getTextFileBufferManager();
        final IPath path = compilationUnit.getPath();
        final LocationKind locationKind = LocationKind.NORMALIZE;
        try {
            bufferManager.connect(path, locationKind, null);
            final ITextFileBuffer textFileBuffer = bufferManager
                    .getTextFileBuffer(path, locationKind);
            final IDocument document = textFileBuffer.getDocument();
            applyRefactoring(document, compilationUnit,
                    javaSERelease, tabSize, refactoringToApply);
            textFileBuffer.commit(null, false);
        } finally {
            bufferManager.disconnect(path, locationKind, null);
        }
    }

    public void applyRefactoring(IDocument document, ICompilationUnit compilationUnit, Release javaSERelease,
            int tabSize, AggregateASTVisitor refactoring) throws JavaModelException {
        // creation of DOM/AST from a ICompilationUnit
        final ASTParser parser = ASTParser.newParser(AST.JLS4);
        resetParser(compilationUnit, parser, javaSERelease);

        CompilationUnit astRoot = (CompilationUnit) parser.createAST(null);

        int totalNbLoops = 0;
        List<ASTVisitor> lastLoopVisitors = Collections.emptyList();
        int nbLoopsWithSameVisitors = 0;
        while (true) {
            if (totalNbLoops > 10000) {
                // Oops! Something went wrong.
                final String message = getPossibleCulprits(nbLoopsWithSameVisitors, lastLoopVisitors);
                throw new IllegalStateException("An infinite loop has been detected."
                        + " A possible cause is that code is being incorrectly"
                        + " refactored one way then refactored back to what it was."
                        + message);
            }

            try {
                final RefactoringContext ctx = new RefactoringContext(compilationUnit,
                        astRoot.getAST(), javaSERelease);
                refactoring.setRefactoringContext(ctx);

                final Refactorings refactorings = refactoring.getRefactorings(astRoot);
                if (!refactorings.hasRefactorings()) {
                    // no new refactorings have been applied,
                    // we are done with applying the refactorings.
                    return;
                }

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
            } catch (Exception e) {
                // TODO JNR add UI error reporting with Display.getCurrent().asyncExec()
                throw new RuntimeException("Unexpected exception", e);
            }
        }
    }

    private static void resetParser(ICompilationUnit compilationUnit,
            ASTParser parser, Release javaSERelease) {
        parser.setSource(compilationUnit);
        parser.setResolveBindings(true);
        parser.setCompilerOptions(getCompilerOptions(javaSERelease));
    }

    @SuppressWarnings("unchecked")
    public static Map<String, String> getCompilerOptions(Release javaSERelease) {
        final Map<String, String> options = JavaCore.getOptions();
        final String v = javaSERelease.getMajorVersion() + "." + javaSERelease.getMinorVersion();
        JavaCore.setComplianceOptions(v, options);
        return options;
    }

    private String getPossibleCulprits(int nbLoopsWithSameVisitors,
            List<ASTVisitor> lastLoopVisitors) {
        if (nbLoopsWithSameVisitors < 100 || lastLoopVisitors.isEmpty()) {
            return "";
        }
        final StringBuilder sb = new StringBuilder(" Possible culprit ASTVisitor classes are: ");
        final Iterator<ASTVisitor> iter = lastLoopVisitors.iterator();
        sb.append(iter.next().getClass().getName());
        for (; iter.hasNext();) {
            sb.append(", ").append(iter.next().getClass().getName());
        }
        return sb.toString();
    }

    private Pair<ASTRewrite, ASTCommentRewriter> getASTRewrite(
            final CompilationUnit astRoot, final Refactorings refactorings) {
        final ASTRewrite rewrite = ASTRewrite.create(astRoot.getAST());
        if (!refactorings.getInserts().isEmpty()) {
            for (Entry<ChildListPropertyDescriptor, List<Insert>> entry : refactorings
                    .getInserts().entrySet()) {
                for (final Insert insert : entry.getValue()) {
                    final ListRewrite listRewrite = rewrite.getListRewrite(
                            insert.getListHolder(), entry.getKey());
                    if (InsertType.AT_INDEX.equals(insert.getInsertType())) {
                        listRewrite.insertAt(insert.getNodeToInsert(),
                                insert.getIndex(), null);
                    } else if (InsertType.BEFORE.equals(insert.getInsertType())) {
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
        for (Comment toRemove : refactorings.getCommentRemovals()) {
            commentRewriter.remove(toRemove);
        }
        for (Pair<Comment, String> toReplace : refactorings.getCommentReplacements()) {
            commentRewriter.replace(toReplace.getFirst(), toReplace.getSecond());
        }
        for (BlockComment toJavadoc : refactorings.getBlockCommentToJavadoc()) {
            commentRewriter.toJavadoc(toJavadoc);
        }
        for (List<LineComment> toJavadoc : refactorings.getLineCommentsToJavadoc()) {
            commentRewriter.toJavadoc(toJavadoc);
        }
        return Pair.of(rewrite, commentRewriter);
    }
}
