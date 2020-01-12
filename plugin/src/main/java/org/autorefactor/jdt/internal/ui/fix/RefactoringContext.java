/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.environment.Environment;
import org.autorefactor.environment.Logger;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.JavaProjectOptions;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.SourceLocation;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;

/** Class holding necessary data for a refactoring. */
public class RefactoringContext {
    private final ICompilationUnit compilationUnit;
    private final CompilationUnit astRoot;
    private final Refactorings refactorings;
    private final ASTNodeFactory astBuilder;
    private final JavaProjectOptions options;
    private final SubMonitor monitor;
    private final Environment environment;

    /**
     * Builds an instance of this class.
     *
     * @param compilationUnit the compilation unit to refactor
     * @param astRoot         the compilation unit, root of the AST
     * @param options         the Java project options used to compile the project
     * @param monitor         the progress monitor of the current job
     * @param environment     the environment
     */
    public RefactoringContext(final ICompilationUnit compilationUnit, final CompilationUnit astRoot, final JavaProjectOptions options,
            final SubMonitor monitor, final Environment environment) {
        this.compilationUnit= compilationUnit;
        this.astRoot= astRoot;
        this.monitor= monitor;
        this.environment= environment;
        this.refactorings= new Refactorings(astRoot, environment.getEventLoop(), monitor);
        this.astBuilder= new ASTNodeFactory(refactorings);
        this.options= options;
    }

    /**
     * Returns the {@link AST} object to use in the cleanup.
     *
     * @return the {@link AST} object to use in the cleanup
     */
    public AST getAST() {
        return refactorings.getAST();
    }

    /**
     * Returns a new {@link ASTNodeFactory} object to use in the cleanup.
     *
     * @return a new {@link ASTNodeFactory} object to use in the cleanup
     */
    public ASTNodeFactory getASTBuilder() {
        return astBuilder;
    }

    /**
     * Returns the compilation unit to refactor.
     *
     * @return the compilation unit to refactor
     */
    public ICompilationUnit getCompilationUnit() {
        return compilationUnit;
    }

    /**
     * Returns the Java project options used to compile the project.
     *
     * @return the Java project options used to compile the project
     */
    public JavaProjectOptions getJavaProjectOptions() {
        return options;
    }

    /**
     * Returns the progress monitor of the current job.
     *
     * @return the progress monitor of the current job
     */
    public SubMonitor getProgressMonitor() {
        return monitor;
    }

    /**
     * Returns the {@link Refactorings} object containing the changes that must be
     * applied to the AST.
     *
     * @return the {@link Refactorings} object containing the changes that must be
     *         applied to the AST
     */
    public Refactorings getRefactorings() {
        return refactorings;
    }

    String getSource(final ASTNode node) {
        try {
            return compilationUnit.getSource();
        } catch (JavaModelException e) {
            throw new UnhandledException(node, e);
        }
    }

    boolean isInComment(final int position) {
        for (Comment comment : ASTNodes.getCommentList(astRoot)) {
            if (comment.getStartPosition() <= position && position <= SourceLocation.getEndPosition(comment)) {
                return true;
            }
            if (position < comment.getStartPosition()) {
                // Since comment list is "arranged in order of increasing source position"
                // it is impossible for this position to be surrounded by a comment
                return false;
            }
        }

        return false;
    }

    /**
     * Returns the logger.
     *
     * @return the logger
     */
    public Logger getLogger() {
        return environment.getLogger();
    }
}
