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
package org.autorefactor.refactoring.rules;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.JavaProjectOptions;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.UnhandledException;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.SourceLocation.*;

import org.autorefactor.environment.Environment;
import org.autorefactor.environment.Logger;

/** Class holding necessary data for a refactoring. */
public class RefactoringContext {
    private final ICompilationUnit compilationUnit;
    private final CompilationUnit astRoot;
    private final Refactorings refactorings;
    private final ASTBuilder astBuilder;
    private final JavaProjectOptions options;
    private final SubMonitor monitor;
    private final Environment environment;

    /**
     * Builds an instance of this class.
     *
     * @param compilationUnit the compilation unit to refactor
     * @param astRoot the compilation unit, root of the AST
     * @param options the Java project options used to compile the project
     * @param monitor the progress monitor of the current job
     * @param environment the environment
     */
    public RefactoringContext(ICompilationUnit compilationUnit, CompilationUnit astRoot,
            JavaProjectOptions options, SubMonitor monitor, Environment environment) {
        this.compilationUnit = compilationUnit;
        this.astRoot = astRoot;
        this.monitor = monitor;
        this.environment = environment;
        this.refactorings = new Refactorings(astRoot, environment.getEventLoop());
        this.astBuilder = new ASTBuilder(refactorings);
        this.options = options;
    }

    /**
     * Returns the {@link AST} object to use in the refactoring.
     *
     * @return the {@link AST} object to use in the refactoring
     */
    public AST getAST() {
        return refactorings.getAST();
    }

    /**
     * Returns a new {@link ASTBuilder} object to use in the refactoring.
     *
     * @return a new {@link ASTBuilder} object to use in the refactoring
     */
    public ASTBuilder getASTBuilder() {
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
     * Returns the {@link Refactorings} object containing the changes that must be applied to the AST.
     *
     * @return the {@link Refactorings} object containing the changes that must be applied to the AST
     */
    public Refactorings getRefactorings() {
        return refactorings;
    }

    String getSource(ASTNode node) {
        try {
            return compilationUnit.getSource();
        } catch (JavaModelException e) {
            throw new UnhandledException(node, e);
        }
    }

    boolean isInComment(int position) {
        for (Comment comment : getCommentList(astRoot)) {
            if (comment.getStartPosition() <= position && position <= getEndPosition(comment)) {
                return true;
            } else if (position < comment.getStartPosition()) {
                // since comment list is "arranged in order of increasing source position"
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
