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
package org.autorefactor.refactoring.rules;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.dom.AST;

/**
 * Class holding necessary data for a refactoring.
 */
public class RefactoringContext {

    private final Refactorings refactorings;
    private final ICompilationUnit compilationUnit;
    private final Release javaSERelease;

    /**
     * Builds an instance of this class.
     *
     * @param compilationUnit the compilation unit to refactor
     * @param ast the {@link AST} object to use in the refactoring
     * @param javaSERelease the Java SE version that is used to compile the compilation unit
     */
    public RefactoringContext(ICompilationUnit compilationUnit, AST ast,
            Release javaSERelease) {
        this.refactorings = new Refactorings(ast);
        this.javaSERelease = javaSERelease;
        this.compilationUnit = compilationUnit;
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
        return new ASTBuilder(refactorings.getAST());
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
     * Returns the Java SE version that is used to compile the compilation unit.
     *
     * @return the Java SE version that is used to compile the compilation unit
     */
    public Release getJavaSERelease() {
        return javaSERelease;
    }

    /**
     * Returns the {@link Refactorings} object containing the changes that must be applied to the AST.
     *
     * @return the {@link Refactorings} object containing the changes that must be applied to the AST
     */
    public Refactorings getRefactorings() {
        return refactorings;
    }

}
