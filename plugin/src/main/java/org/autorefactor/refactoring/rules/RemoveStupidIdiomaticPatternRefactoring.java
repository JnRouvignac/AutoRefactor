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

import java.util.List;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * TODO JNR
 *
 * <pre>
 * public myMethod() throws MyException {
 *     int result;
 *     try {
 *         result = someOtherMethod();
 *     } catch (MyException2 e) {
 *         throw new MyException(e);
 *     }
 *     return result;
 * }
 * </pre>
 *
 * should be transformed into:
 *
 * <pre>
 * public myMethod() throws MyException {
 *     try {
 *         return someOtherMethod();
 *     } catch (MyException2 e) {
 *         throw new MyException(e);
 *     }
 * }
 * </pre>
 */
public class RemoveStupidIdiomaticPatternRefactoring extends ASTVisitor
        implements IJavaRefactoring {

    private RefactoringContext ctx;

    /** Default constructor. */
    public RemoveStupidIdiomaticPatternRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    // TODO JNR

    // public Object myMethod() {
    // Object result = null;
    // try {
    // result = myOtherMethod();
    // } catch (Exception e) {
    // throw e;
    // }
    // return result;
    // }

    private Object getModifiersOnly(List modifiers) {
        return null;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}
