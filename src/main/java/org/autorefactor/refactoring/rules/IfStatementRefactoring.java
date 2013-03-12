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

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * TODO JNR 
 */
public class IfStatementRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private final Refactorings refactorings = new Refactorings();
	private AST ast;
	private Release javaSERelease;

	public IfStatementRefactoring() {
		super();
	}

	public void setAST(final AST ast) {
		this.ast = ast;
	}

	public void setJavaSERelease(Release javaSERelease) {
		this.javaSERelease = javaSERelease;
	}

	// TODO JNR

	// RemoveUselessElseRefactoring
	// if (true) {
	// return s;
	// } else {
	// return null;
	// }

	// UseIfElseIfRefactoring
	// if (b) {
	// return i;
	// }
	// if (c) {
	// return j;
	// }
	// if (d) {
	// return k;
	// }
	// return l;

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.refactorings;
	}
}
