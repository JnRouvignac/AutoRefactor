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

import java.util.Map;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.WhileStatement;

/**
 * Replace while/for with iterator/for with index loops into foreach loops.
 * <p>
 * Also Replace Map keys iteration + {@link Map#get(Object)} with iterations
 * over {@link Map#entrySet()}
 * </p>
 */
public class ForeachRefactoring extends ASTVisitor implements IJavaRefactoring {

	private RefactoringContext ctx;

	public ForeachRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	private static class VariableUseVisitor extends ASTVisitor {

		@Override
		public boolean visit(SimpleName node) {
			ASTNode parent = node.getParent();
			if (parent instanceof QualifiedName
					|| parent instanceof FieldAccess) {
				return ASTHelper.DO_NOT_VISIT_SUBTREE;
			}
			return ASTHelper.DO_NOT_VISIT_SUBTREE;
		}
	}

	@Override
	public boolean visit(ForStatement node) {
		final VariableUseVisitor variableUseVisitor = new VariableUseVisitor();
		node.accept(variableUseVisitor);

		if (node.initializers().size() == 1) {
			node.initializers();
		}

		node.getExpression();
		node.updaters();
		node.getBody();
		// TODO JNR iterate over array with index
		// TODO JNR iterate over array with temporary variable with generics
		// TODO JNR iterate over array with temporary variable without generics
		// TODO JNR iterate over col with index
		// TODO JNR iterate over col with temporary variable with generics
		// TODO JNR iterate over col with temporary variable without generics
		// TODO JNR iterate over col with Iterator with generics
		// TODO JNR iterate over col with Iterator without generics
		// TODO JNR iterate over col with ListIterator with generics
		// TODO JNR iterate over col with ListIterator without generics
		// be careful with use of index/iterator inside the loop
		return ASTHelper.VISIT_SUBTREE;
	}

	@Override
	public boolean visit(WhileStatement node) {
		node.getExpression();
		node.getBody();
		// TODO JNR iterate over array with index
		// TODO JNR iterate over array with temporary variable with generics
		// TODO JNR iterate over array with temporary variable without generics
		// TODO JNR iterate over col with index
		// TODO JNR iterate over col with temporary variable with generics
		// TODO JNR iterate over col with temporary variable without generics
		// TODO JNR iterate over col with Iterator with generics
		// TODO JNR iterate over col with Iterator without generics
		// TODO JNR iterate over col with ListIterator with generics
		// TODO JNR iterate over col with ListIterator without generics
		// be careful with use of index/iterator inside the loop
		return ASTHelper.VISIT_SUBTREE;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
