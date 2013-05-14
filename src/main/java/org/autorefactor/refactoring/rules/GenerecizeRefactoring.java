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

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Type;

/**
 * Add generics, be more assertive about generics that what Eclipse does.
 */
public class GenerecizeRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;

	public GenerecizeRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	// TODO JNR where are we doing casts?
	// Generics
	// Collection.iterator
	// List.listIterator
	// List.get
	// List.add
	// Map.entries
	// Map.Entry.getKey
	// Map.Entry.getValue
	// Map.values
	// Map.keys
	// Map.get
	// Map.put
	// Iterator.next
	// Comparator.compareTo
	// Comparable.compareTo
	// Class.newInstance

	// Varargs
	// Arrays.asList remove now useless array creations + add generics to it
	// Method.invoke remove now useless array creation
	// Class.getMethod / Class.getDeclaredMethod remove now useless array
	// creation

	@Override
	public boolean visit(MethodInvocation node) {
		if ("next".equals(node.getName().getIdentifier())
				&& node.arguments().size() == 0
				&& node.getExpression() != null
				&& ASTHelper.instanceOf(node.getExpression()
						.resolveTypeBinding(), "java.util.Iterator")) {
			if (node.getParent() instanceof CastExpression) {
				CastExpression cast = (CastExpression) node.getParent();
				Type type = cast.getType();

				// find variable declaration and include with the type above
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
