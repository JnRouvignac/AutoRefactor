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

import static org.autorefactor.refactoring.ASTHelper.*;

import java.util.List;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.*;

public class IfStatementRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;

	public IfStatementRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
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

	@Override
	public boolean visit(IfStatement node) {
		final Statement elseStmt = node.getElseStatement();
		if (elseStmt instanceof Block) {
			List<Statement> elseStmts = statements((Block) elseStmt);
			if (elseStmts.size() == 1
					&& elseStmts.get(0) instanceof IfStatement) {
				final AST ast = this.ctx.getAST();
				final IfStatement newIS = copySubtree(ast, node);
				newIS.setElseStatement(copySubtree(ast, elseStmts.get(0)));
				this.ctx.getRefactorings().replace(node, newIS);
				return DO_NOT_VISIT_SUBTREE;
			}
		}
		return VISIT_SUBTREE;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
