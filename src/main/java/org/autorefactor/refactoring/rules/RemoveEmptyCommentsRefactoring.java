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
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.LineComment;

/**
 * Remove all empty comments.
 */
public class RemoveEmptyCommentsRefactoring extends ASTVisitor implements
		IJavaRefactoring {

	private RefactoringContext ctx;

	public RemoveEmptyCommentsRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	// TODO also remove commented out code

	@Override
	public boolean visit(BlockComment node) {
		// TODO, this is just a placeholder, how to read the comment's text?
		// node.getStartPosition();
		// node.getLength();
		return super.visit(node);
	}

	@Override
	public boolean visit(Javadoc node) {
		if (node.tags().size() == 0) {
			this.ctx.getRefactorings().remove(node);
		}
		return super.visit(node);
	}

	@Override
	public boolean visit(LineComment node) {
		// TODO, this is just a placeholder, how to read the comment's text?
		// node.getStartPosition();
		// node.getLength();
		return super.visit(node);
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		for (Comment comment : (List<Comment>) astRoot.getCommentList()) {
			if (comment.isBlockComment()) {
				final BlockComment bc = (BlockComment) comment;
				bc.accept(this);
			} else if (comment.isLineComment()) {
				final LineComment lc = (LineComment) comment;
				lc.accept(this);
			} else if (comment.isDocComment()) {
				final Javadoc jc = (Javadoc) comment;
				jc.accept(this);
			}
		}
		return this.ctx.getRefactorings();
	}
}
