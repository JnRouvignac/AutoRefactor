/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2018 Jean-Noël Rouvignac - minor changes
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTComments;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.SourceLocation;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class RemoveUselessBlockCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveUselessBlockCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveUselessBlockCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveUselessBlockCleanUp_reason;
	}

	@Override
	public boolean visit(final Block visited) {
		List<Statement> statements= visited.statements();

		if (statements.size() == 1 && statements.get(0) instanceof Block) {
			replaceBlock((Block) statements.get(0));
			return false;
		}

		if (visited.getParent() instanceof Block) {
			Set<SimpleName> ifVariableNames= ASTNodes.getLocalVariableIdentifiers(visited, false);
			Set<SimpleName> followingVariableNames= new HashSet<>();

			for (Statement statement : ASTNodes.getNextSiblings(visited)) {
				followingVariableNames.addAll(ASTNodes.getLocalVariableIdentifiers(statement, true));
			}

			for (SimpleName ifVariableName : ifVariableNames) {
				for (SimpleName followingVariableName : followingVariableNames) {
					if (Utils.equalNotNull(ifVariableName.getIdentifier(), followingVariableName.getIdentifier())) {
						return true;
					}
				}
			}

			replaceBlock(visited);
			return false;
		}

		return true;
	}

	private void replaceBlock(final Block visited) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveUselessBlockCleanUp_description);

		List<Statement> statements= visited.statements();
		List<String> extraLeadingComments = collectExtraLeadingComments(visited, statements);

		for (String comment : extraLeadingComments) {
			rewrite.insertBefore(ast.rawComment(comment), visited, group);
		}

		ASTNodes.replaceButKeepComment(rewrite, visited, ast.copyRange(statements), group);
	}

	/**
	 * If statements starts on different line than block and comment starts on same line then block
	 * the comment has to be copied manually (see #396)
	 */
	private List<String> collectExtraLeadingComments(Block visited, List<Statement> statements) {
		List<String> comments = new ArrayList<>();
		CompilationUnit cu = visited.getRoot() instanceof CompilationUnit ? (CompilationUnit) visited.getRoot() : null;

		if (cu != null) {
			int stmtStartPosition = statements.isEmpty()
					? SourceLocation.getEndPosition(visited) - 1
					: statements.get(0).getStartPosition();
			List<Comment> leadingComments = ASTComments.filterCommentsInRange(
					visited.getStartPosition() + 1,
					stmtStartPosition, cu);

			try {
				if (!leadingComments.isEmpty()) {
					int lineNumberBlockStart = cu.getLineNumber(visited.getStartPosition());
					String source = cu.getTypeRoot().getSource();

					for (Comment comment : leadingComments) {
						String s = source.substring(comment.getStartPosition(), SourceLocation.getEndPosition(comment));
						if (lineNumberBlockStart == cu.getLineNumber(comment.getStartPosition())
								&& lineNumberBlockStart != cu.getLineNumber(stmtStartPosition)) {
							comments.add(s);
						}
					}
				}
			} catch (JavaModelException e) {
				// Should we abort?
			}
		}

		return comments;
	}
}
