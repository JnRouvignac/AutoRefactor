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

import org.autorefactor.jdt.internal.corext.dom.ASTComments;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.SourceLocation;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class RemoveUselessBlockCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveUselessBlockCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveUselessBlockCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_RemoveUselessBlockCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		@SuppressWarnings("unchecked")
		List<Statement> statements= node.statements();

		if (statements.size() == 1 && statements.get(0) instanceof Block) {
			replaceBlock((Block) statements.get(0));
			return false;
		}

		if (node.getParent() instanceof Block) {
			Set<SimpleName> ifVariableNames= ASTNodes.getLocalVariableIdentifiers(node, false);
			Set<SimpleName> followingVariableNames= new HashSet<>();

			for (Statement statement : ASTNodes.getNextSiblings(node)) {
				followingVariableNames.addAll(ASTNodes.getLocalVariableIdentifiers(statement, true));
			}

			for (SimpleName ifVariableName : ifVariableNames) {
				for (SimpleName followingVariableName : followingVariableNames) {
					if (Utils.equalNotNull(ifVariableName.getIdentifier(), followingVariableName.getIdentifier())) {
						return true;
					}
				}
			}

			replaceBlock(node);
			return false;
		}

		return true;
	}

	@SuppressWarnings("unchecked")
	private void replaceBlock(final Block node) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

        List<Statement> statements= (List<Statement>) node.statements();

        List<String> extraLeadingComments = collectExtraLeadingComments(node, statements);

        for (String comment : extraLeadingComments) {
            rewrite.insertBefore(ast.rawComment(comment), node, null);
        }
        rewrite.replace(node, ast.copyRange(statements), null);
    }

    /**
     * If statements starts on different line than block and comment starts on same line then block
     * the comment has to be copied manually (see #396)
     */
    private List<String> collectExtraLeadingComments(Block node, List<Statement> statements) {
        List<String> comments = new ArrayList<>();

        CompilationUnit cu = node.getRoot() instanceof CompilationUnit ? (CompilationUnit) node.getRoot() : null;
        if (cu != null) {
            int stmtStartPosition = !statements.isEmpty()
                    ? statements.get(0).getStartPosition()
                    : SourceLocation.getEndPosition(node) - 1;
            List<Comment> leadingComments = ASTComments.filterCommentsInRange(
                    node.getStartPosition() + 1,
                    stmtStartPosition, cu);

            try {
                if (!leadingComments.isEmpty()) {
                    int lineNumberBlockStart = cu.getLineNumber(node.getStartPosition());
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
                // should we abort?
            }
        }
        return comments;
	}
}
