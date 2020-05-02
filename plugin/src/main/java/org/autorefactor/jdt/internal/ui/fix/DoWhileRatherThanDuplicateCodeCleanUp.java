/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class DoWhileRatherThanDuplicateCodeCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_DoWhileRatherThanDuplicateCodeCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_DoWhileRatherThanDuplicateCodeCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_DoWhileRatherThanDuplicateCodeCleanUp_reason;
	}

	@Override
	public boolean visit(final WhileStatement node) {
		List<Statement> whileStatements= ASTNodes.asList(node.getBody());

		if (whileStatements.isEmpty()) {
			return true;
		}

		List<Statement> previousStatements= new ArrayList<>(whileStatements.size());

		Statement previousStatement= ASTNodes.getPreviousSibling(node);
		int i= whileStatements.size() - 1;
		while (i >= 0) {
			if (previousStatement == null || !ASTNodes.match(previousStatement, whileStatements.get(i))) {
				return true;
			}
			i--;
			previousStatements.add(previousStatement);
			previousStatement= ASTNodes.getPreviousSibling(previousStatement);
		}

		replaceWithDoWhile(node, previousStatements);
		return false;
	}

	private void replaceWithDoWhile(final WhileStatement node, final List<Statement> previousStatements) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		rewrite.remove(previousStatements, null);

		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		rewrite.replace(node, ast.doWhile(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(node.getExpression())), ASTNodes.createMoveTarget(rewrite, node.getBody())), null);
	}
}
