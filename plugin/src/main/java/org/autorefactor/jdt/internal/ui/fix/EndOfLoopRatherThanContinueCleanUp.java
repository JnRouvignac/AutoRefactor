/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class EndOfLoopRatherThanContinueCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_EndOfLoopRatherThanContinueCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_EndOfLoopRatherThanContinueCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_EndOfLoopRatherThanContinueCleanUp_reason;
	}

	@Override
	public boolean visit(final ContinueStatement node) {
		if (node.getLabel() == null && isLastStatementInLoop(node)) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.CleanUpRefactoringWizard_EndOfLoopRatherThanContinueCleanUp_name);

			if (ASTNodes.canHaveSiblings(node) || node.getLocationInParent() == IfStatement.ELSE_STATEMENT_PROPERTY) {
				rewrite.remove(node, group);
			} else {
				rewrite.replace(node, cuRewrite.getASTBuilder().block(), group);
			}

			return false;
		}

		return true;
	}

	private boolean isLastStatementInLoop(final Statement node) {
		Statement nextStatement= ASTNodes.getNextStatement(node);

		if (nextStatement == null) {
			if (node.getParent() instanceof WhileStatement || node.getParent() instanceof DoStatement
					|| node.getParent() instanceof ForStatement || node.getParent() instanceof EnhancedForStatement) {
				return true;
			}
			if (node.getParent() instanceof MethodDeclaration) {
				return false;
			}
			if (node.getParent() instanceof Statement) {
				return isLastStatementInLoop((Statement) node.getParent());
			}
		}

		return false;
	}
}
