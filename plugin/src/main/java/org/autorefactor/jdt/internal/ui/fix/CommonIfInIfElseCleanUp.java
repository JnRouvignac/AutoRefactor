/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Avoid to break the workflow
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
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.IfStatement;

/**
 * Refactors:
 *
 * <pre>
 * if (a) {
 *   if (moveAroundIfElse()) {
 *     ...
 *   }
 * } else {
 *   if (moveAroundIfElse()) {
 *     ...
 *   }
 * }
 * </pre>
 *
 * into
 *
 * <pre>
 * if (moveAroundIfElse()) {
 *   if (a) {
 *     ...
 *   } else {
 *     ...
 *   }
 * }
 * </pre>
 *
 * @see #getDescription()
 */
public class CommonIfInIfElseCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_CommonIfInIfElseCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_CommonIfInIfElseCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_CommonIfInIfElseCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement node) {
		IfStatement thenInnerIfStatement= ASTNodes.as(node.getThenStatement(), IfStatement.class);
		IfStatement elseInnerIfStatement= ASTNodes.as(node.getElseStatement(), IfStatement.class);

		if (ASTNodes.isPassive(node.getExpression()) && thenInnerIfStatement != null && elseInnerIfStatement != null
				&& thenInnerIfStatement.getElseStatement() == null && elseInnerIfStatement.getElseStatement() == null
				&& ASTNodes.isPassive(thenInnerIfStatement.getExpression())
				&& ASTNodes.match(thenInnerIfStatement.getExpression(), elseInnerIfStatement.getExpression())) {
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			ASTRewrite rewrite= cuRewrite.getASTRewrite();

			rewrite.replace(node,
					ast.if0(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(thenInnerIfStatement.getExpression())),
							ast.block(ast.if0(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(node.getExpression())),
							ASTNodes.createMoveTarget(rewrite, thenInnerIfStatement.getThenStatement()), ASTNodes.createMoveTarget(rewrite, elseInnerIfStatement.getThenStatement())))), null);
			return false;
		}

		return true;
	}
}
