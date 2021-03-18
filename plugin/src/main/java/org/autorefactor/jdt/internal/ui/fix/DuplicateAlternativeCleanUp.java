/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2021 Fabrice TIERCELIN - initial API and implementation
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
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class DuplicateAlternativeCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.DuplicateAlternativeCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.DuplicateAlternativeCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.DuplicateAlternativeCleanUp_reason;
	}

	@Override
	public boolean visit(final IfStatement visited) {
		if (visited.getElseStatement() != null) {
			IfStatement innerIf= ASTNodes.as(visited.getThenStatement(), IfStatement.class);

			if (innerIf != null
					&& innerIf.getElseStatement() != null
					&& !ASTNodes.asList(visited.getElseStatement()).isEmpty()
					&& ASTNodes.getNbOperands(visited.getExpression()) + ASTNodes.getNbOperands(innerIf.getExpression()) < ASTNodes.EXCESSIVE_OPERAND_NUMBER) {
				if (ASTNodes.match(visited.getElseStatement(), innerIf.getElseStatement())) {
					replaceIfNoElseStatement(visited, innerIf, innerIf.getThenStatement(), true);
					return false;
				}

				if (ASTNodes.match(visited.getElseStatement(), innerIf.getThenStatement())) {
					replaceIfNoElseStatement(visited, innerIf, innerIf.getElseStatement(), false);
					return false;
				}
			}
		}

		return true;
	}

	private void replaceIfNoElseStatement(final IfStatement visited, final IfStatement innerIf,
			final Statement innerMainStatement, final boolean isInnerMainFirst) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.DuplicateAlternativeCleanUp_description);

		if (visited.getThenStatement() instanceof Block || innerMainStatement instanceof Block) {
			InfixExpression newInfixExpression= ast.newInfixExpression();

			Expression outerCondition;
			if (isInnerMainFirst) {
				outerCondition= ASTNodes.createMoveTarget(rewrite, visited.getExpression());
			} else {
				outerCondition= ast.negate(visited.getExpression(), true);
			}

			newInfixExpression.setLeftOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, outerCondition));
			newInfixExpression.setOperator(isInnerMainFirst ? InfixExpression.Operator.CONDITIONAL_AND
					: InfixExpression.Operator.CONDITIONAL_OR);
			newInfixExpression.setRightOperand(ASTNodeFactory.parenthesizeIfNeeded(ast,
					ASTNodes.createMoveTarget(rewrite, innerIf.getExpression())));

			ASTNodes.replaceButKeepComment(rewrite, innerIf.getExpression(), newInfixExpression, group);
			ASTNodes.replaceButKeepComment(rewrite, visited, ASTNodes.createMoveTarget(rewrite, innerIf), group);
		} else {
			// Workaround: Do not do the cleanup
			// Prepare the code for the next pass
			Block newBlock= ast.newBlock();
			newBlock.statements().add(ASTNodes.createMoveTarget(rewrite, innerIf));

			ASTNodes.replaceButKeepComment(rewrite, innerIf, newBlock, group);
		}
	}
}
