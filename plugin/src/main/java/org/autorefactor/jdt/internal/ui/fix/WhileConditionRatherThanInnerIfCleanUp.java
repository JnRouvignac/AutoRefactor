/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Move a condition if an inner if than breaks a while loop into the while condition.
 *
 * @see #getDescription()
 */
public class WhileConditionRatherThanInnerIfCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.WhileConditionRatherThanInnerIfCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.WhileConditionRatherThanInnerIfCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.WhileConditionRatherThanInnerIfCleanUp_reason;
	}

	@Override
	public boolean visit(final WhileStatement node) {
		List<Statement> statements= ASTNodes.asList(node.getBody());

		if (!statements.isEmpty()) {
			IfStatement ifStatement= ASTNodes.as(statements.get(0), IfStatement.class);

			if (ifStatement != null) {
				return maybeRefactorWhile(node, ifStatement, ifStatement.getThenStatement(), ifStatement.getElseStatement(), true)
						&& maybeRefactorWhile(node, ifStatement, ifStatement.getElseStatement(), ifStatement.getThenStatement(), false);
			}
		}

		return true;
	}

	private boolean maybeRefactorWhile(final WhileStatement node, final IfStatement ifStatement, final Statement breakingStatement,
			final Statement otherStatement, final boolean isPositive) {
		List<Statement> breakingStatements= ASTNodes.asList(breakingStatement);

		if (breakingStatements.size() == 1) {
			BreakStatement breakStatement= ASTNodes.as(breakingStatements.get(0), BreakStatement.class);

			if (breakStatement != null && breakStatement.getLabel() == null) {
				refactorWhileCondition(node, ifStatement, otherStatement, isPositive);
				return false;
			}
		}

		return true;
	}

	private void refactorWhileCondition(final WhileStatement node, final IfStatement ifStatement, final Statement otherStatement,
			final boolean isPositive) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.WhileConditionRatherThanInnerIfCleanUp_description);

		Expression originalCondition= ASTRewrite.parenthesizeIfNeeded(ast, ASTNodes.createMoveTarget(rewrite, node.getExpression()));
		Expression ifCondition= ifStatement.getExpression();

		if (isPositive) {
			ifCondition= ast.negate(ifCondition);
		} else {
			ifCondition= ASTNodes.createMoveTarget(rewrite, ifCondition);
		}

		InfixExpression newCondition= ast.newInfixExpression(originalCondition, InfixExpression.Operator.CONDITIONAL_AND, ASTRewrite.parenthesizeIfNeeded(ast, ifCondition));
		rewrite.replace(node.getExpression(), newCondition, group);

		List<Statement> otherStatements= ASTNodes.asList(otherStatement);

		if (ASTNodes.canHaveSiblings(ifStatement)) {
			if (otherStatement != null && !otherStatements.isEmpty()) {
				for (int i= otherStatements.size() - 1; 0 < i; i--) {
					rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, otherStatements.get(i)), ifStatement, group);
				}

				rewrite.replace(ifStatement, ASTNodes.createMoveTarget(rewrite, otherStatements.get(0)), group);
			} else {
				rewrite.remove(ifStatement, group);
			}
		} else if (otherStatement == null || otherStatements.isEmpty()) {
			rewrite.replace(node.getBody(), ast.newBlock(), group);
		} else if (otherStatements.size() == 1) {
			rewrite.replace(originalCondition, ASTNodes.createMoveTarget(rewrite, otherStatements.get(0)), group);
		} else {
			List<Statement> newStatements= new ArrayList<>(otherStatements.size());

			for (Statement statement : otherStatements) {
				newStatements.add(ASTNodes.createMoveTarget(rewrite, statement));
			}

			Block block= ast.newBlock(newStatements);

			rewrite.replace(node.getBody(), block, group);
		}
	}
}
