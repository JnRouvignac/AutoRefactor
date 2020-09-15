/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class AssignRatherThanFilterThenAssignAnywayCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.AssignRatherThanFilterThenAssignAnywayCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.AssignRatherThanFilterThenAssignAnywayCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.AssignRatherThanFilterThenAssignAnywayCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		IfAndReturnVisitor ifAndReturnVisitor= new IfAndReturnVisitor();
		ifAndReturnVisitor.visitNode(node);
		return ifAndReturnVisitor.result;
	}

	private final class IfAndReturnVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final IfStatement node) {
			InfixExpression condition= ASTNodes.as(node.getExpression(), InfixExpression.class);
			Statement thenStatement= getThenStatement(node);
			Statement elseStatement= getElseStatement(node, thenStatement);

			if (result && condition != null && ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS) && !condition.hasExtendedOperands() && thenStatement != null && elseStatement != null) {
				Assignment thenAssignment= ASTNodes.asExpression(thenStatement, Assignment.class);
				Assignment elseAssignment= ASTNodes.asExpression(elseStatement, Assignment.class);
				boolean isEqual= ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS);

				if (ASTNodes.hasOperator(thenAssignment, Assignment.Operator.ASSIGN) && ASTNodes.hasOperator(elseAssignment, Assignment.Operator.ASSIGN)
						&& ASTNodes.match(thenAssignment.getLeftHandSide(), elseAssignment.getLeftHandSide())) {
					return maybeReplaceWithStraightAssign(node, condition, thenAssignment, elseAssignment, isEqual);
				}

				ReturnStatement thenRS= ASTNodes.as(thenStatement, ReturnStatement.class);
				ReturnStatement elseRS= ASTNodes.as(elseStatement, ReturnStatement.class);

				if (thenRS != null && elseRS != null) {
					if (isEqual) {
						return maybeReplaceWithStraightReturn(node, condition, elseRS, thenRS, elseRS);
					}

					return maybeReplaceWithStraightReturn(node, condition, thenRS, elseRS, elseRS);
				}
			}

			return true;
		}

		private boolean maybeReplaceWithStraightAssign(final IfStatement node, final InfixExpression condition,
				final Assignment thenAssignment, final Assignment elseAssignment, final boolean isEqual) {
			Expression hardCodedExpression;
			Assignment valuedAssignment;

			if (isEqual) {
				hardCodedExpression= thenAssignment.getRightHandSide();
				valuedAssignment= elseAssignment;
			} else {
				hardCodedExpression= elseAssignment.getRightHandSide();
				valuedAssignment= thenAssignment;
			}

			if (ASTNodes.isHardCoded(hardCodedExpression)) {
				if (ASTNodes.isPassiveWithoutFallingThrough(condition.getLeftOperand())
						&& ASTNodes.match(condition.getRightOperand(), hardCodedExpression)
						&& ASTNodes.match(condition.getLeftOperand(), valuedAssignment.getRightHandSide())) {
					replaceWithStraightAssign(node, valuedAssignment.getLeftHandSide(), condition.getLeftOperand());
					this.result= false;
					return false;
				}

				if (ASTNodes.isPassiveWithoutFallingThrough(condition.getRightOperand())
						&& ASTNodes.match(condition.getLeftOperand(), hardCodedExpression)
						&& ASTNodes.match(condition.getRightOperand(), valuedAssignment.getRightHandSide())) {
					replaceWithStraightAssign(node, valuedAssignment.getLeftHandSide(), condition.getRightOperand());
					this.result= false;
					return false;
				}
			}

			return true;
		}

		private Statement getThenStatement(final IfStatement node) {
			List<Statement> thenStatements= ASTNodes.asList(node.getThenStatement());

			if (thenStatements.size() == 1) {
				return thenStatements.get(0);
			}

			return null;
		}

		private Statement getElseStatement(final IfStatement node, final Statement thenStatement) {
			List<Statement> elseStatements= ASTNodes.asList(node.getElseStatement());

			if (elseStatements.size() == 1) {
				return elseStatements.get(0);
			}

			if (ASTNodes.is(thenStatement, ReturnStatement.class)) {
				return ASTNodes.getNextSibling(node);
			}

			return null;
		}

		private void replaceWithStraightAssign(final IfStatement node, final Expression leftHandSide, final Expression rightHandSide) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.AssignRatherThanFilterThenAssignAnywayCleanUp_description);

			rewrite.replace(node,
					ast.toStatement(ast.assign(ASTNodes.createMoveTarget(rewrite, leftHandSide), Assignment.Operator.ASSIGN, ASTNodes.createMoveTarget(rewrite, rightHandSide))), group);
		}

		private boolean maybeReplaceWithStraightReturn(final IfStatement node, final InfixExpression condition, final ReturnStatement valuedReturn,
				final ReturnStatement hardCodedReturn, final Statement toRemove) {
			if (ASTNodes.isHardCoded(hardCodedReturn.getExpression())) {
				if (ASTNodes.isPassiveWithoutFallingThrough(condition.getLeftOperand())
						&& ASTNodes.match(condition.getRightOperand(), hardCodedReturn.getExpression())
						&& ASTNodes.match(condition.getLeftOperand(), valuedReturn.getExpression())) {
					replaceWithStraightReturn(node, condition.getLeftOperand(), toRemove);
					this.result= false;
					return false;
				}

				if (ASTNodes.isPassiveWithoutFallingThrough(condition.getRightOperand())
						&& ASTNodes.match(condition.getLeftOperand(), hardCodedReturn.getExpression())
						&& ASTNodes.match(condition.getRightOperand(), valuedReturn.getExpression())) {
					replaceWithStraightReturn(node, condition.getRightOperand(), toRemove);
					this.result= false;
					return false;
				}
			}

			return true;
		}

		private void replaceWithStraightReturn(final IfStatement node, final Expression returnedExpression, final Statement toRemove) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.AssignRatherThanFilterThenAssignAnywayCleanUp_description);

			rewrite.remove(toRemove, group);
			rewrite.replace(node, ast.return0(ASTNodes.createMoveTarget(rewrite, returnedExpression)), group);
		}
	}
}
