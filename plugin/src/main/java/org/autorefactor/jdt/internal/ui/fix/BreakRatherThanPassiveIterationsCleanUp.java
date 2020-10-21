/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarOccurrenceVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class BreakRatherThanPassiveIterationsCleanUp extends AbstractCleanUpRule {
	private static final class SideEffectVisitor extends InterruptibleVisitor {
		private final Set<SimpleName> localVariableNames;
		private boolean hasSideEffect;

		private SideEffectVisitor(final Set<SimpleName> localVariableNames) {
			this.localVariableNames= localVariableNames;
		}

		private boolean hasSideEffect() {
			return hasSideEffect;
		}

		@Override
		public boolean visit(final Assignment node) {
			if (!ASTNodes.hasOperator(node, Assignment.Operator.ASSIGN)) {
				hasSideEffect= true;
				return interruptVisit();
			}

			return visitVar(node.getLeftHandSide());
		}

		private boolean visitVar(final Expression modifiedVar) {
			if (!(modifiedVar instanceof SimpleName)) {
				hasSideEffect= true;
				return interruptVisit();
			}

			boolean isFound= false;

			for (SimpleName localVariableName : localVariableNames) {
				if (ASTNodes.isSameVariable(localVariableName, (SimpleName) modifiedVar)) {
					isFound= true;
					break;
				}
			}

			if (!isFound) {
				hasSideEffect= true;
				return interruptVisit();
			}

			return true;
		}

		@Override
		public boolean visit(final PrefixExpression node) {
			if (ASTNodes.hasOperator(node, PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.DECREMENT)) {
				return visitVar(node.getOperand());
			}

			return true;
		}

		@Override
		public boolean visit(final PostfixExpression node) {
			return visitVar(node.getOperand());
		}

		@Override
		public boolean visit(final InfixExpression node) {
			if (ASTNodes.hasOperator(node, InfixExpression.Operator.PLUS) && ASTNodes.hasType(node, String.class.getCanonicalName())
					&& (mayCallImplicitToString(node.getLeftOperand())
							|| mayCallImplicitToString(node.getRightOperand())
							|| mayCallImplicitToString(node.extendedOperands()))) {
				hasSideEffect= true;
				return interruptVisit();
			}

			return true;
		}

		private boolean mayCallImplicitToString(final List<Expression> extendedOperands) {
			if (extendedOperands != null) {
				for (Expression expression : extendedOperands) {
					if (mayCallImplicitToString(expression)) {
						return true;
					}
				}
			}

			return false;
		}

		private boolean mayCallImplicitToString(final Expression expression) {
			return !ASTNodes.hasType(expression, String.class.getCanonicalName(), boolean.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName(), float.class.getSimpleName(), double.class.getSimpleName(),
					Short.class.getCanonicalName(), Boolean.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Float.class.getCanonicalName(),
					Double.class.getCanonicalName()) && !(expression instanceof PrefixExpression) && !(expression instanceof InfixExpression)
					&& !(expression instanceof PostfixExpression);
		}

		@Override
		public boolean visit(final SuperMethodInvocation node) {
			hasSideEffect= true;
			return interruptVisit();
		}

		@Override
		public boolean visit(final MethodInvocation node) {
			hasSideEffect= true;
			return interruptVisit();
		}

		@Override
		public boolean visit(final ClassInstanceCreation node) {
			hasSideEffect= true;
			return interruptVisit();
		}

		@Override
		public boolean visit(final ThrowStatement node) {
			hasSideEffect= true;
			return interruptVisit();
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.BreakRatherThanPassiveIterationsCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.BreakRatherThanPassiveIterationsCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.BreakRatherThanPassiveIterationsCleanUp_reason;
	}

	@Override
	public boolean visit(final ForStatement node) {
		Set<SimpleName> vars= new HashSet<>();

		for (Expression initializer : (List<Expression>) node.initializers()) {
			vars.addAll(ASTNodes.getLocalVariableIdentifiers(initializer, true));
		}

		if (hasSideEffect(node.getExpression(), vars)) {
			return true;
		}

		for (Expression updater : (List<Expression>) node.updaters()) {
			if (hasSideEffect(updater, vars)) {
				return true;
			}
		}

		return visitLoopBody(node.getBody(), vars);
	}

	private boolean hasSideEffect(final ASTNode node, final Set<SimpleName> allowedVars) {
		SideEffectVisitor variableUseVisitor= new SideEffectVisitor(allowedVars);
		variableUseVisitor.traverseNodeInterruptibly(node);
		return variableUseVisitor.hasSideEffect();
	}

	@Override
	public boolean visit(final EnhancedForStatement node) {
		return !ASTNodes.isArray(node.getExpression()) || visitLoopBody(node.getBody(), new HashSet<SimpleName>());
	}

	private boolean visitLoopBody(final Statement body, final Set<SimpleName> allowedVars) {
		List<Statement> statements= ASTNodes.asList(body);

		if (Utils.isEmpty(statements)) {
			return true;
		}

		for (int i= 0; i < statements.size() - 1; i++) {
			Statement statement= statements.get(i);
			allowedVars.addAll(ASTNodes.getLocalVariableIdentifiers(statement, true));

			if (hasSideEffect(statement, allowedVars)) {
				return true;
			}
		}

		IfStatement ifStatement= ASTNodes.as(statements.get(statements.size() - 1), IfStatement.class);

		if (ifStatement != null && ifStatement.getElseStatement() == null && !hasSideEffect(ifStatement.getExpression(), allowedVars)) {
			List<Statement> assignments= ASTNodes.asList(ifStatement.getThenStatement());

			if (areAssignmentsValid(allowedVars, assignments)) {
				addBreak(ifStatement, assignments);
				return false;
			}
		}

		return true;
	}

	private boolean areAssignmentsValid(final Set<SimpleName> allowedVars, final List<Statement> assignments) {
		for (Statement statement : assignments) {
			VariableDeclarationStatement variableDeclaration= ASTNodes.as(statement, VariableDeclarationStatement.class);
			Assignment assignment= ASTNodes.asExpression(statement, Assignment.class);

			if (variableDeclaration != null) {
				for (Object obj : variableDeclaration.fragments()) {
					VariableDeclarationFragment fragment= (VariableDeclarationFragment) obj;

					if (!ASTNodes.isHardCoded(fragment.getInitializer())) {
						return false;
					}
				}
			} else if (assignment != null
					&& ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
					&& ASTNodes.isHardCoded(assignment.getRightHandSide())
					&& ASTNodes.isPassive(assignment.getLeftHandSide())) {
				VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(allowedVars, true);
				varOccurrenceVisitor.traverseNodeInterruptibly(assignment.getLeftHandSide());

				if (varOccurrenceVisitor.isVarUsed()) {
					return false;
				}
			} else {
				return false;
			}
		}

		return true;
	}

	private void addBreak(final IfStatement ifStatement, final List<Statement> assignments) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.BreakRatherThanPassiveIterationsCleanUp_description);

		if (ifStatement.getThenStatement() instanceof Block) {
			rewrite.insertAfter(ast.newBreakStatement(), assignments.get(assignments.size() - 1), group);
		} else {
			ASTNodes.replaceButKeepComment(rewrite, ifStatement.getThenStatement(), ast.newBlock(ASTNodes.createMoveTarget(rewrite, ifStatement.getThenStatement()), ast.newBreakStatement()), group);
		}
	}
}
