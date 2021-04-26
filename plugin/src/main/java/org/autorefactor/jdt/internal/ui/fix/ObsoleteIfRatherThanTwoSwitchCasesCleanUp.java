/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarConflictVisitor;
import org.autorefactor.util.Pair;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteIfRatherThanTwoSwitchCasesCleanUp extends AbstractCleanUpRule {
	private static class BreakVisitor extends InterruptibleVisitor {
		private final SwitchStatement root;
		private final List<BreakStatement> breaks= new ArrayList<>();
		private boolean canBeRefactored= true;

		public BreakVisitor(final SwitchStatement root) {
			this.root= root;
		}

		public List<BreakStatement> getBreaks() {
			return breaks;
		}

		public boolean canBeRefactored() {
			return canBeRefactored;
		}

		@Override
		public boolean visit(final BreakStatement aBreak) {
			if (aBreak.getLabel() != null) {
				return false;
			}

			Statement parent= aBreak;
			do {
				parent= ASTNodes.getTypedAncestor(parent, Statement.class);
			} while (parent != root && Utils.isEmpty(ASTNodes.getNextSiblings(parent)));

			if (parent != root) {
				canBeRefactored= false;
				return interruptVisit();
			}

			breaks.add(aBreak);

			return true;
		}

		@Override
		public boolean visit(final WhileStatement visited) {
			return false;
		}

		@Override
		public boolean visit(final DoStatement visited) {
			return false;
		}

		@Override
		public boolean visit(final ForStatement visited) {
			return false;
		}

		@Override
		public boolean visit(final EnhancedForStatement visited) {
			return false;
		}

		@Override
		public boolean visit(final SwitchStatement visited) {
			return false;
		}

		@Override
		public boolean visit(final AnonymousClassDeclaration visited) {
			return false;
		}

		@Override
		public boolean visit(final LambdaExpression visited) {
			return false;
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteIfRatherThanTwoSwitchCasesCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteIfRatherThanTwoSwitchCasesCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteIfRatherThanTwoSwitchCasesCleanUp_reason;
	}

	@SuppressWarnings("deprecation")
	@Override
	public boolean visit(final SwitchStatement visited) {
		if (!ASTNodes.isPassive(visited.getExpression())) {
			return true;
		}

		List<?> statements= visited.statements();

		if (statements.isEmpty()) {
			return true;
		}

		Set<SimpleName> previousVarIds= new HashSet<>();
		Set<SimpleName> caseVarIds= new HashSet<>();
		List<Pair<List<Expression>, List<Statement>>> switchStructure= new ArrayList<>();
		List<Expression> caseExprs= new ArrayList<>();
		List<Statement> caseStatements= new ArrayList<>();

		boolean isPreviousStmtACase= true;
		int caseNb= 0;
		int caseIndexWithDefault= -1;

		for (Object object : statements) {
			Statement statement= (Statement) object;

			if (statement instanceof SwitchCase) {
				if (!isPreviousStmtACase) {
					caseNb++;

					if (caseNb > 2) {
						return true;
					}

					previousVarIds.addAll(caseVarIds);
					caseVarIds.clear();

					switchStructure.add(Pair.<List<Expression>, List<Statement>>of(caseExprs, caseStatements));
					caseExprs= new ArrayList<>();
					caseStatements= new ArrayList<>();
				}

				if (((SwitchCase) statement).isDefault()) {
					caseIndexWithDefault= caseNb;
				} else {
					caseExprs.add(((SwitchCase) statement).getExpression());
				}

				isPreviousStmtACase= true;
			} else {
				VarConflictVisitor varOccurrenceVisitor= new VarConflictVisitor(previousVarIds, false);
				varOccurrenceVisitor.traverseNodeInterruptibly(statement);

				if (varOccurrenceVisitor.isVarConflicting()) {
					return true;
				}

				caseVarIds.addAll(ASTNodes.getLocalVariableIdentifiers(statement, false));
				caseStatements.add(statement);

				isPreviousStmtACase= false;
			}
		}

		switchStructure.add(Pair.<List<Expression>, List<Statement>>of(caseExprs, caseStatements));
		caseNb++;

		if (caseNb > 2) {
			return true;
		}

		if (caseIndexWithDefault != -1) {
			Pair<List<Expression>, List<Statement>> caseWithDefault= switchStructure.remove(caseIndexWithDefault);
			switchStructure.add(caseWithDefault);
		}

		List<BreakStatement> overBreaks= new ArrayList<>();

		for (int i= 0; i < switchStructure.size(); i++) {
			Pair<List<Expression>, List<Statement>> caseStructure= switchStructure.get(i);

			if (!caseStructure.getSecond().isEmpty()) {
				Statement lastStatement= caseStructure.getSecond().get(caseStructure.getSecond().size() - 1);

				if (i < switchStructure.size() - 1 && !ASTNodes.fallsThrough(lastStatement)) {
					return true;
				}

				BreakStatement breakStatement= ASTNodes.as(lastStatement, BreakStatement.class);

				if (breakStatement != null && breakStatement.getLabel() == null) {
					caseStructure.getSecond().remove(caseStructure.getSecond().size() - 1);
				}
			} else if (i < switchStructure.size() - 1) {
				return true;
			}
		}

		for (Pair<List<Expression>, List<Statement>> caseStructure : switchStructure) {
			for (Statement oneStatement : caseStructure.getSecond()) {
				BreakVisitor breakVisitor= new BreakVisitor(visited);
				breakVisitor.traverseNodeInterruptibly(oneStatement);

				if (!breakVisitor.canBeRefactored()) {
					return true;
				}

				overBreaks.addAll(breakVisitor.getBreaks());
			}
		}

		replaceBySwitch(visited, switchStructure, caseIndexWithDefault, overBreaks);
		return false;
	}

	private void replaceBySwitch(final SwitchStatement visited,
			final List<Pair<List<Expression>, List<Statement>>> switchStructure, final int caseIndexWithDefault, final List<BreakStatement> overBreaks) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteIfRatherThanTwoSwitchCasesCleanUp_description);

		List<Block> newBlocks= prepareNewBlocks(rewrite, ast, switchStructure);

		for (BreakStatement breakStatement : overBreaks) {
			rewrite.remove(breakStatement, group);
		}

		int localCaseIndexWithDefault= caseIndexWithDefault;
		Expression discriminant= visited.getExpression();
		Statement currentBlock= null;

		for (int i= switchStructure.size() - 1; i >= 0; i--) {
			Pair<List<Expression>, List<Statement>> caseStructure= switchStructure.get(i);

			Expression newCondition= buildNewCondition(rewrite, ast, discriminant, caseStructure);

			if (currentBlock != null) {
				IfStatement newIfStatement= ast.newIfStatement();
				newIfStatement.setExpression(newCondition);
				newIfStatement.setThenStatement(newBlocks.get(i));
				newIfStatement.setElseStatement(currentBlock);
				currentBlock= newIfStatement;
			} else if (caseStructure.getSecond().isEmpty()) {
				localCaseIndexWithDefault = -1;
			} else if (localCaseIndexWithDefault == -1) {
				IfStatement newIfStatement= ast.newIfStatement();
				newIfStatement.setExpression(newCondition);
				newIfStatement.setThenStatement(newBlocks.get(i));
				currentBlock= newIfStatement;
			} else {
				currentBlock= newBlocks.get(i);
			}
		}

		ASTNodes.replaceButKeepComment(rewrite, visited, currentBlock, group);
	}

	private List<Block> prepareNewBlocks(final ASTRewrite rewrite,
			final ASTNodeFactory ast, final List<Pair<List<Expression>, List<Statement>>> switchStructure) {
		List<Block> newBlocks= new ArrayList<>(switchStructure.size());

		for (Pair<List<Expression>, List<Statement>> caseStructure : switchStructure) {
			Block newBlock= ast.newBlock();

			if (!caseStructure.getSecond().isEmpty()) {
				ListRewrite listRewrite= rewrite.getListRewrite(caseStructure.getSecond().get(0).getParent(), SwitchStatement.STATEMENTS_PROPERTY);
				ASTNode moveTarget= listRewrite.createMoveTarget(caseStructure.getSecond().get(0), caseStructure.getSecond().get(caseStructure.getSecond().size() - 1));
				newBlock.statements().add(moveTarget);
			}

			newBlocks.add(newBlock);
		}

		return newBlocks;
	}

	private Expression buildNewCondition(final ASTRewrite rewrite,
			final ASTNodeFactory ast, final Expression discriminant,
			final Pair<List<Expression>, List<Statement>> caseStructure) {
		if (caseStructure.getFirst().isEmpty()) {
			return null;
		}

		if (caseStructure.getFirst().size() == 1) {
			return buildEquality(rewrite, ast, discriminant, caseStructure.getFirst().get(0));
		}

		List<Expression> equalities= new ArrayList<>();

		for (Expression value : caseStructure.getFirst()) {
			equalities.add(ASTNodeFactory.parenthesizeIfNeeded(ast, buildEquality(rewrite, ast, discriminant, value)));
		}

		return ast.newInfixExpression(InfixExpression.Operator.CONDITIONAL_OR, equalities);
	}

	private Expression buildEquality(final ASTRewrite rewrite,
			ASTNodeFactory ast, final Expression discriminant, final Expression value) {
		if (ASTNodes.hasType(value, String.class.getCanonicalName(), Boolean.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(),
				Double.class.getCanonicalName(), Float.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Short.class.getCanonicalName())) {
			MethodInvocation equalsMethod= ast.newMethodInvocation();
			equalsMethod.setExpression(ast.createCopyTarget(value));
			equalsMethod.setName(ast.newSimpleName("equals")); //$NON-NLS-1$
			equalsMethod.arguments().add(ast.createCopyTarget(ASTNodes.getUnparenthesedExpression(discriminant)));
			return equalsMethod;
		}

		InfixExpression newInfixExpression= ast.newInfixExpression();
		newInfixExpression.setOperator(InfixExpression.Operator.EQUALS);

		if (value.resolveTypeBinding() != null && value.resolveTypeBinding().isEnum()) {
			newInfixExpression.setLeftOperand(ast.createCopyTarget(discriminant));
			newInfixExpression.setRightOperand(ast.getAST().newQualifiedName(
								ASTNodeFactory.newName(ast, value.resolveTypeBinding().getQualifiedName()), ASTNodes.createMoveTarget(rewrite, (SimpleName) value)));
		} else {
			newInfixExpression.setLeftOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ast.createCopyTarget(discriminant)));
			newInfixExpression.setRightOperand(ASTNodes.createMoveTarget(rewrite, value));
		}

		return newInfixExpression;
	}
}
