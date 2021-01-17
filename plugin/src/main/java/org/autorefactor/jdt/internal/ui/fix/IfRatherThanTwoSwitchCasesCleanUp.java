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
import org.autorefactor.jdt.internal.corext.dom.VarConflictVisitor;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class IfRatherThanTwoSwitchCasesCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.IfRatherThanTwoSwitchCasesCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.IfRatherThanTwoSwitchCasesCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.IfRatherThanTwoSwitchCasesCleanUp_reason;
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

		replaceBySwitch(visited, switchStructure, caseIndexWithDefault);
		return false;
	}

	private void replaceBySwitch(final SwitchStatement visited,
			final List<Pair<List<Expression>, List<Statement>>> switchStructure, final int caseIndexWithDefault) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.IfRatherThanTwoSwitchCasesCleanUp_description);

		int localCaseIndexWithDefault= caseIndexWithDefault;
		Expression discriminant= visited.getExpression();
		Statement currentBlock= null;

		for (int i= switchStructure.size() - 1; i >= 0; i--) {
			Pair<List<Expression>, List<Statement>> caseStructure= switchStructure.get(i);

			Expression newCondition;
			if (caseStructure.getFirst().isEmpty()) {
				newCondition= null;
			} else if (caseStructure.getFirst().size() == 1) {
				newCondition= buildEquality(discriminant, caseStructure.getFirst().get(0));
			} else {
				List<Expression> equalities= new ArrayList<>();

				for (Expression value : caseStructure.getFirst()) {
					equalities.add(ASTNodeFactory.parenthesizeIfNeeded(ast, buildEquality(discriminant, value)));
				}
				newCondition= ast.newInfixExpression(InfixExpression.Operator.CONDITIONAL_OR, equalities);
			}

			Block newBlock= ast.newBlock();

			for (Statement statement : caseStructure.getSecond()) {
				newBlock.statements().add(ast.createCopyTarget(statement));
			}

			if (currentBlock != null) {
				IfStatement newIfStatement= ast.newIfStatement();
				newIfStatement.setExpression(newCondition);
				newIfStatement.setThenStatement(newBlock);
				newIfStatement.setElseStatement(currentBlock);
				currentBlock= newIfStatement;
			} else if (caseStructure.getSecond().isEmpty()) {
				localCaseIndexWithDefault = -1;
			} else if (localCaseIndexWithDefault == -1) {
				IfStatement newIfStatement= ast.newIfStatement();
				newIfStatement.setExpression(newCondition);
				newIfStatement.setThenStatement(newBlock);
				currentBlock= newIfStatement;
			} else {
				currentBlock= newBlock;
			}
		}

		ASTNodes.replaceButKeepComment(rewrite, visited, currentBlock, group);
	}

	private Expression buildEquality(final Expression discriminant, final Expression value) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		if (ASTNodes.hasType(value, String.class.getCanonicalName(), Boolean.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(),
				Double.class.getCanonicalName(), Float.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Short.class.getCanonicalName())) {
			MethodInvocation methodInvocation= ast.newMethodInvocation();
			methodInvocation.setExpression(ast.createCopyTarget(value));
			methodInvocation.setName(ast.newSimpleName("equals")); //$NON-NLS-1$
			methodInvocation.arguments().add(ast.createCopyTarget(ASTNodes.getUnparenthesedExpression(discriminant)));
			return methodInvocation;
		}

		InfixExpression newInfixExpression= ast.newInfixExpression();
		newInfixExpression.setOperator(InfixExpression.Operator.EQUALS);

		if (value.resolveTypeBinding() != null && value.resolveTypeBinding().isEnum()) {
			newInfixExpression.setLeftOperand(ast.createCopyTarget(discriminant));
			newInfixExpression.setRightOperand(ast.getAST().newQualifiedName(
								ASTNodeFactory.newName(ast, value.resolveTypeBinding().getQualifiedName()), ast.createCopyTarget((SimpleName) value)));
		} else {
			newInfixExpression.setLeftOperand(ASTNodeFactory.parenthesizeIfNeeded(ast, ast.createCopyTarget(discriminant)));
			newInfixExpression.setRightOperand(ast.createCopyTarget(value));
		}

		return newInfixExpression;
	}
}
