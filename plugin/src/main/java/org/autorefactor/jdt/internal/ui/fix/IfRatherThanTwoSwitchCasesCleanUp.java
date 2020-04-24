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
import org.autorefactor.jdt.internal.corext.dom.VarOccurrenceVisitor;
import org.autorefactor.util.Pair;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;

/** See {@link #getDescription()} method. */
public class IfRatherThanTwoSwitchCasesCleanUp extends AbstractCleanUpRule {
	/**
	 * Get the name.
	 *
	 * @return the name.
	 */
	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_name;
	}

	/**
	 * Get the description.
	 *
	 * @return the description.
	 */
	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_description;
	}

	/**
	 * Get the reason.
	 *
	 * @return the reason.
	 */
	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanTwoSwitchCasesCleanUp_reason;
	}

	@SuppressWarnings("deprecation")
	@Override
	public boolean visit(final SwitchStatement node) {
		if (!ASTNodes.isPassive(node.getExpression())) {
			return true;
		}

		List<?> statements= node.statements();

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
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

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
				VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(previousVarIds, false);
				varOccurrenceVisitor.visitNode(statement);

				if (varOccurrenceVisitor.isVarUsed()) {
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

		for (Pair<List<Expression>, List<Statement>> caseStructure : switchStructure) {
			Statement lastStatement= Utils.getLast(caseStructure.getSecond());

			if (!ASTNodes.fallsThrough(lastStatement)) {
				return true;
			}

			BreakStatement bs= ASTNodes.as(lastStatement, BreakStatement.class);

			if (bs != null && bs.getLabel() == null) {
				caseStructure.getSecond().remove(caseStructure.getSecond().size() - 1);
			}
		}

		replaceSwitch(node, switchStructure, caseIndexWithDefault, ast);

		return false;
	}

	private void replaceSwitch(final SwitchStatement node,
			final List<Pair<List<Expression>, List<Statement>>> switchStructure, final int caseIndexWithDefault,
			final ASTNodeFactory ast) {
		int localCaseIndexWithDefault= caseIndexWithDefault;
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		Expression discriminant= node.getExpression();
		Statement currentBlock= null;

		for (int i= switchStructure.size() - 1; i >= 0; i--) {
			Pair<List<Expression>, List<Statement>> caseStructure= switchStructure.get(i);

			Expression newCondition;
			if (caseStructure.getFirst().isEmpty()) {
				newCondition= null;
			} else if (caseStructure.getFirst().size() == 1) {
				newCondition= buildEquality(ast, discriminant, caseStructure.getFirst().get(0));
			} else {
				List<Expression> equalities= new ArrayList<>();

				for (Expression value : caseStructure.getFirst()) {
					equalities.add(ast.parenthesizeIfNeeded(buildEquality(ast, discriminant, value)));
				}
				newCondition= ast.infixExpression(InfixExpression.Operator.CONDITIONAL_OR, equalities);
			}

			Statement[] copyOfStatements= new Statement[caseStructure.getSecond().size()];

			for (int j= 0; j < caseStructure.getSecond().size(); j++) {
				copyOfStatements[j]= ast.createCopyTarget(caseStructure.getSecond().get(j));
			}

			Block newBlock= ast.block(copyOfStatements);

			if (currentBlock != null) {
				currentBlock= ast.if0(newCondition, newBlock, currentBlock);
			} else if (copyOfStatements.length == 0) {
				localCaseIndexWithDefault= -1;
			} else if (localCaseIndexWithDefault == -1) {
				currentBlock= ast.if0(newCondition, newBlock);
			} else {
				currentBlock= newBlock;
			}
		}

		rewrite.replace(node, currentBlock, null);
	}

	private Expression buildEquality(final ASTNodeFactory ast, final Expression discriminant, final Expression value) {
		Expression equality;

		if (ASTNodes.hasType(value, String.class.getCanonicalName(), Boolean.class.getCanonicalName(), Byte.class.getCanonicalName(), Character.class.getCanonicalName(),
				Double.class.getCanonicalName(), Float.class.getCanonicalName(), Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Short.class.getCanonicalName())) {
			equality= ast.newMethodInvocation(ast.createCopyTarget(value), "equals", ast.createCopyTarget(ASTNodes.getUnparenthesedExpression(discriminant))); //$NON-NLS-1$
		} else if (value.resolveTypeBinding() != null && value.resolveTypeBinding().isEnum()) {
			equality= ast.infixExpression(ast.createCopyTarget(discriminant), InfixExpression.Operator.EQUALS, ast.getAST().newQualifiedName(
					ast.name(value.resolveTypeBinding().getQualifiedName()), ast.createCopyTarget((SimpleName) value)));
		} else {
			equality= ast.infixExpression(ast.parenthesizeIfNeeded(ast.createCopyTarget(discriminant)), InfixExpression.Operator.EQUALS,
					ast.createCopyTarget(value));
		}

		return equality;
	}
}
