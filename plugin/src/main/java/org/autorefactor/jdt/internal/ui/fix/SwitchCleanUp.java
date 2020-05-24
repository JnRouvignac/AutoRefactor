/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - various fixes
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
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTMatcherSameVariablesAndMethods;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.FinderVisitor;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.CharacterLiteral;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SwitchCase;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class SwitchCleanUp extends AbstractCleanUpRule {
	private static final class Variable {
		private final Expression name;
		private final List<Expression> constantValues;

		private Variable(final Expression firstOp, final List<Expression> constantValues) {
			this.name= firstOp;
			this.constantValues= constantValues;
		}

		private boolean isSameVariable(final Variable other) {
			return other != null && ASTNodes.isSameVariable(name, other.name);
		}

		private Variable mergeValues(final Variable other) {
			List<Expression> values= new ArrayList<>(constantValues);
			values.addAll(other.constantValues);
			return new Variable(name, values);
		}

		@Override
		public String toString() {
			return constantValues.size() == 1 ? name + " = " + constantValues.get(0) //$NON-NLS-1$
					: name + " = one of " + constantValues; //$NON-NLS-1$
		}
	}

	/**
	 * Represents a switch case section (cases + statements).
	 * <p>
	 * It can represent a switch case to build (when converting if else if
	 * statements), or existing switch cases when representing the structure of a
	 * whole switch.
	 */
	private static final class SwitchCaseSection {
		/**
		 * Must resolve to constant values. Used when representing switch cases to
		 * build.
		 */
		private final List<Expression> constantExprs;
		/** Used when representing the existing switch cases in a switch structure. */
		private final List<SwitchCase> existingCases;
		/** The statements executed for the switch cases. */
		private final List<Statement> statements;
		private final ASTMatcherSameVariablesAndMethods variablesAndMethodsMatcher= new ASTMatcherSameVariablesAndMethods();

		/** Used for switch structures, there is no constant expressions. */
		private SwitchCaseSection() {
			this(Collections.<Expression>emptyList(), new ArrayList<SwitchCase>(), new ArrayList<Statement>());
		}

		/** Used for if statements, only constant expressions are used. */
		private SwitchCaseSection(final List<Expression> constantExprs, final List<Statement> statements) {
			this(constantExprs, Collections.<SwitchCase>emptyList(), statements);
		}

		private SwitchCaseSection(final List<Expression> constantExprs, final List<SwitchCase> existingCases,
				final List<Statement> statements) {
			this.constantExprs= constantExprs;
			this.existingCases= existingCases;
			this.statements= statements;
		}

		private boolean fallsThrough() {
			return statements.isEmpty() || !ASTNodes.fallsThrough(Utils.getLast(statements));
		}

		private boolean hasSameCode(final SwitchCaseSection other) {
			if (statements.size() != other.statements.size()) {
				return false;
			}

			for (int i= 0; i < statements.size(); i++) {
				if (!ASTNodes.match(variablesAndMethodsMatcher, statements.get(i), other.statements.get(i))) {
					return false;
				}
			}

			return true;
		}

		@SuppressWarnings("deprecation")
		@Override
		public String toString() {
			StringBuilder sb= new StringBuilder();

			for (Expression expression : constantExprs) {
				sb.append("new case ").append(expression).append(":\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			for (SwitchCase existingCase : existingCases) {
				sb.append("existing case ").append(existingCase.getExpression()).append(":\n"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			for (Statement statement : statements) {
				sb.append("    ").append(statement); //$NON-NLS-1$
			}

			sb.append("    break; // not needed if previous statement breaks control flow"); //$NON-NLS-1$
			return sb.toString();
		}
	}

	private static final class HasUnlabeledBreakVisitor extends FinderVisitor<Boolean> {
		@Override
		public boolean visit(final BreakStatement node) {
			if (node.getLabel() == null) {
				setResult(true);
				return false;
			}

			return true;
		}

		@Override
		public boolean visit(final DoStatement node) {
			return ignoreUnlabeledBreaksInInnerBreakableStatement();
		}

		@Override
		public boolean visit(final EnhancedForStatement node) {
			return ignoreUnlabeledBreaksInInnerBreakableStatement();
		}

		@Override
		public boolean visit(final ForStatement node) {
			return ignoreUnlabeledBreaksInInnerBreakableStatement();
		}

		@Override
		public boolean visit(final SwitchStatement node) {
			return ignoreUnlabeledBreaksInInnerBreakableStatement();
		}

		@Override
		public boolean visit(final WhileStatement node) {
			return ignoreUnlabeledBreaksInInnerBreakableStatement();
		}

		private boolean ignoreUnlabeledBreaksInInnerBreakableStatement() {
			// Unlabeled breaks in inner loops/switchs work ok with switch cleanup rule
			return false;
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_SwitchCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_SwitchCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_SwitchCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		SeveralIfVisitor severalIfVisitor= new SeveralIfVisitor();
		severalIfVisitor.visitNode(node);
		return severalIfVisitor.result;
	}

	private final class SeveralIfVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final IfStatement node) {
			if (!result || hasUnlabeledBreak(node)) {
				return true;
			}

			Variable variable= extractVariableAndValues(node);
			if (variable == null) {
				return true;
			}

			Expression switchExpression= variable.name;
			List<IfStatement> ifStatements= new ArrayList<>();
			List<SwitchCaseSection> cases= new ArrayList<>();
			Statement remainingStatement= null;

			Set<SimpleName> variableDeclarationIds= new HashSet<>();
			IfStatement ifStatement= node;
			boolean isFallingThrough= true;

			do {
				IfStatement currentNode= ifStatement;

				while (ASTNodes.isSameVariable(switchExpression, variable.name)) {
					if (detectDeclarationConflicts(currentNode.getThenStatement(), variableDeclarationIds)) {
						// Cannot declare two variables with the same name in two cases
						return true;
					}

					cases.add(new SwitchCaseSection(variable.constantValues,
							ASTNodes.asList(currentNode.getThenStatement())));

					if (!ASTNodes.fallsThrough(currentNode.getThenStatement())) {
						isFallingThrough= false;
					}

					remainingStatement= currentNode.getElseStatement();

					if (remainingStatement == null) {
						break;
					}

					variable= extractVariableAndValues(remainingStatement);

					if (variable == null) {
						break;
					}

					currentNode= (IfStatement) remainingStatement;
				}

				ifStatements.add(ifStatement);
				ifStatement= ASTNodes.as(ASTNodes.getNextSibling(ifStatement), IfStatement.class);
				variable= extractVariableAndValues(ifStatement);
			} while (isFallingThrough && ifStatement != null && remainingStatement == null && variable != null && ASTNodes.isSameVariable(switchExpression, variable.name));

			List<SwitchCaseSection> filteredCases= filterDuplicateCaseValues(cases);
			return maybeReplaceWithSwitchStatement(ifStatements, switchExpression, filteredCases, remainingStatement);
		}

		private boolean maybeReplaceWithSwitchStatement(final List<IfStatement> ifStatements, final Expression switchExpression,
				final List<SwitchCaseSection> cases, final Statement remainingStatement) {
			if (switchExpression != null && cases.size() > 2) {
				replaceWithSwitchStatement(ifStatements, switchExpression, cases, remainingStatement);
				this.result= false;
				return false;
			}

			return true;
		}
	}

	private boolean hasUnlabeledBreak(final IfStatement node) {
		return new HasUnlabeledBreakVisitor().findOrDefault(node, false);
	}

	private boolean detectDeclarationConflicts(final Statement statement, final Set<SimpleName> variableDeclarationIds) {
		Set<SimpleName> varNames= ASTNodes.getLocalVariableIdentifiers(statement, false);
		boolean hasConflict= containsAny(variableDeclarationIds, varNames);
		variableDeclarationIds.addAll(varNames);
		return hasConflict;
	}

	private boolean containsAny(final Set<SimpleName> variableDeclarations, final Set<SimpleName> varNames) {
		for (SimpleName varName : varNames) {
			for (SimpleName variableDeclaration : variableDeclarations) {
				if (Utils.equalNotNull(varName.getIdentifier(), variableDeclaration.getIdentifier())) {
					return true;
				}
			}
		}

		return false;
	}

	/** Side-effect: removes the dead branches in a chain of if-elseif. */
	private List<SwitchCaseSection> filterDuplicateCaseValues(final List<SwitchCaseSection> sourceCases) {
		List<SwitchCaseSection> results= new ArrayList<>();
		Set<Object> alreadyProccessedValues= new HashSet<>();

		for (SwitchCaseSection sourceCase : sourceCases) {
			List<Expression> filteredExprs= new ArrayList<>();

			for (Expression expression : sourceCase.constantExprs) {
				Object constantValue= expression.resolveConstantExpressionValue();

				if (constantValue == null) {
					throw new NotImplementedException(expression, "Cannot handle non constant expressions"); //$NON-NLS-1$
				}

				if (alreadyProccessedValues.add(constantValue)) {
					// This is a new value (never seen before)
					filteredExprs.add(expression);
				}
			}

			if (!filteredExprs.isEmpty()) {
				results.add(new SwitchCaseSection(filteredExprs, sourceCase.statements));
			}
		}

		return results;
	}

	private void replaceWithSwitchStatement(final List<IfStatement> ifStatements, final Expression switchExpression,
			final List<SwitchCaseSection> cases, final Statement remainingStatement) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		SwitchStatement switchStatement= ast.switch0(ASTNodes.createMoveTarget(rewrite, switchExpression));

		for (SwitchCaseSection aCase : cases) {
			addCaseWithStatements(switchStatement, aCase.constantExprs, aCase.statements);
		}

		if (remainingStatement != null) {
			addCaseWithStatements(switchStatement, null, ASTNodes.asList(remainingStatement));
		}

		for (int i= 0; i < ifStatements.size() - 1; i++) {
			rewrite.removeButKeepComment(ifStatements.get(i), null);
		}

		rewrite.replace(Utils.getLast(ifStatements), switchStatement, null);
	}

	private void addCaseWithStatements(final SwitchStatement switchStatement, final List<Expression> caseValuesOrNullForDefault,
			final List<Statement> innerStatements) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		@SuppressWarnings("unchecked")
		List<Statement> switchStatements= switchStatement.statements();

		// Add the case statement(s)
		if (caseValuesOrNullForDefault != null) {
			for (Expression caseValue : caseValuesOrNullForDefault) {
				switchStatements.add(ast.case0(ASTNodes.createMoveTarget(rewrite, caseValue)));
			}
		} else {
			switchStatements.add(ast.default0());
		}

		// Add the statement(s) for this case(s)
		boolean isBreakNeeded= true;
		if (!innerStatements.isEmpty()) {
			for (Statement statement : innerStatements) {
				switchStatements.add(ASTNodes.createMoveTarget(rewrite, statement));
			}
			isBreakNeeded= !ASTNodes.fallsThrough(Utils.getLast(innerStatements));
		}

		// When required: end with a break
		if (isBreakNeeded) {
			switchStatements.add(ast.break0());
		}
	}

	private Variable extractVariableAndValues(final Statement statement) {
		if (statement instanceof IfStatement) {
			return extractVariableAndValues(((IfStatement) statement).getExpression());
		}

		return null;
	}

	private Variable extractVariableAndValues(final Expression expression) {
		InfixExpression infix= ASTNodes.as(expression, InfixExpression.class);

		if (infix != null) {
			return extractVariableAndValuesFromInfixExpression(infix);
		}

		return null;
	}

	private Variable extractVariableAndValuesFromInfixExpression(final InfixExpression infixExpression) {
		if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.OR, InfixExpression.Operator.XOR)) {
			List<Expression> operands= ASTNodes.allOperands(infixExpression);
			Variable mergedVariable= null;

			for (Expression operand : operands) {
				Variable variable= extractVariableAndValues(operand);

				if (variable == null) {
					return null;
				}

				if (mergedVariable == null) {
					mergedVariable = variable;
				} else if (mergedVariable.isSameVariable(variable)) {
					mergedVariable = mergedVariable.mergeValues(variable);
				} else {
					return null;
				}
			}

			return mergedVariable;
		}
		if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.EQUALS) && !infixExpression.hasExtendedOperands()) {
			Expression leftOp= infixExpression.getLeftOperand();
			Expression rightOp= infixExpression.getRightOperand();

			Variable variable= extractVariableWithConstantValue(leftOp, rightOp);
			return variable != null ? variable : extractVariableWithConstantValue(rightOp, leftOp);
		}

		return null;
	}

	private Variable extractVariableWithConstantValue(final Expression variable, final Expression constant) {
		if ((variable instanceof Name || variable instanceof FieldAccess || variable instanceof SuperFieldAccess) && ASTNodes.hasType(variable, char.class.getCanonicalName(), byte.class.getCanonicalName(), short.class.getCanonicalName(), int.class.getCanonicalName())
				&& (constant instanceof NumberLiteral || constant instanceof CharacterLiteral)) {
			return new Variable(variable, Arrays.asList(constant));
		}

		return null;
	}

	@Override
	public boolean visit(final SwitchStatement node) {
		List<SwitchCaseSection> switchStructure= getSwitchStructure(node);

		for (int referenceIndex= 0; referenceIndex < switchStructure.size() - 1; referenceIndex++) {
			SwitchCaseSection referenceCase= switchStructure.get(referenceIndex);

			if (referenceCase.fallsThrough()) {
				continue;
			}

			for (int comparedIndex= referenceIndex + 1; comparedIndex < switchStructure.size(); comparedIndex++) {
				SwitchCaseSection comparedCase= switchStructure.get(comparedIndex);

				if (referenceCase.hasSameCode(comparedCase)) {
					if (!previousSectionFallsthrough(switchStructure, comparedIndex)) {
						mergeCases(Merge.AFTER_SWITCH_CASES, referenceCase, comparedCase);
						return false;
					}

					if (referenceIndex == 0 || !previousSectionFallsthrough(switchStructure, referenceIndex)) {
						mergeCases(Merge.BEFORE_SWITCH_CASES, comparedCase, referenceCase);
						return false;
					}
				}
			}
		}

		return true;
	}

	private boolean previousSectionFallsthrough(final List<SwitchCaseSection> switchStructure, final int idx) {
		return switchStructure.get(idx - 1).fallsThrough();
	}

	@SuppressWarnings("unchecked")
	private List<SwitchCaseSection> getSwitchStructure(final SwitchStatement node) {
		List<SwitchCaseSection> switchStructure= new ArrayList<>();

		SwitchCaseSection currentCase= new SwitchCaseSection();
		for (Statement statement : (List<Statement>) node.statements()) {
			if (statement instanceof SwitchCase) {
				if (!currentCase.statements.isEmpty()) {
					switchStructure.add(currentCase);
					currentCase= new SwitchCaseSection();
				}

				SwitchCase swithCase= (SwitchCase) statement;
				currentCase.existingCases.add(swithCase);
			} else {
				currentCase.statements.add(statement);
			}
		}

		if (!currentCase.existingCases.isEmpty()) {
			switchStructure.add(currentCase);
		}

		return switchStructure;
	}

	enum Merge {
		/** Insert before the first `case XX:`. */
		BEFORE_SWITCH_CASES,
		/** Insert after the last `case XX:`, i.e before the first statement. */
		AFTER_SWITCH_CASES
	}

	private void mergeCases(final Merge merge, final SwitchCaseSection sectionToKeep, final SwitchCaseSection sectionToRemove) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		Statement caseKept;
		if (merge == Merge.BEFORE_SWITCH_CASES) {
			caseKept= sectionToKeep.existingCases.get(0);
		} else { // move == Move.AFTER_SWITCH_CASES
			caseKept= sectionToKeep.statements.get(0);
		}

		for (SwitchCase caseToMove : sectionToRemove.existingCases) {
			rewrite.insertBefore(ASTNodes.createMoveTarget(rewrite, caseToMove), caseKept, null);
		}
		rewrite.remove(sectionToRemove.statements, null);
	}
}
