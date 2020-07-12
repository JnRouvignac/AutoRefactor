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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.jdt.internal.ui.fix;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class StringBuilderRatherThanStringCleanUp extends AbstractCleanUpRule {
	private static class VarOccurrenceVisitor extends ASTVisitor {
		private final Set<SimpleName> searchedVariables;
		private final Set<SimpleName> foundVariables= new HashSet<>();
		private final boolean hasToVisitLoops;

		/**
		 * The constructor.
		 *
		 * @param searchedVariables The variable to search
		 * @param hasToVisitLoops Has to visit loops
		 */
		public VarOccurrenceVisitor(final Set<SimpleName> searchedVariables, final boolean hasToVisitLoops) {
			this.searchedVariables= searchedVariables;
			this.hasToVisitLoops= hasToVisitLoops;
		}

		/**
		 * Returns the found variables.
		 *
		 * @return the found variables.
		 */
		public Set<SimpleName> getFoundVariables() {
			return foundVariables;
		}

		@Override
		public boolean visit(final SimpleName aVariable) {
			if (searchedVariables.contains(aVariable)) {
				foundVariables.add(aVariable);
			}

			return true;
		}

		@Override
		public boolean visit(final ForStatement node) {
			return hasToVisitLoops;
		}

		@Override
		public boolean visit(final EnhancedForStatement node) {
			return hasToVisitLoops;
		}

		@Override
		public boolean visit(final WhileStatement node) {
			return hasToVisitLoops;
		}

		@Override
		public boolean visit(final DoStatement node) {
			return hasToVisitLoops;
		}

		@Override
		public boolean visit(final TypeDeclaration node) {
			return false;
		}

		@Override
		public boolean visit(final LambdaExpression node) {
			return false;
		}
	}

	@Override
	public String getName() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderRatherThanStringCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderRatherThanStringCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderRatherThanStringCleanUp_reason;
	}

	@Override
	public boolean visit(final Block node) {
		StringOccurrencesVisitor stringOccurrencesVisitor= new StringOccurrencesVisitor();
		stringOccurrencesVisitor.visitNode(node);
		return stringOccurrencesVisitor.result;
	}

	private final class StringOccurrencesVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final VariableDeclarationStatement node) {
			if (node.fragments().size() != 1) {
				return true;
			}

			VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);
			return visitVariable(node.getType(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getName(), fragment.getInitializer());
		}

		@Override
		public boolean visit(final VariableDeclarationExpression node) {
			if (node.fragments().size() != 1) {
				return true;
			}

			VariableDeclarationFragment fragment= (VariableDeclarationFragment) node.fragments().get(0);
			return visitVariable(node.getType(), fragment.resolveBinding(), fragment.getExtraDimensions(), fragment.getName(), fragment.getInitializer());
		}

		@Override
		public boolean visit(final SingleVariableDeclaration node) {
			return visitVariable(node.getType(), node.resolveBinding(), node.getExtraDimensions(), node.getName(), node.getInitializer());
		}

		private boolean visitVariable(final Type type, final IVariableBinding variableBinding, final int extraDimensions, final SimpleName declaration, final Expression initializer) {
			if (result && extraDimensions == 0
					&& initializer != null
					&& ASTNodes.hasType(type.resolveBinding(), String.class.getCanonicalName())
					&& !ASTNodes.is(initializer, NullLiteral.class)) {
				VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor(variableBinding,
						startNode, true).find();

				List<SimpleName> reads= varOccurrencesVisitor.getReads();
				List<SimpleName> writes= varOccurrencesVisitor.getWrites();
				writes.remove(declaration);

				Set<SimpleName> unvisitedReads= new HashSet<>(reads);
				Set<SimpleName> assignmentWrites= new HashSet<>();
				Set<SimpleName> concatenationWrites= new HashSet<>();

				for (SimpleName simpleName : writes) {
					if (!isWriteValid(simpleName, unvisitedReads, assignmentWrites, concatenationWrites)) {
						return true;
					}
				}

				if (unvisitedReads.size() == 1
						&& !writes.isEmpty()
						&& writes.size() == assignmentWrites.size() + concatenationWrites.size()) {
					Statement declarationStatement= ASTNodes.getTypedAncestor(type, Statement.class);
					SimpleName finalRead= unvisitedReads.iterator().next();

					if (isOccurrencesValid(declarationStatement, reads, writes, finalRead)) {
						replaceString(type, initializer, assignmentWrites, concatenationWrites, finalRead);

						this.result= false;
						return false;
					}
				}
			}

			return true;
		}

		private void replaceString(final Type type, final Expression initializer, final Set<SimpleName> assignmentWrites,
				final Set<SimpleName> concatenationWrites, final SimpleName finalRead) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();

			Class<?> builder;
			if (getJavaMinorVersion() >= 5) {
				builder= StringBuilder.class;
			} else {
				builder= StringBuffer.class;
			}

			rewrite.replace(type, ast.type(builder.getSimpleName()), null);

			StringLiteral stringLiteral= ASTNodes.as(initializer, StringLiteral.class);

			if (stringLiteral != null && stringLiteral.getLiteralValue().matches("")) { //$NON-NLS-1$
				rewrite.replace(initializer, ast.new0(builder.getSimpleName()), null);
			} else {
				rewrite.replace(initializer, ast.new0(builder.getSimpleName(), ASTNodes.createMoveTarget(rewrite, initializer)), null);
			}

			for (SimpleName simpleName : assignmentWrites) {
				Assignment assignment= (Assignment) simpleName.getParent();
				InfixExpression concatenation= ASTNodes.as(assignment.getRightHandSide(), InfixExpression.class);

				List<Expression> operands;
				if (concatenation != null
						&& ASTNodes.hasOperator(concatenation, InfixExpression.Operator.PLUS)) {
					operands= ASTNodes.allOperands(concatenation);
				} else {
					operands= Arrays.asList(assignment.getRightHandSide());
				}

				Expression newExpression= ASTNodes.createMoveTarget(rewrite, assignment.getLeftHandSide());

				for (Object operand : operands) {
					newExpression= ast.newMethodInvocation(newExpression, "append", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) operand))); //$NON-NLS-1$
				}

				rewrite.replace(assignment, newExpression, null);
			}

			for (SimpleName simpleName : concatenationWrites) {
				Assignment assignment= (Assignment) simpleName.getParent();
				InfixExpression concatenation= (InfixExpression) assignment.getRightHandSide();

				Expression newExpression= ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, assignment.getLeftHandSide()), "append", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(concatenation.getRightOperand()))); //$NON-NLS-1$

				if (concatenation.hasExtendedOperands()) {
					for (Object operand : concatenation.extendedOperands()) {
						newExpression= ast.newMethodInvocation(newExpression, "append", ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression((Expression) operand))); //$NON-NLS-1$
					}
				}

				rewrite.replace(assignment, newExpression, null);
			}

			rewrite.replace(finalRead, ast.newMethodInvocation(ASTNodes.createMoveTarget(rewrite, finalRead), "toString"), null); //$NON-NLS-1$
		}

		private boolean isOccurrencesValid(final Statement declaration, final List<SimpleName> reads, final List<SimpleName> writes,
				final SimpleName finalRead) {
			if (declaration != null) {
				Set<SimpleName> remainingWrites= new HashSet<>(writes);
				Set<SimpleName> remainingReads= new HashSet<>(reads);
				remainingReads.remove(finalRead);
				Set<SimpleName> foundVariables= findVariables(declaration, remainingReads, remainingWrites,
						finalRead);

				if (foundVariables.isEmpty()) {
					List<Statement> statements= ASTNodes.getNextSiblings(declaration);
					AtomicBoolean hasFinalReadBeenFound= new AtomicBoolean(false);

					if (isOccurrenceValid(statements, remainingWrites, remainingReads, finalRead, hasFinalReadBeenFound)) {
						return hasFinalReadBeenFound.get() && remainingReads.isEmpty() && remainingWrites.isEmpty();
					}
				}
			}

			return false;
		}

		private boolean isOccurrenceValid(final List<Statement> statements, final Set<SimpleName> remainingWrites,
				final Set<SimpleName> remainingReads, final SimpleName finalRead, final AtomicBoolean hasFinalReadBeenFound) {
			for (Statement statement : statements) {
				Set<SimpleName> foundVariables= findVariables(statement, remainingReads, remainingWrites,
						finalRead);

				if (foundVariables.contains(finalRead)) {
					hasFinalReadBeenFound.set(true);

					if (!findVariables(statement, remainingReads, remainingWrites,
							finalRead, false).contains(finalRead)) {
						return false;
					}

					if (remainingReads.isEmpty() && remainingWrites.isEmpty()) {
						return true;
					}

					if (!foundVariables.containsAll(remainingReads)
							|| !foundVariables.containsAll(remainingWrites)) {
						return false;
					}

					IfStatement ifStatement= ASTNodes.as(statement, IfStatement.class);

					if (ifStatement != null) {
						if (findVariables(ifStatement.getExpression(), remainingReads, remainingWrites,
								finalRead).isEmpty()
								&& isBlockValid(remainingWrites, remainingReads, finalRead, ifStatement.getThenStatement())
								&& isBlockValid(remainingWrites, remainingReads, finalRead, ifStatement.getElseStatement())) {
							remainingWrites.removeAll(foundVariables);
							remainingReads.removeAll(foundVariables);

							return true;
						}

						return false;
					}

					TryStatement tryStatement= ASTNodes.as(statement, TryStatement.class);

					if (tryStatement != null
							&& isEmptyNodes(tryStatement.resources(), remainingReads, remainingWrites,
									finalRead)
							&& isBlockValid(remainingWrites, remainingReads, finalRead, tryStatement.getBody())) {
						for (Object catchClause : tryStatement.catchClauses()) {
							if (!isBlockValid(remainingWrites, remainingReads, finalRead, ((CatchClause) catchClause).getBody())) {
								return false;
							}
						}

						return isBlockValid(remainingWrites, remainingReads, finalRead, tryStatement.getFinally());
					}

					return false;
				}

				remainingWrites.removeAll(foundVariables);
				remainingReads.removeAll(foundVariables);
			}

			return true;
		}

		private boolean isBlockValid(final Set<SimpleName> remainingWrites, final Set<SimpleName> remainingReads,
				final SimpleName finalRead, final Statement subStatement) {
			Set<SimpleName> subRemainingWrites= new HashSet<>(remainingWrites);
			Set<SimpleName> subRemainingReads= new HashSet<>(remainingReads);
			AtomicBoolean subHasFinalReadBeenFound= new AtomicBoolean(false);

			return isOccurrenceValid(ASTNodes.asList(subStatement), subRemainingWrites, subRemainingReads, finalRead, subHasFinalReadBeenFound)
					&& subHasFinalReadBeenFound.get() == (subRemainingReads.isEmpty() && subRemainingWrites.isEmpty());
		}

		private boolean isEmptyNodes(final List<?> nodes, final Set<SimpleName> remainingReads,
				final Set<SimpleName> remainingWrites, final SimpleName finalRead) {
			if (nodes != null) {
				for (Object currentNode : nodes) {
					if (!findVariables((ASTNode) currentNode, remainingReads, remainingWrites,
							finalRead).isEmpty()) {
						return false;
					}
				}
			}

			return true;
		}

		private Set<SimpleName> findVariables(final ASTNode currentNode, final Set<SimpleName> remainingReads,
				final Set<SimpleName> remainingWrites, final SimpleName finalRead) {
			return findVariables(currentNode, remainingReads, remainingWrites, finalRead, true);
		}

		private Set<SimpleName> findVariables(final ASTNode currentNode, final Set<SimpleName> remainingReads,
				final Set<SimpleName> remainingWrites, final SimpleName finalRead, final boolean hasToVisitLoops) {
			if (currentNode == null) {
				return Collections.emptySet();
			}

			Set<SimpleName> searchedVariables= new HashSet<>(remainingReads);
			searchedVariables.addAll(remainingWrites);
			searchedVariables.add(finalRead);
			VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(searchedVariables, hasToVisitLoops);
			currentNode.accept(varOccurrenceVisitor);

			return varOccurrenceVisitor.getFoundVariables();
		}

		private boolean isWriteValid(final SimpleName simpleName, final Set<SimpleName> unvisitedReads, final Set<SimpleName> assignmentWrites, final Set<SimpleName> concatenationWrites) {
			if (simpleName.getParent() instanceof Assignment) {
				Assignment assignment= (Assignment) simpleName.getParent();

				if (assignment.getParent() instanceof ExpressionStatement
						&& simpleName.getLocationInParent() == Assignment.LEFT_HAND_SIDE_PROPERTY) {
					if (ASTNodes.hasOperator(assignment, Assignment.Operator.PLUS_ASSIGN)) {
						assignmentWrites.add(simpleName);
						return true;
					}

					if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)) {
						InfixExpression concatenation= ASTNodes.as(assignment.getRightHandSide(), InfixExpression.class);

						if (concatenation != null
								&& ASTNodes.hasOperator(concatenation, InfixExpression.Operator.PLUS)) {
							SimpleName stringRead= ASTNodes.as(concatenation.getLeftOperand(), SimpleName.class);

							if (stringRead != null
									&& unvisitedReads.contains(stringRead)) {
								unvisitedReads.remove(stringRead);
								concatenationWrites.add(simpleName);
								return true;
							}
						}
					}
				}
			}

			return false;
		}
	}
}
