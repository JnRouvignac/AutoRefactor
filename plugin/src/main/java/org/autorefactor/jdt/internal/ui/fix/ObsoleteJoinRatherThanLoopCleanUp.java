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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.ForLoops;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ContainerType;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.ForLoopContent;
import org.autorefactor.jdt.internal.corext.dom.ForLoops.IterationType;
import org.autorefactor.jdt.internal.corext.dom.OrderedInfixExpression;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class ObsoleteJoinRatherThanLoopCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.ObsoleteJoinRatherThanLoopCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.ObsoleteJoinRatherThanLoopCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.ObsoleteJoinRatherThanLoopCleanUp_reason;
	}

	@Override
	public boolean isJavaVersionSupported(final Release javaSeRelease) {
		return javaSeRelease.getMinorVersion() >= 8;
	}

	@Override
	public boolean visit(final Block node) {
		BuilderForAndUseVisitor builderForAndUseVisitor= new BuilderForAndUseVisitor();
		builderForAndUseVisitor.visitNode(node);
		return builderForAndUseVisitor.result;
	}

	private final class BuilderForAndUseVisitor extends BlockSubVisitor {
		@Override
		public boolean visit(final ForStatement node) {
			ForLoopContent loopContent= ForLoops.iterateOverContainer(node);

			if (loopContent != null
					&& loopContent.isLoopingForward()
					&& loopContent.getContainerType() == ContainerType.ARRAY) {
				return maybeRefactorLoop(node, loopContent.getContainerVariable(), loopContent.getIterationType(), loopContent.getLoopVariable(), node.getBody());
			}

			return true;
		}

		@Override
		public boolean visit(final EnhancedForStatement node) {
			return maybeRefactorLoop(node, node.getExpression(), IterationType.FOREACH, node.getParameter().getName(), node.getBody());
		}

		private boolean maybeRefactorLoop(final Statement node, final Expression containerVariable,
				final IterationType iterationType, final Name loopVariable, final Statement body) {
			List<Statement> statements= ASTNodes.asList(body);
			Statement previousStatement= ASTNodes.getPreviousSibling(node);

			if (result
					&& previousStatement != null
					&& containerVariable != null
					&& ASTNodes.hasType(containerVariable, String[].class.getCanonicalName())
					&& (statements.size() == 2 || statements.size() == 3)) {
				AtomicBoolean isInitedToTrueAtomic= new AtomicBoolean();

				SimpleName builder= getBuilder(previousStatement);
				SimpleName booleanForInterval= getBoolean(previousStatement, isInitedToTrueAtomic);
				Statement earlierStatement= ASTNodes.getPreviousSibling(previousStatement);
				Statement booleanStatement= null;
				Statement builderStatement= null;

				if (earlierStatement != null) {
					if (builder != null) {
						booleanForInterval= getBoolean(earlierStatement, isInitedToTrueAtomic);
						booleanStatement= earlierStatement;
						builderStatement= previousStatement;
					} else if (booleanForInterval != null) {
						builder= getBuilder(earlierStatement);
						builderStatement= earlierStatement;
						booleanStatement= previousStatement;
					}
				} else {
					builderStatement= previousStatement;
				}

				boolean isInitedToTrue= isInitedToTrueAtomic.get();

				if (builder == null) {
					return true;
				}

				String builderClass= ASTNodes.hasType(builder, StringBuffer.class.getCanonicalName()) ? StringBuffer.class.getCanonicalName() : StringBuilder.class.getCanonicalName();
				Set<SimpleName> builderUses= new HashSet<>(3);
				Set<SimpleName> booleanUses= new HashSet<>(2);

				Statement itemAppendingStatement= null;

				for (Statement statement : statements) {
					if (isItemAppendingValid(containerVariable, loopVariable, iterationType, builderClass, statement, builderUses)) {
						itemAppendingStatement= statement;
						break;
					}
				}

				if (itemAppendingStatement != null) {
					Statement delimiterConditionStatement= null;
					Expression delimiter= null;
					boolean isDelimiterFirst= true;

					for (Statement statement : statements) {
						if (itemAppendingStatement == statement) {
							isDelimiterFirst= false;
						} else {
							delimiter= getDelimiter(iterationType, containerVariable, loopVariable,
									isInitedToTrue, isDelimiterFirst, statement, builderClass, booleanUses, builderUses, statements.size() == 3);

							if (delimiter != null) {
								delimiterConditionStatement= statement;
								break;
							}
						}
					}

					if (statements.size() == 3) {
						Statement booleanShiftStatement= null;

                        if (booleanUses.isEmpty() || !isDelimiterFirst) {
							return true;
						}

						boolean isDelimiterConditionVisited= false;

						for (Statement statement : statements) {
							if (delimiterConditionStatement == statement) {
								isDelimiterConditionVisited= true;
							} else if (isDelimiterConditionVisited
									&& itemAppendingStatement != statement
									&& isBooleanShifterValid(isInitedToTrue, statement, booleanUses)) {
								booleanShiftStatement= statement;
								break;
							}
						}

						if (booleanShiftStatement == null) {
							return true;
						}
					}

					List<SimpleName> readsToRefactor= new ArrayList<>();

					if (delimiterConditionStatement != null
							&& (booleanUses.isEmpty() || isBooleanUseValid(booleanForInterval, booleanUses))
							&& isConcatenationUseValid(node, builder, builderClass, readsToRefactor, builderUses)) {
						replaceWithStringJoin(node, containerVariable, booleanStatement, builderStatement, builder, delimiter,
								readsToRefactor, booleanUses);
						result= false;
						return false;
					}
				}
			}

			return true;
		}

		private Expression getDelimiter(final IterationType iterationType, final Expression containerVariable,
				final Name loopVariable, final boolean isInitedToTrue, final boolean isDelimiterFirst,
				final Statement statement, final String builderClass, final Set<SimpleName> booleanUses, final Set<SimpleName> builderUses, final boolean isBooleanShiftDone) {
			IfStatement delimiterCondition= ASTNodes.as(statement, IfStatement.class);

			if (delimiterCondition != null) {
				Expression delimiter= null;
				ExpressionStatement delimiterStatement= ASTNodes.as(delimiterCondition.getThenStatement(), ExpressionStatement.class);

				if (delimiterCondition.getElseStatement() == null
						&& delimiterStatement != null) {
					delimiter= getDelimiterAppend(builderClass, delimiterStatement, builderUses);
				}

				if (isConditionOnIndexValid(iterationType, containerVariable, loopVariable, isDelimiterFirst, delimiterCondition, builderUses)) {
					if (isBooleanShiftDone) {
						return null;
					}

					return delimiter;
				}

				delimiter= getDelimiterWithBooleanShifter(delimiterCondition, isInitedToTrue, isDelimiterFirst, builderClass, builderUses, booleanUses, isBooleanShiftDone);

				if (delimiter != null) {
					return delimiter;
				}
			}

			return null;
		}

		private boolean isConditionOnIndexValid(final IterationType iterationType, final Expression containerVariable,
				final Name loopVariable, final boolean isDelimiterFirst, final IfStatement delimiterCondition, final Set<SimpleName> builderUses) {
			InfixExpression conditionForDelimiter= ASTNodes.as(delimiterCondition.getExpression(), InfixExpression.class);

			if (conditionForDelimiter != null) {
				OrderedInfixExpression<MethodInvocation, Expression> emptyLength= ASTNodes.orderedInfix(conditionForDelimiter, MethodInvocation.class, Expression.class);

				if (emptyLength != null
						&& ASTNodes.usesGivenSignature(emptyLength.getFirstOperand(), CharSequence.class.getCanonicalName(), "length") //$NON-NLS-1$
						&& ASTNodes.as(emptyLength.getFirstOperand().getExpression(), SimpleName.class) != null) {
					Long literal= ASTNodes.getIntegerLiteral(emptyLength.getSecondOperand());

					if (literal != null) {
						if (literal.equals(0L) && Arrays.asList(InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.GREATER).contains(emptyLength.getOperator())
								|| literal.equals(1L) && InfixExpression.Operator.GREATER_EQUALS.equals(emptyLength.getOperator())) {
							builderUses.add(ASTNodes.as(emptyLength.getFirstOperand().getExpression(), SimpleName.class));
							return true;
						}
					}
				}

				if (IterationType.INDEX.equals(iterationType)) {
					if (isDelimiterFirst) {
						OrderedInfixExpression<SimpleName, Expression> orderedConditionForDelimiter= ASTNodes.orderedInfix(conditionForDelimiter, SimpleName.class, Expression.class);

						if (orderedConditionForDelimiter != null) {
							Long literal= ASTNodes.getIntegerLiteral(orderedConditionForDelimiter.getSecondOperand());

							if (ASTNodes.isSameVariable(loopVariable, orderedConditionForDelimiter.getFirstOperand())
									&& literal != null) {
								if (literal.equals(0L) && Arrays.asList(InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.GREATER).contains(orderedConditionForDelimiter.getOperator())
										|| literal.equals(1L) && InfixExpression.Operator.GREATER_EQUALS.equals(orderedConditionForDelimiter.getOperator())) {
									return true;
								}
							}
						}
					} else {
						OrderedInfixExpression<SimpleName, InfixExpression> orderedConditionForDelimiter= ASTNodes.orderedInfix(conditionForDelimiter, SimpleName.class, InfixExpression.class);

						if (orderedConditionForDelimiter != null
								&& ASTNodes.isSameVariable(loopVariable, orderedConditionForDelimiter.getFirstOperand())) {
							InfixExpression beyondScope= orderedConditionForDelimiter.getSecondOperand();
							QualifiedName limit= ASTNodes.as(beyondScope.getLeftOperand(), QualifiedName.class);

							if (InfixExpression.Operator.MINUS.equals(beyondScope.getOperator())
									&& limit != null
									&& "length".equals(limit.getName().getIdentifier()) //$NON-NLS-1$
									&& ASTNodes.isSameVariable(containerVariable, limit.getQualifier())) {
								Long literal= ASTNodes.getIntegerLiteral(beyondScope.getRightOperand());

								if (Arrays.asList(
										InfixExpression.Operator.NOT_EQUALS,
										InfixExpression.Operator.LESS).contains(orderedConditionForDelimiter.getOperator()) && literal.equals(1L)
										|| InfixExpression.Operator.LESS_EQUALS.equals(orderedConditionForDelimiter.getOperator()) && literal.equals(2L)) {
									return true;
								}
							}
						}
					}
				}
			}

			return false;
		}

		private boolean isConcatenationUseValid(final Statement node, final SimpleName builder, final String builderClass,
				final List<SimpleName> readsToRefactor, final Set<SimpleName> builderUses) {
			for (SimpleName simpleName : builderUses) {
				if (!ASTNodes.isSameVariable(builder, simpleName)) {
					return false;
				}
			}

			VarDefinitionsUsesVisitor varOccurrencesVisitor= new VarDefinitionsUsesVisitor((IVariableBinding) builder.resolveBinding(),
			startNode, true);

			List<SimpleName> reads= varOccurrencesVisitor.getReads();
			List<SimpleName> writes= varOccurrencesVisitor.getWrites();
			reads.removeAll(builderUses);
			writes.removeAll(builderUses);

			if (writes.size() != 1 || reads.isEmpty()) {
				return false;
			}

			for (SimpleName read : reads) {
				if (!isReadValid(node, builderClass, readsToRefactor, read)) {
					return false;
				}
			}

			return true;
		}

		private boolean isReadValid(final Statement node, final String builderClass,
				final List<SimpleName> readsToRefactor, final SimpleName read) {
			if (!ASTNodes.isParent(read, node) && read.getParent() instanceof MethodInvocation) {
				MethodInvocation methodInvocation= (MethodInvocation) read.getParent();

				if (read.getLocationInParent() == MethodInvocation.EXPRESSION_PROPERTY) {
					if (ASTNodes.usesGivenSignature(methodInvocation, builderClass, "toString")) { //$NON-NLS-1$
						readsToRefactor.add(read);
						return true;
					}

					if (ASTNodes.usesGivenSignature(methodInvocation, CharSequence.class.getCanonicalName(), "charAt", int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, CharSequence.class.getCanonicalName(), "chars") //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, CharSequence.class.getCanonicalName(), "length") //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, CharSequence.class.getCanonicalName(), "codePoints") //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, CharSequence.class.getCanonicalName(), "subSequence", int.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "codePointAt", int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "codePointBefore", int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "codePointCount", int.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "getChars", int.class.getCanonicalName(), int.class.getCanonicalName(), char[].class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "indexOf", String.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "indexOf", String.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "lastIndexOf", String.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "lastIndexOf", String.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "offsetByCodePoints", int.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "substring", int.class.getCanonicalName()) //$NON-NLS-1$
							|| ASTNodes.usesGivenSignature(methodInvocation, builderClass, "substring", int.class.getCanonicalName(), int.class.getCanonicalName())) { //$NON-NLS-1$
						return true;
					}
				}
			}

			return false;
		}

		private boolean isBooleanUseValid(final SimpleName booleanForInterval, final Set<SimpleName> booleanUses) {
			if (booleanForInterval == null) {
				return false;
			}

			for (SimpleName simpleName : booleanUses) {
				if (!ASTNodes.isSameVariable(booleanForInterval, simpleName)) {
					return false;
				}
			}

			VarDefinitionsUsesVisitor booleanOccurrencesVisitor= new VarDefinitionsUsesVisitor((IVariableBinding) booleanForInterval.resolveBinding(),
			startNode, true);

			Set<SimpleName> actualBooleanOccurences= new HashSet<>(booleanOccurrencesVisitor.getReads());
			actualBooleanOccurences.addAll(booleanOccurrencesVisitor.getWrites());

			if (!actualBooleanOccurences.remove(booleanForInterval)) {
				return false;
			}

			return actualBooleanOccurences.containsAll(booleanUses) && booleanUses.containsAll(actualBooleanOccurences);
		}

		private Expression getDelimiterWithBooleanShifter(final IfStatement delimiterCondition, final boolean isInitedToTrue,
				final boolean isDelimiterFirst, final String builderClass, final Set<SimpleName> builderUses, final Set<SimpleName> booleanUses, final boolean isBooleanShiftDone) {
			boolean isConditionReversed= true;
			SimpleName booleanForDelimiterAgain= ASTNodes.as(delimiterCondition.getExpression(), SimpleName.class);
			PrefixExpression notBooleanForDelimiterAgain= ASTNodes.as(delimiterCondition.getExpression(), PrefixExpression.class);

			if (isDelimiterFirst) {
				if (notBooleanForDelimiterAgain != null && ASTNodes.hasOperator(notBooleanForDelimiterAgain, PrefixExpression.Operator.NOT)) {
					isConditionReversed= false;
					booleanForDelimiterAgain= ASTNodes.as(notBooleanForDelimiterAgain.getOperand(), SimpleName.class);
				}

				List<Statement> thenStatements= ASTNodes.asList(delimiterCondition.getThenStatement());

				if (booleanForDelimiterAgain != null
						&& thenStatements.size() == 1) {
					if (isBooleanShiftDone) {
						if (delimiterCondition.getElseStatement() == null
								&& isInitedToTrue != isConditionReversed) {
							Expression appendDelimiter= getDelimiterAppend(builderClass, thenStatements.get(0), builderUses);

							if (appendDelimiter != null) {
								booleanUses.add(booleanForDelimiterAgain);
								return appendDelimiter;
							}
						}
					} else if (delimiterCondition.getElseStatement() != null) {
						List<Statement> elseStatements= ASTNodes.asList(delimiterCondition.getElseStatement());

						if (elseStatements.size() == 1) {
							boolean isBooleanShifterValid;
							Expression appendDelimiter;

							if (isInitedToTrue == isConditionReversed) {
								isBooleanShifterValid= isBooleanShifterValid(isInitedToTrue, thenStatements.get(0), booleanUses);
								appendDelimiter= getDelimiterAppend(builderClass, elseStatements.get(0), builderUses);
							} else {
								appendDelimiter= getDelimiterAppend(builderClass, thenStatements.get(0), builderUses);
								isBooleanShifterValid= isBooleanShifterValid(isInitedToTrue, elseStatements.get(0), booleanUses);
							}

							if (isBooleanShifterValid && appendDelimiter != null) {
								booleanUses.add(booleanForDelimiterAgain);
								return appendDelimiter;
							}
						}
					}
				}
			}

			return null;
		}

		private Expression getDelimiterAppend(final String builderClass, final Statement appendDelimiter, final Set<SimpleName> builderUses) {
			MethodInvocation appendInvocation= getAppendInvocation(ASTNodes.as(appendDelimiter, ExpressionStatement.class), builderClass, builderUses);

			if (ASTNodes.usesGivenSignature(appendInvocation, builderClass, "append", char.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(appendInvocation, builderClass, "append", Character.class.getCanonicalName()) //$NON-NLS-1$
					|| ASTNodes.usesGivenSignature(appendInvocation, builderClass, "append", String.class.getCanonicalName())) { //$NON-NLS-1$
				Expression delimiter= (Expression) appendInvocation.arguments().get(0);

				if (ASTNodes.isHardCoded(delimiter)) {
					return delimiter;
				}
			}

			return null;
		}

		private boolean isBooleanShifterValid(final boolean isInitedToTrue, final Statement statement,
				final Set<SimpleName> booleanUses) {
			Assignment changeBoolean= ASTNodes.asExpression(statement, Assignment.class);

			if (changeBoolean != null
					&& ASTNodes.hasOperator(changeBoolean, Assignment.Operator.ASSIGN)
					&& ASTNodes.as(changeBoolean.getLeftHandSide(), SimpleName.class) != null) {
				Boolean booleanInstantiation= ASTNodes.getBooleanLiteral(changeBoolean.getRightHandSide());

				if (booleanInstantiation != null && isInitedToTrue != booleanInstantiation) {
					booleanUses.add(ASTNodes.as(changeBoolean.getLeftHandSide(), SimpleName.class));
					return true;
				}
			}

			return false;
		}

		private boolean isItemAppendingValid(final Expression containerVariable,
				final Name loopVariable, final IterationType iterationType,
				final String builderClass, final Statement appendingStatement,
				final Set<SimpleName> builderUses) {
			ExpressionStatement appendDelimiter= ASTNodes.as(appendingStatement, ExpressionStatement.class);

			if (appendDelimiter != null) {
				MethodInvocation appendingElement= getAppendInvocation(appendDelimiter, builderClass, builderUses);

				if (appendingElement != null
						&& ASTNodes.usesGivenSignature(appendingElement, builderClass, "append", String.class.getCanonicalName())) { //$NON-NLS-1$
					Expression itemToAppend= (Expression) appendingElement.arguments().get(0);

					if (IterationType.FOREACH.equals(iterationType)) {
						return ASTNodes.isSameLocalVariable(loopVariable, itemToAppend);
					}

					if (IterationType.INDEX.equals(iterationType)) {
						ArrayAccess arrayAccess= ASTNodes.as(itemToAppend, ArrayAccess.class);

						if (arrayAccess != null
								&& isSameVariable(arrayAccess, containerVariable, loopVariable)) {
							return true;
						}
					}
				}
			}

			return false;
		}

		private MethodInvocation getAppendInvocation(final ExpressionStatement appending, final String builderClass, final Set<SimpleName> builderUses) {
			if (appending != null) {
				Assignment assignDelimiter= ASTNodes.as(appending.getExpression(), Assignment.class);

				MethodInvocation appendDelimiter;
				if (assignDelimiter != null) {
					appendDelimiter= ASTNodes.as(assignDelimiter.getRightHandSide(), MethodInvocation.class);
					SimpleName targetVar= ASTNodes.as(assignDelimiter.getLeftHandSide(), SimpleName.class);

					if (appendDelimiter == null
							|| !ASTNodes.hasOperator(assignDelimiter, Assignment.Operator.ASSIGN)
							|| !ASTNodes.hasType(targetVar, builderClass)
							|| !ASTNodes.isSameVariable(appendDelimiter.getExpression(), targetVar)) {
						return null;
					}

					builderUses.add(targetVar);
				} else {
					appendDelimiter= ASTNodes.as(appending.getExpression(), MethodInvocation.class);
				}

				if (appendDelimiter != null
						&& ASTNodes.is(appendDelimiter.getExpression(), SimpleName.class)) {
					builderUses.add(ASTNodes.as(appendDelimiter.getExpression(), SimpleName.class));
					return appendDelimiter;
				}
			}

			return null;
		}

		private SimpleName getBuilder(final Statement statement) {
			VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(statement);

			if (fragment != null
					&& ASTNodes.hasType(fragment.getName(), StringBuilder.class.getCanonicalName(), StringBuffer.class.getCanonicalName())) {
				ClassInstanceCreation builderInstantiation= ASTNodes.as(fragment.getInitializer(), ClassInstanceCreation.class);

				if (builderInstantiation != null
						&& ASTNodes.hasType(builderInstantiation, StringBuilder.class.getCanonicalName(), StringBuffer.class.getCanonicalName())
						&& Utils.isEmpty(builderInstantiation.arguments())
						&& Utils.isEmpty(builderInstantiation.typeArguments())
						&& builderInstantiation.getAnonymousClassDeclaration() == null
						&& Utils.equalNotNull(fragment.resolveBinding().getType(), builderInstantiation.resolveTypeBinding())) {
					return fragment.getName();
				}
			}

			return null;
		}

		private SimpleName getBoolean(final Statement statement, final AtomicBoolean isInitedToTrue) {
			VariableDeclarationFragment fragment= ASTNodes.getUniqueFragment(statement);

			if (fragment != null
					&& ASTNodes.hasType(fragment.getName(), boolean.class.getCanonicalName(), Boolean.class.getCanonicalName())) {
				Boolean booleanInstantiation= ASTNodes.getBooleanLiteral(fragment.getInitializer());

				if (booleanInstantiation != null) {
					isInitedToTrue.lazySet(booleanInstantiation);
					return fragment.getName();
				}
			}

			return null;
		}

		private boolean isSameVariable(final ArrayAccess arrayAccess, final Expression containerVariable,
				final Name loopVariable) {
			return arrayAccess != null && ASTNodes.isSameVariable(arrayAccess.getArray(), containerVariable)
					&& ASTNodes.isSameLocalVariable(arrayAccess.getIndex(), loopVariable);
		}

		private void replaceWithStringJoin(final Statement node, final Expression containerVariable,
				final Statement booleanStatement, final Statement builderStatement, final SimpleName builder,
				final Expression delimiter, final List<SimpleName> readsToRefactor, final Set<SimpleName> booleanUses) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			ASTNodeFactory ast= cuRewrite.getASTBuilder();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.ObsoleteJoinRatherThanLoopCleanUp_description);

			Expression copyOfDelimiter= ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(delimiter));

			if (!ASTNodes.hasType(delimiter, String.class.getCanonicalName())) {
				MethodInvocation valueOfMethod= ast.newMethodInvocation();
				valueOfMethod.setExpression(ASTNodeFactory.newName(ast, String.class.getSimpleName()));
				valueOfMethod.setName(ast.newSimpleName("valueOf")); //$NON-NLS-1$
				valueOfMethod.arguments().add(copyOfDelimiter);
				copyOfDelimiter= valueOfMethod;
			}

			MethodInvocation joinMethod= ast.newMethodInvocation();
			joinMethod.setExpression(ASTNodeFactory.newName(ast, String.class.getSimpleName()));
			joinMethod.setName(ast.newSimpleName("join")); //$NON-NLS-1$
			joinMethod.arguments().add(copyOfDelimiter);
			joinMethod.arguments().add(ASTNodes.createMoveTarget(rewrite, ASTNodes.getUnparenthesedExpression(containerVariable)));

			VariableDeclarationStatement joinStatement= ast.declareStatement(ast.type(String.class.getSimpleName()), ASTNodes.createMoveTarget(rewrite, builder), joinMethod);
			List<ASTNode> varModifiers= joinStatement.modifiers();
			List<ASTNode> modifiers= ASTNodes.as(builderStatement, VariableDeclarationStatement.class).modifiers();

			varModifiers.clear();
			varModifiers.addAll(ASTNodes.createMoveTarget(rewrite, modifiers));

			if (!booleanUses.isEmpty()) {
				ASTNodes.removeButKeepComment(rewrite, booleanStatement, group);
			}

			ASTNodes.removeButKeepComment(rewrite, builderStatement, group);
			ASTNodes.replaceButKeepComment(rewrite, node, joinStatement, group);

			for (SimpleName readToRefactor : readsToRefactor) {
				ASTNodes.replaceButKeepComment(rewrite, readToRefactor.getParent(), ASTNodes.createMoveTarget(rewrite, readToRefactor), group);
			}
		}
	}
}
