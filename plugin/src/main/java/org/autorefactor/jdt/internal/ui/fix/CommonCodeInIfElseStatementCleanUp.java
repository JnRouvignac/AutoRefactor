/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
 * Copyright (C) 2016 Sameer Misger - Make SonarQube more happy
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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTMatcherSameVariablesAndMethods;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.VarOccurrenceVisitor;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.text.edits.TextEditGroup;

/** See {@link #getDescription()} method. */
public class CommonCodeInIfElseStatementCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.CommonCodeInIfElseStatementCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.CommonCodeInIfElseStatementCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.CommonCodeInIfElseStatementCleanUp_reason;
	}

	// TODO handle switch statements
	// TODO also handle ternary operator, ConditionalExpression

	@Override
	public boolean visit(final IfStatement node) {
		if (node.getElseStatement() == null) {
			return true;
		}

		List<ASTNode> allCases= new ArrayList<>();
		List<List<Statement>> allCasesStatements= new ArrayList<>();

		// Collect all the if / else if / else if / ... / else cases
		if (collectAllCases(allCasesStatements, node, allCases)) {
			List<List<Statement>> caseStmtsToRemove= new ArrayList<>(allCasesStatements.size());

			// Initialize removedCaseStatements list
			for (int i= 0; i < allCasesStatements.size(); i++) {
				caseStmtsToRemove.add(new LinkedList<Statement>());
			}

			// If all cases exist
			ASTSemanticMatcher matcher= new ASTMatcherSameVariablesAndMethods();
			int minSize= minSize(allCasesStatements);
			List<Integer> casesToRefactor= getMatchingCases(allCasesStatements, matcher);

			if (casesToRefactor == null || casesToRefactor.size() <= 1) {
				return true;
			}

			// Identify matching statements starting from the end of each case
			for (int stmtIndex= 1; stmtIndex <= minSize; stmtIndex++) {
				if (!match(matcher, allCasesStatements, stmtIndex, casesToRefactor)) {
					break;
				}
				flagStmtsToRemove(allCasesStatements, stmtIndex, caseStmtsToRemove, casesToRefactor);
			}

			if (!hasVariableConflict(node, caseStmtsToRemove)) {
				removeIdenticalTrailingCode(node, allCases, allCasesStatements, caseStmtsToRemove, casesToRefactor);
				return false;
			}
		}

		return true;
	}

	private List<Integer> getMatchingCases(final List<List<Statement>> allCasesStatements,
			final ASTSemanticMatcher matcher) {
		List<Pair<Statement, List<Integer>>> matchingCases= new ArrayList<>();

		for (int i= 0; i < allCasesStatements.size(); i++) {
			boolean isMatching= false;
			Statement currentStatement= allCasesStatements.get(i).get(allCasesStatements.get(i).size() - 1);

			for (Pair<Statement, List<Integer>> pair : matchingCases) {
				if (ASTNodes.match(matcher, pair.getFirst(), currentStatement)) {
					pair.getSecond().add(i);
					isMatching= true;
					break;
				}
			}

			if (!isMatching) {
				Pair<Statement, List<Integer>> newPair= Pair.<Statement, List<Integer>>of(currentStatement, new ArrayList<>());
				newPair.getSecond().add(i);
				matchingCases.add(newPair);
			}
		}

		if (matchingCases.isEmpty()) {
			return null;
		}

		Collections.sort(matchingCases, new Comparator<Pair<Statement, List<Integer>>>() {
			@Override
			public int compare(final Pair<Statement, List<Integer>> o1, final Pair<Statement, List<Integer>> o2) {
				return Integer.compare(o2.getSecond().size(), o1.getSecond().size());
			}
		});
		Pair<Statement, List<Integer>> notFallingThroughCase= null;

		for (Pair<Statement, List<Integer>> matchingCase : matchingCases) {
			if (!ASTNodes.fallsThrough(matchingCase.getFirst())) {
				if (notFallingThroughCase != null) {
					return null;
				}

				notFallingThroughCase= matchingCase;
			}
		}

		if (notFallingThroughCase != null) {
			return notFallingThroughCase.getSecond();
		}

		return matchingCases.get(0).getSecond();
	}

	private void flagStmtsToRemove(final List<List<Statement>> allCasesStatements, final int stmtIndex,
			final List<List<Statement>> removedCaseStatements, final List<Integer> casesToRefactor) {
		for (int i : casesToRefactor) {
			List<Statement> caseStatements= allCasesStatements.get(i);
			Statement stmtToRemove= caseStatements.get(caseStatements.size() - stmtIndex);
			removedCaseStatements.get(i).add(stmtToRemove);
		}
	}

	private void removeIdenticalTrailingCode(final IfStatement node, final List<ASTNode> allCases,
			final List<List<Statement>> allCasesStatements, final List<List<Statement>> caseStmtsToRemove, final List<Integer> casesToRefactor) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		ASTNodeFactory ast= cuRewrite.getASTBuilder();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommonCodeInIfElseStatementCleanUp_description);

		// Remove the nodes common to all cases
		boolean[] areCasesRemovable= new boolean[allCasesStatements.size()];
		Arrays.fill(areCasesRemovable, false);
		removeStmtsFromCases(allCases, allCasesStatements, caseStmtsToRemove, areCasesRemovable, casesToRefactor);
		List<Statement> oneCaseToRemove= caseStmtsToRemove.get(casesToRefactor.get(0));

		if (allRemovable(areCasesRemovable, 0)) {
			if (ASTNodes.canHaveSiblings(node)) {
				insertIdenticalCode(node, oneCaseToRemove);

				rewrite.removeButKeepComment(node, group);
			} else {
				List<Statement> orderedStatements= new ArrayList<>(oneCaseToRemove.size());
				for (Statement stmtToRemove : oneCaseToRemove) {
					orderedStatements.add(0, ASTNodes.createMoveTarget(rewrite, stmtToRemove));
				}
				Block newBlock= ast.newBlock();
				newBlock.statements().addAll(orderedStatements);
				ASTNodes.replaceButKeepComment(rewrite, node, newBlock, group);
			}
		} else {
			// Remove empty cases
			for (int i : casesToRefactor) {
				ASTNode parent= allCases.get(i);

				if (areCasesRemovable[i]) {
					if (i == areCasesRemovable.length - 2 && !areCasesRemovable[i + 1]) {
						// Then clause is empty and there is only one else clause
						// => revert if statement
						IfStatement newIfStatement= ast.newIfStatement();
						newIfStatement.setExpression(ast.negate(((IfStatement) parent).getExpression(), true));
						newIfStatement.setThenStatement(ASTNodes.createMoveTarget(rewrite, ((IfStatement) parent).getElseStatement()));
						ASTNodes.replaceButKeepComment(rewrite, parent, newIfStatement, group);
						break;
					}

					if (allRemovable(areCasesRemovable, i)) {
						rewrite.remove(parent, group);
						break;
					}
					ASTNodes.replaceButKeepComment(rewrite, ((IfStatement) parent).getThenStatement(), ast.newBlock(), group);
				}
			}

			if (ASTNodes.canHaveSiblings(node)) {
				insertIdenticalCode(node, oneCaseToRemove);
			} else {
				List<Statement> orderedStatements= new ArrayList<>(oneCaseToRemove.size() + 1);
				for (Statement stmtToRemove : oneCaseToRemove) {
					orderedStatements.add(0, ASTNodes.createMoveTarget(rewrite, stmtToRemove));
				}
				orderedStatements.add(0, ASTNodes.createMoveTarget(rewrite, node));
				Block newBlock= ast.newBlock();
				newBlock.statements().addAll((Collection<Statement>) orderedStatements);
				ASTNodes.replaceButKeepComment(rewrite, node, newBlock, group);
			}
		}
	}

	private void insertIdenticalCode(final IfStatement node, final List<Statement> stmtsToRemove) {
		ASTRewrite rewrite= cuRewrite.getASTRewrite();
		TextEditGroup group= new TextEditGroup(MultiFixMessages.CommonCodeInIfElseStatementCleanUp_description);

		for (Statement stmtToRemove : stmtsToRemove) {
			rewrite.insertAfter(ASTNodes.createMoveTarget(rewrite, stmtToRemove), node, group);
		}
	}

	private boolean allRemovable(final boolean[] areCasesRemovable, final int start) {
		for (int i= start; i < areCasesRemovable.length; i++) {
			if (!areCasesRemovable[i]) {
				return false;
			}
		}

		return true;
	}

	private void removeStmtsFromCases(final List<ASTNode> allCases, final List<List<Statement>> allCasesStatements,
			final List<List<Statement>> removedCaseStatements, final boolean[] areCasesRemovable, final List<Integer> casesToRefactor) {
		for (int i : casesToRefactor) {
			List<Statement> removedStatements= removedCaseStatements.get(i);
			ASTNode parent= allCases.get(i);

			if (removedStatements.containsAll(allCasesStatements.get(i))
					&& (!(parent instanceof IfStatement) || ASTNodes.isPassiveWithoutFallingThrough(((IfStatement) parent).getExpression()))) {
				areCasesRemovable[i]= true;
			} else {
				TextEditGroup group= new TextEditGroup(MultiFixMessages.CommonCodeInIfElseStatementCleanUp_description);
				ASTRewrite rewrite= cuRewrite.getASTRewrite();

				rewrite.remove(removedStatements, group);
			}
		}
	}

	private boolean match(final ASTSemanticMatcher matcher, final List<List<Statement>> allCasesStatements, final int stmtIndex, final List<Integer> casesToRefactor) {
		List<Statement> firstCaseToRefactor= allCasesStatements.get(casesToRefactor.get(0));

		for (int i= 1; i < casesToRefactor.size(); i++) {
			List<Statement> anotherCaseToRefactor= allCasesStatements.get(casesToRefactor.get(i));

			if (!ASTNodes.match(matcher, firstCaseToRefactor.get(firstCaseToRefactor.size() - stmtIndex),
					anotherCaseToRefactor.get(anotherCaseToRefactor.size() - stmtIndex))) {
				return false;
			}
		}

		return true;
	}

	private int minSize(final List<List<Statement>> allCasesStatements) {
		if (allCasesStatements.isEmpty()) {
			throw new IllegalStateException(null, "allCasesStatements List must not be empty"); //$NON-NLS-1$
		}

		int min= allCasesStatements.get(0).size();

		for (List<Statement> statements : allCasesStatements) {
			min= Math.min(min, statements.size());
		}

		return min;
	}

	/**
	 * Collects all cases (if/else, if/else if/else, etc.) and returns whether all
	 * are covered.
	 *
	 * @param allCasesStatements the output collection for all the cases
	 * @param node     the {@link IfStatement} to examine
	 * @param allCases All the cases
	 * @return true if all cases (if/else, if/else if/else, etc.) are covered, false
	 *         otherwise
	 */
	private boolean collectAllCases(final List<List<Statement>> allCasesStatements, final IfStatement node, final List<ASTNode> allCases) {
		List<Statement> thenStatements= ASTNodes.asList(node.getThenStatement());
		List<Statement> elseStatements= ASTNodes.asList(node.getElseStatement());

		if (thenStatements.isEmpty() || elseStatements.isEmpty()) {
			// If the then or else clause is empty, then there is no common code whatsoever.
			// let other cleanups take care of removing empty blocks.
			return false;
		}

		allCases.add(node);
		allCasesStatements.add(thenStatements);

		if (elseStatements.size() == 1) {
			IfStatement is= ASTNodes.as(elseStatements.get(0), IfStatement.class);
			if (is != null) {
				return collectAllCases(allCasesStatements, is, allCases);
			}
		}

		allCases.add(node.getElseStatement());
		allCasesStatements.add(elseStatements);
		return true;
	}

	private boolean hasVariableConflict(final IfStatement node, final List<List<Statement>> casesStmtsToRemove) {
		Set<SimpleName> ifVariableNames= new HashSet<>();

		for (List<Statement> caseStmtsToRemove : casesStmtsToRemove) {
			for (Statement caseStmtToRemove : caseStmtsToRemove) {
				ifVariableNames.addAll(ASTNodes.getLocalVariableIdentifiers(caseStmtToRemove, false));
			}
		}

		for (Statement statement : ASTNodes.getNextSiblings(node)) {
			VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(ifVariableNames, true);
			varOccurrenceVisitor.traverseNodeInterruptibly(statement);

			if (varOccurrenceVisitor.isVarUsed()) {
				return true;
			}
		}

		return false;
	}
}
