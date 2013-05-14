/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;

/**
 * Factorize common code in all if / else if / else statements either at the
 * start of each blocks or at the end. Could actually end up completely removing
 * an if statement.
 */
public class CommonCodeInIfElseStatementRefactoring extends ASTVisitor
		implements IJavaRefactoring {

	private RefactoringContext ctx;

	public CommonCodeInIfElseStatementRefactoring() {
		super();
	}

	public void setRefactoringContext(RefactoringContext ctx) {
		this.ctx = ctx;
	}

	// TODO handle switch statements
	// TODO handle clauses in catch blocks (also useful for java 7 with
	// multi-catch)
	// TODO also handle ternary operator, ConditionalExpression
	// TODO move to IfStatementRefactoring??

	@Override
	public boolean visit(IfStatement node) {
		if (isElseStatementOfParentIf(node)) {
			return ASTHelper.VISIT_SUBTREE;
		}

		final List<List<Statement>> allCasesStmts = new ArrayList<List<Statement>>();
		final List<List<ASTNode>> removedCaseStmts = new LinkedList<List<ASTNode>>();

		// collect all the if / else if / else if / ... / else cases
		if (collectAllCases(allCasesStmts, node)) {
			// initialize removedCaseStmts list
			for (int i = 0; i < allCasesStmts.size(); i++) {
				removedCaseStmts.add(new LinkedList<ASTNode>());
			}
			// if all cases exist
			final ASTMatcher matcher = new ASTMatcher();
			final int minSize = minSize(allCasesStmts);
			final List<Statement> caseStmts = allCasesStmts.get(0);

			// identify matching statements starting from the beginning of each
			// case
			for (int stmtIndex = 0; stmtIndex < minSize; stmtIndex++) {
				if (!match(matcher, allCasesStmts, true, stmtIndex, 0,
						allCasesStmts.size())) {
					break;
				}
				this.ctx.getRefactorings().insertBefore(
						ASTHelper.copySubtree(this.ctx.getAST(),
								caseStmts.get(stmtIndex)), node);
				removeStmts(allCasesStmts, true, stmtIndex, removedCaseStmts);
			}

			// identify matching statements starting from the end of each case
			for (int stmtIndex = 1; 0 <= minSize - stmtIndex; stmtIndex++) {
				if (!match(matcher, allCasesStmts, false, stmtIndex, 0,
						allCasesStmts.size())
						|| anyContains(removedCaseStmts, allCasesStmts,
								stmtIndex)) {
					break;
				}
				this.ctx.getRefactorings().insertAfter(
						ASTHelper.copySubtree(this.ctx.getAST(),
								caseStmts.get(caseStmts.size() - stmtIndex)),
						node);
				removeStmts(allCasesStmts, false, stmtIndex, removedCaseStmts);
			}

			// remove the nodes common to all cases
			final List<Boolean> areCasesEmpty = new ArrayList<Boolean>(
					allCasesStmts.size());
			for (int i = 0; i < allCasesStmts.size(); i++) {
				areCasesEmpty.add(Boolean.FALSE);
			}
			removeStmtsFromCases(allCasesStmts, removedCaseStmts, areCasesEmpty);

			if (allEmpty(areCasesEmpty)) {
				// TODO JNR keep comments
				this.ctx.getRefactorings().remove(node);
				// } else if (areCasesEmpty.get(0)) {
				// // TODO JNR then clause is empty => revert if statement
				// this.ctx.getRefactorings().replace(node.getThenStatement(),
				// this.ctx.getAST().newBlock());
			} else {
				// remove empty cases
				if (areCasesEmpty.get(0)) {
					// TODO JNR then clause is empty => revert if statement
					this.ctx.getRefactorings().replace(node.getThenStatement(),
							this.ctx.getAST().newBlock());
				}
				for (int i = 1; i < areCasesEmpty.size(); i++) {
					if (areCasesEmpty.get(i)) {
						final Statement firstStmt = allCasesStmts.get(i).get(0);
						this.ctx.getRefactorings().remove(findNodeToRemove(firstStmt,
								firstStmt.getParent()));
					}
				}
			}
		}
		return ASTHelper.VISIT_SUBTREE;
	}

	private boolean isElseStatementOfParentIf(IfStatement node) {
		final ASTNode parent = node.getParent();
		if (parent instanceof IfStatement) {
			final IfStatement is = (IfStatement) parent;
			if (is.getElseStatement() == node) {
				return true;
			}
		} else if (parent instanceof Block) {
			final Block b = (Block) parent;
			if (b.getParent() instanceof IfStatement
					&& b.statements().size() == 1
					&& b.statements().get(0) == node) {
				return true;
			}
		}
		return false;
	}

	private ASTNode findNodeToRemove(ASTNode node, ASTNode parent) {
		if (parent instanceof IfStatement) {
			return node;
		}
		if (parent instanceof Block) {
			final Block b = (Block) parent;
			return findNodeToRemove(b, b.getParent());
		}
		throw new IllegalStateException("Not implemented for parent of "
				+ parent.getClass());
	}

	private boolean allEmpty(List<Boolean> areCasesEmpty) {
		for (int i = 0; i < areCasesEmpty.size(); i++) {
			if (!areCasesEmpty.get(i)) {
				return false;
			}
		}
		return true;
	}

	private void removeStmtsFromCases(List<List<Statement>> allCasesStmts,
			List<List<ASTNode>> removedCaseStmts, List<Boolean> areCasesEmpty) {
		for (int i = 0; i < allCasesStmts.size(); i++) {
			final List<ASTNode> removedStmts = removedCaseStmts.get(i);
			if (removedStmts.containsAll(allCasesStmts.get(i))) {
				areCasesEmpty.set(i, Boolean.TRUE);
			} else {
				this.ctx.getRefactorings().remove(removedStmts);
			}
		}
	}

	private boolean anyContains(List<List<ASTNode>> removedCaseStmts,
			List<List<Statement>> allCasesStmts, int stmtIndex) {
		for (int i = 0; i < allCasesStmts.size(); i++) {
			final List<Statement> caseStmts = allCasesStmts.get(i);
			if (removedCaseStmts.get(i).contains(
					caseStmts.get(caseStmts.size() - stmtIndex))) {
				return true;
			}
		}
		return false;
	}

	private void removeStmts(List<List<Statement>> allCasesStmts,
			boolean forwardCase, int stmtIndex,
			List<List<ASTNode>> removedCaseStmts) {
		for (int i = 0; i < allCasesStmts.size(); i++) {
			final List<Statement> caseStmts = allCasesStmts.get(i);
			final Statement stmtToRemove;
			if (forwardCase) {
				stmtToRemove = caseStmts.get(stmtIndex);
			} else {
				stmtToRemove = caseStmts.get(caseStmts.size() - stmtIndex);
			}
			removedCaseStmts.get(i).add(stmtToRemove);
		}
	}

	private boolean match(ASTMatcher matcher,
			List<List<Statement>> allCasesStmts, boolean matchForward,
			int stmtIndex, int startIndex, int endIndex) {
		if (startIndex == endIndex || startIndex == endIndex - 1) {
			return true;
		}
		final int comparisonIndex;
		if (endIndex - startIndex > 1) {
			final int pivotIndex = (endIndex + startIndex + 1) / 2;
			if (!match(matcher, allCasesStmts, matchForward, stmtIndex,
					startIndex, pivotIndex)
					|| !match(matcher, allCasesStmts, matchForward, stmtIndex,
							pivotIndex, endIndex)) {
				return false;
			}
			comparisonIndex = pivotIndex;
		} else {
			comparisonIndex = endIndex - 1;
		}

		final List<Statement> caseStmts1 = allCasesStmts.get(startIndex);
		final List<Statement> caseStmts2 = allCasesStmts.get(comparisonIndex);
		if (matchForward) {
			return ASTHelper.match(matcher, caseStmts1.get(stmtIndex),
					caseStmts2.get(stmtIndex));
		} else {
			return ASTHelper.match(matcher,
					caseStmts1.get(caseStmts1.size() - stmtIndex),
					caseStmts2.get(caseStmts2.size() - stmtIndex));
		}
	}

	private int minSize(List<List<Statement>> allCasesStmts) {
		if (allCasesStmts.size() == 0) {
			throw new IllegalStateException(
					"allCasesStmts List must not be empty");
		}
		int min = Integer.MAX_VALUE;
		for (List<Statement> stmts : allCasesStmts) {
			min = Math.min(min, stmts.size());
		}
		if (min == Integer.MAX_VALUE) {
			throw new IllegalStateException(
					"The minimum size should never have been equal to Integer.MAX_VALUE");
		}
		return min;
	}

	/**
	 * @param allCases
	 * @param node
	 * @return true if all cases (if/else, if/else if/else, etc.) are covered,
	 *         false otherwise
	 */
	private boolean collectAllCases(List<List<Statement>> allCases,
			IfStatement node) {
		final List<Statement> thenStmts = ASTHelper.asList(node.getThenStatement());
		final List<Statement> elseStmts = ASTHelper.asList(node.getElseStatement());
		if (thenStmts.isEmpty() || elseStmts.isEmpty()) {
			// if the then or else clause is empty, then there is no common code whatsoever.
			// let other refactorings take care of removing empty blocks.
			return false;
		}

		allCases.add(thenStmts);
		if (elseStmts.size() == 1) {
			final IfStatement is = ASTHelper.as(elseStmts.get(0), IfStatement.class);
			if (is != null) {
				return collectAllCases(allCases, is);
			}
		}
		allCases.add(elseStmts);
		return true;
	}

	public Refactorings getRefactorings(CompilationUnit astRoot) {
		astRoot.accept(this);
		return this.ctx.getRefactorings();
	}
}
