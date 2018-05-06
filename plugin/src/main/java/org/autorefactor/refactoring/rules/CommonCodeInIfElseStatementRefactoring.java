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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.asList;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTHelper;
import org.autorefactor.refactoring.ASTMatcherSameVariablesAndMethods;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
/** See {@link #getDescription()} method. */
public class CommonCodeInIfElseStatementRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Extract common code in if else statement";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Factorizes common code in all if / else if / else statements"
            + " either at the start of each blocks or at the end.\n"
            + "Ultimately it can completely remove the if statement condition.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces the coding, reading, debugging and testing cost.";
    }

    // TODO handle switch statements
    // TODO also handle ternary operator, ConditionalExpression

    @Override
    public boolean visit(IfStatement node) {
        if (node.getElseStatement() == null) {
            return VISIT_SUBTREE;
        }
        if (!(node.getParent() instanceof Block)) {
            // TODO current code does not handle common code in if / else if statements
            // when not inside curly braces
            return VISIT_SUBTREE;
        }

        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        final List<List<Statement>> allCasesStmts = new ArrayList<List<Statement>>();
        final List<List<ASTNode>> removedCaseStmts = new LinkedList<List<ASTNode>>();

        // Collect all the if / else if / else if / ... / else cases
        if (collectAllCases(allCasesStmts, node)) {
            // initialize removedCaseStmts list
            for (int i = 0; i < allCasesStmts.size(); i++) {
                removedCaseStmts.add(new LinkedList<ASTNode>());
            }
            // If all cases exist
            final ASTSemanticMatcher matcher = new ASTMatcherSameVariablesAndMethods();
            final int minSize = minSize(allCasesStmts);
            final List<Statement> caseStmts = allCasesStmts.get(0);

            boolean result = VISIT_SUBTREE;

            // Identify matching statements starting from the beginning of each case
            for (int stmtIndex = 0; stmtIndex < minSize; stmtIndex++) {
                if (!match(matcher, allCasesStmts, true, stmtIndex, 0, allCasesStmts.size())) {
                    break;
                }
                r.insertBefore(b.copy(caseStmts.get(stmtIndex)), node);
                removeStmts(allCasesStmts, true, stmtIndex, removedCaseStmts);
                result = DO_NOT_VISIT_SUBTREE;
            }

            // Identify matching statements starting from the end of each case
            for (int stmtIndex = 1; 0 <= minSize - stmtIndex; stmtIndex++) {
                if (!match(matcher, allCasesStmts, false, stmtIndex, 0, allCasesStmts.size())
                        || anyContains(removedCaseStmts, allCasesStmts, stmtIndex)) {
                    break;
                }
                r.insertAfter(b.copy(caseStmts.get(caseStmts.size() - stmtIndex)), node);
                removeStmts(allCasesStmts, false, stmtIndex, removedCaseStmts);
                result = DO_NOT_VISIT_SUBTREE;
            }

            // Remove the nodes common to all cases
            final boolean[] areCasesEmpty = new boolean[allCasesStmts.size()];
            for (int i = 0; i < allCasesStmts.size(); i++) {
                areCasesEmpty[i] = false;
            }
            removeStmtsFromCases(allCasesStmts, removedCaseStmts, areCasesEmpty);

            if (allEmpty(areCasesEmpty)) {
                r.removeButKeepComment(node);
                return DO_NOT_VISIT_SUBTREE;
            }

            // Remove empty cases
            if (areCasesEmpty[0]) {
                if (areCasesEmpty.length == 2
                        && !areCasesEmpty[1]) {
                    // Then clause is empty and there is only one else clause
                    // => revert if statement
                    r.replace(node,
                              b.if0(b.not(b.parenthesizeIfNeeded(b.move(node.getExpression()))),
                                    b.move(node.getElseStatement())));
                } else {
                    r.replace(node.getThenStatement(), b.block());
                }
                result = DO_NOT_VISIT_SUBTREE;
            }
            for (int i = 1; i < areCasesEmpty.length; i++) {
                if (areCasesEmpty[i]) {
                    final Statement firstStmt = allCasesStmts.get(i).get(0);
                    r.remove(findNodeToRemove(firstStmt, firstStmt.getParent()));
                    result = DO_NOT_VISIT_SUBTREE;
                }
            }
            return result;
        }
        return VISIT_SUBTREE;
    }

    private ASTNode findNodeToRemove(ASTNode node, ASTNode parent) {
        if (parent instanceof IfStatement) {
            return node;
        }
        if (parent instanceof Block) {
            final Block b = (Block) parent;
            return findNodeToRemove(b, b.getParent());
        }
        throw new NotImplementedException(parent, "for parent of type " + parent.getClass());
    }

    private boolean allEmpty(boolean[] areCasesEmpty) {
        for (boolean isCaseEmpty : areCasesEmpty) {
            if (!isCaseEmpty) {
                return false;
            }
        }
        return true;
    }

    private void removeStmtsFromCases(List<List<Statement>> allCasesStmts, List<List<ASTNode>> removedCaseStmts,
            boolean[] areCasesEmpty) {
        for (int i = 0; i < allCasesStmts.size(); i++) {
            final List<ASTNode> removedStmts = removedCaseStmts.get(i);
            if (removedStmts.containsAll(allCasesStmts.get(i))) {
                areCasesEmpty[i] = true;
            } else {
                this.ctx.getRefactorings().remove(removedStmts);
            }
        }
    }

    private boolean anyContains(List<List<ASTNode>> removedCaseStmts, List<List<Statement>> allCasesStmts,
            int stmtIndex) {
        for (int i = 0; i < allCasesStmts.size(); i++) {
            final List<Statement> caseStmts = allCasesStmts.get(i);
            if (removedCaseStmts.get(i).contains(caseStmts.get(caseStmts.size() - stmtIndex))) {
                return true;
            }
        }
        return false;
    }

    private void removeStmts(List<List<Statement>> allCasesStmts, boolean forwardCase, int stmtIndex,
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

    private boolean match(ASTSemanticMatcher matcher, List<List<Statement>> allCasesStmts, boolean matchForward,
            int stmtIndex, int startIndex, int endIndex) {
        if (startIndex == endIndex || startIndex == endIndex - 1) {
            return true;
        }
        final int comparisonIndex;
        if (endIndex - startIndex > 1) {
            final int pivotIndex = (endIndex + startIndex + 1) / 2;
            if (!match(matcher, allCasesStmts, matchForward, stmtIndex, startIndex, pivotIndex)
                    || !match(matcher, allCasesStmts, matchForward, stmtIndex, pivotIndex, endIndex)) {
                return false;
            }
            comparisonIndex = pivotIndex;
        } else {
            comparisonIndex = endIndex - 1;
        }

        final List<Statement> caseStmts1 = allCasesStmts.get(startIndex);
        final List<Statement> caseStmts2 = allCasesStmts.get(comparisonIndex);
        if (matchForward) {
            return ASTHelper.match(matcher, caseStmts1.get(stmtIndex), caseStmts2.get(stmtIndex));
        } else {
            return ASTHelper.match(matcher, caseStmts1.get(caseStmts1.size() - stmtIndex),
                    caseStmts2.get(caseStmts2.size() - stmtIndex));
        }
    }

    private int minSize(List<List<Statement>> allCasesStmts) {
        if (allCasesStmts.isEmpty()) {
            throw new IllegalStateException(null, "allCasesStmts List must not be empty");
        }
        int min = Integer.MAX_VALUE;
        for (List<Statement> stmts : allCasesStmts) {
            min = Math.min(min, stmts.size());
        }
        if (min == Integer.MAX_VALUE) {
            throw new IllegalStateException(null, "The minimum size should never have been equal to Integer.MAX_VALUE");
        }
        return min;
    }

    /**
     * Collects all cases (if/else, if/else if/else, etc.) and returns whether all are covered.
     *
     * @param allCases the output collection for all the cases
     * @param node the {@link IfStatement} to examine
     * @return true if all cases (if/else, if/else if/else, etc.) are covered,
     *         false otherwise
     */
    private boolean collectAllCases(List<List<Statement>> allCases, IfStatement node) {
        final List<Statement> thenStmts = asList(node.getThenStatement());
        final List<Statement> elseStmts = asList(node.getElseStatement());
        if (thenStmts.isEmpty() || elseStmts.isEmpty()) {
            // if the then or else clause is empty, then there is no common code whatsoever.
            // let other refactorings take care of removing empty blocks.
            return false;
        }

        allCases.add(thenStmts);
        if (elseStmts.size() == 1) {
            final IfStatement is = as(elseStmts.get(0), IfStatement.class);
            if (is != null) {
                return collectAllCases(allCases, is);
            }
        }
        allCases.add(elseStmts);
        return true;
    }
}
