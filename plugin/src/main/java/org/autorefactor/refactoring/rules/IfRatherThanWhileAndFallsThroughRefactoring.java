/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getNextSiblings;
import static org.eclipse.jdt.core.dom.ASTNode.BREAK_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.THROW_STATEMENT;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.InterruptableVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class IfRatherThanWhileAndFallsThroughRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "If rather than while and falls through";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace a while loop that always terminates during the first iteration by an if.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It disambiguates the code to improve the readibility.";
    }

    @Override
    public boolean visit(WhileStatement node) {
        if (isEndingWithExit(node.getBody())) {
            final BreakVisitor breakVisitor = new BreakVisitor(node);
            breakVisitor.visitNode(node);

            if (breakVisitor.canBeRefactored()) {
                final ASTBuilder b = ctx.getASTBuilder();
                for (final BreakStatement breakStmt : breakVisitor.getBreaks()) {
                    ctx.getRefactorings().remove(breakStmt);
                }
                ctx.getRefactorings().replace(node, b.if0(b.copy(node.getExpression()), b.copy(node.getBody())));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    /**
     * Return true if the statement falls through.
     *
     * @param stmt the statement
     * @return true if the statement falls through.
     */
    private boolean isEndingWithExit(final Statement stmt) {
        final List<Statement> stmts = asList(stmt);
        if (stmts.isEmpty()) {
            return false;
        }

        final Statement lastStmt = stmts.get(stmts.size() - 1);
        switch (lastStmt.getNodeType()) {
        case RETURN_STATEMENT:
        case THROW_STATEMENT:
            return true;

        case BREAK_STATEMENT:
            final BreakStatement breakStmt = (BreakStatement) lastStmt;
            return breakStmt.getLabel() == null;

        case IF_STATEMENT:
            final IfStatement ifStmt = (IfStatement) lastStmt;
            final Statement thenStmt = ifStmt.getThenStatement();
            final Statement elseStmt = ifStmt.getElseStatement();
            return isEndingWithExit(thenStmt)
                    && isEndingWithExit(elseStmt);

        default:
            return false;
        }
    }

    private class BreakVisitor extends InterruptableVisitor {

        private final WhileStatement root;
        private final List<BreakStatement> breaks = new ArrayList<BreakStatement>();
        private boolean canBeRefactored = true;

        public BreakVisitor(final WhileStatement root) {
            this.root = root;
        }

        public List<BreakStatement> getBreaks() {
            return breaks;
        }

        public boolean canBeRefactored() {
            return canBeRefactored;
        }

        @Override
        public boolean visit(BreakStatement aBreak) {
            Statement parent = getAncestorOrNull(aBreak, Statement.class);
            while (parent != root && (getNextSiblings(parent) == null || getNextSiblings(parent).isEmpty())) {
                parent = getAncestorOrNull(parent, Statement.class);
            }

            if (parent != root) {
                canBeRefactored = false;
                return interruptVisit();
            }

            breaks.add(aBreak);

            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(WhileStatement node) {
            if (!root.equals(node)) {
                return DO_NOT_VISIT_SUBTREE;
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(DoStatement node) {
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(ForStatement node) {
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(EnhancedForStatement node) {
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(SwitchStatement node) {
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(AnonymousClassDeclaration node) {
            return DO_NOT_VISIT_SUBTREE;
        }
    }
}
