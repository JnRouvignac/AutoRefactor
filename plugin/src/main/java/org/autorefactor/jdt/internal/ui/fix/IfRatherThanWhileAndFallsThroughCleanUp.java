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
package org.autorefactor.jdt.internal.ui.fix;

import static org.eclipse.jdt.core.dom.ASTNode.BREAK_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.THROW_STATEMENT;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
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
public class IfRatherThanWhileAndFallsThroughCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanWhileAndFallsThroughCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanWhileAndFallsThroughCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfRatherThanWhileAndFallsThroughCleanUp_reason;
    }

    @Override
    public boolean visit(WhileStatement node) {
        if (isEndingWithExit(node.getBody())) {
            final BreakVisitor breakVisitor= new BreakVisitor(node);
            breakVisitor.visitNode(node);

            if (breakVisitor.canBeRefactored()) {
                final ASTNodeFactory b= ctx.getASTBuilder();
                for (BreakStatement breakStatement : breakVisitor.getBreaks()) {
                    ctx.getRefactorings().remove(breakStatement);
                }
                ctx.getRefactorings().replace(node, b.if0(b.copy(node.getExpression()), b.copy(node.getBody())));
                return false;
            }
        }
        return true;
    }

    /**
     * Return true if the statement falls through.
     *
     * @param statement the statement
     * @return true if the statement falls through.
     */
    private boolean isEndingWithExit(final Statement statement) {
        final List<Statement> statements= ASTNodes.asList(statement);
        if (statements.isEmpty()) {
            return false;
        }

        final Statement lastStatement= statements.get(statements.size() - 1);
        switch (lastStatement.getNodeType()) {
        case RETURN_STATEMENT:
        case THROW_STATEMENT:
            return true;

        case BREAK_STATEMENT:
            final BreakStatement breakStatement= (BreakStatement) lastStatement;
            return breakStatement.getLabel() == null;

        case IF_STATEMENT:
            final IfStatement ifStatement= (IfStatement) lastStatement;
            final Statement thenStatement= ifStatement.getThenStatement();
            final Statement elseStatement= ifStatement.getElseStatement();
            return isEndingWithExit(thenStatement) && isEndingWithExit(elseStatement);

        default:
            return false;
        }
    }

    private class BreakVisitor extends InterruptibleVisitor {
        private final WhileStatement root;
        private final List<BreakStatement> breaks= new ArrayList<>();
        private boolean canBeRefactored= true;

        public BreakVisitor(final WhileStatement root) {
            this.root= root;
        }

        public List<BreakStatement> getBreaks() {
            return breaks;
        }

        public boolean canBeRefactored() {
            return canBeRefactored;
        }

        @Override
        public boolean visit(BreakStatement aBreak) {
            Statement parent= ASTNodes.getAncestorOrNull(aBreak, Statement.class);
            while (parent != root && (ASTNodes.getNextSiblings(parent) == null || ASTNodes.getNextSiblings(parent).isEmpty())) {
                parent= ASTNodes.getAncestorOrNull(parent, Statement.class);
            }

            if (parent != root) {
                canBeRefactored= false;
                return interruptVisit();
            }

            breaks.add(aBreak);

            return true;
        }

        @Override
        public boolean visit(WhileStatement node) {
            return root.equals(node);
        }

        @Override
        public boolean visit(DoStatement node) {
            return false;
        }

        @Override
        public boolean visit(ForStatement node) {
            return false;
        }

        @Override
        public boolean visit(EnhancedForStatement node) {
            return false;
        }

        @Override
        public boolean visit(SwitchStatement node) {
            return false;
        }

        @Override
        public boolean visit(AnonymousClassDeclaration node) {
            return false;
        }
    }
}
