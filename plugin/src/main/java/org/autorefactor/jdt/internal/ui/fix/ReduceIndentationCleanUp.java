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
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CatchClause;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class ReduceIndentationCleanUp extends AbstractCleanUpRule {
    private static final class IndentationVisitor extends ASTVisitor {
        private int indentation;

        public int getIndentation() {
            return indentation;
        }

        @Override
        public boolean visit(final IfStatement node) {
            computeGreatestIndentation(node.getThenStatement());

            if (node.getElseStatement() != null) {
                computeGreatestIndentation(node.getElseStatement());
            }

            return false;
        }

        @Override
        public boolean visit(final WhileStatement node) {
            computeGreatestIndentation(node.getBody());
            return false;
        }

        @Override
        public boolean visit(final DoStatement node) {
            computeGreatestIndentation(node.getBody());
            return false;
        }

        @Override
        public boolean visit(final ForStatement node) {
            computeGreatestIndentation(node.getBody());
            return false;
        }

        @Override
        public boolean visit(final EnhancedForStatement node) {
            computeGreatestIndentation(node.getBody());
            return false;
        }

        @Override
        public boolean visit(final TryStatement node) {
            computeGreatestIndentation(node.getBody());

            for (Object object : node.catchClauses()) {
                CatchClause clause= (CatchClause) object;
                computeGreatestIndentation(clause.getBody());
            }

            if (node.getFinally() != null) {
                computeGreatestIndentation(node.getFinally());
            }

            if (node.getFinally() != null) {
                computeGreatestIndentation(node.getFinally());
            }

            return false;
        }

        @Override
        public boolean visit(final Block node) {
            computeGreatestIndentation(node);
            return false;
        }

        private void computeGreatestIndentation(final Statement statements) {
            for (Statement statement : ASTNodes.asList(statements)) {
                IndentationVisitor visitor= new IndentationVisitor();

                statement.accept(visitor);

                indentation= Math.max(indentation, visitor.getIndentation() + 1);
            }
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ReduceIndentationCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ReduceIndentationCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ReduceIndentationCleanUp_reason;
    }

    @Override
    public boolean visit(final IfStatement node) {
        if (node.getElseStatement() != null && !ASTNodes.isInElse(node)) {
            if (ASTNodes.fallsThrough(node.getThenStatement())) {
                if (ASTNodes.fallsThrough(node.getElseStatement())) {
                    if (ASTNodes.getNextSiblings(node).isEmpty()) {
                        int thenIndentation= getIndentation(node.getThenStatement());
                        int elseIndentation= getIndentation(node.getElseStatement());

                        if (thenIndentation <= elseIndentation || node.getElseStatement() instanceof IfStatement) {
                            moveElseStatement(node);
                        } else {
                            moveThenStatement(node);
                        }

                        return false;
                    }
                } else if (!ASTNodes.hasVariableConflict(node, node.getElseStatement())) {
                    moveElseStatement(node);
                    return false;
                }
            } else if (ASTNodes.fallsThrough(node.getElseStatement()) && !ASTNodes.hasVariableConflict(node, node.getThenStatement()) && !(node.getElseStatement() instanceof IfStatement)) {
                moveThenStatement(node);
                return false;
            }
        }

        return true;
    }

    private int getIndentation(final Statement statementInIf) {
        IndentationVisitor visitor= new IndentationVisitor();
        statementInIf.accept(visitor);
        return visitor.getIndentation() + (statementInIf instanceof Block ? -1 : 0);
    }

    private void moveThenStatement(final IfStatement node) {
        Refactorings r= this.ctx.getRefactorings();
        ASTNodeFactory b= this.ctx.getASTBuilder();

        List<Statement> statementsToMove= ASTNodes.asList(node.getThenStatement());

        if (ASTNodes.canHaveSiblings(node)) {
            for (int i= statementsToMove.size() - 1; i >= 0; i--) {
                r.insertAfter(b.createMoveTarget(statementsToMove.get(i)), node);
            }

            r.replace(node.getExpression(), b.negate(node.getExpression()));
            r.replace(node.getThenStatement(), b.createMoveTarget(node.getElseStatement()));
            r.remove(node.getElseStatement());
        } else {
            List<Statement> copyOfStatements= new ArrayList<>(statementsToMove.size() + 1);

            for (Statement statement : statementsToMove) {
                copyOfStatements.add(b.createMoveTarget(statement));
            }

            r.replace(node.getExpression(), b.negate(node.getExpression()));
            r.replace(node.getThenStatement(), b.createMoveTarget(node.getElseStatement()));
            copyOfStatements.add(0, b.createMoveTarget(node));

            Block block= b.block(copyOfStatements);
            r.replace(node, block);
        }
    }

    private void moveElseStatement(final IfStatement node) {
        Refactorings r= this.ctx.getRefactorings();
        ASTNodeFactory b= this.ctx.getASTBuilder();

        List<Statement> statementsToMove= ASTNodes.asList(node.getElseStatement());

        if (ASTNodes.canHaveSiblings(node)) {
            for (int i= statementsToMove.size() - 1; i >= 0; i--) {
                r.insertAfter(b.createMoveTarget(statementsToMove.get(i)), node);
            }

            r.remove(node.getElseStatement());
        } else {
            List<Statement> copyOfStatements= new ArrayList<>(statementsToMove.size() + 1);

            for (Statement statement : statementsToMove) {
                copyOfStatements.add(b.createMoveTarget(statement));
            }

            r.remove(node.getElseStatement());
            copyOfStatements.add(0, b.createMoveTarget(node));

            Block block= b.block(copyOfStatements);
            r.replace(node, block);
        }
    }
}
