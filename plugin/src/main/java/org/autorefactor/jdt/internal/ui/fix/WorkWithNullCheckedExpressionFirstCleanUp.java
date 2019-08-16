/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class WorkWithNullCheckedExpressionFirstCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_WorkWithNullCheckedExpressionFirstCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_WorkWithNullCheckedExpressionFirstCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_WorkWithNullCheckedExpressionFirstCleanUp_reason;
    }

    @Override
    public boolean visit(Block node) {
        final IfAndReturnVisitor ifAndReturnVisitor= new IfAndReturnVisitor(ctx, node);
        node.accept(ifAndReturnVisitor);
        return ifAndReturnVisitor.getResult();
    }

    private static final class IfAndReturnVisitor extends BlockSubVisitor {
        public IfAndReturnVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(IfStatement node) {
            final Statement thenStatement= getThenStatement(node);
            final Statement elseStatement= getElseStatement(node, thenStatement);
            if (isNullCheck(node.getExpression()) && thenStatement != null && elseStatement != null
                    && thenStatement.getNodeType() == elseStatement.getNodeType() && simpleStatement(thenStatement)) {
                revertIfStatement(node, thenStatement, elseStatement);
                setResult(false);
                return false;
            }
            return true;
        }

        private Statement getThenStatement(IfStatement node) {
            final List<Statement> thenStatements= ASTNodes.asList(node.getThenStatement());
            if (thenStatements.size() == 1) {
                return thenStatements.get(0);
            }
            return null;
        }

        private Statement getElseStatement(IfStatement node, Statement thenStatement) {
            final List<Statement> elseStatements= ASTNodes.asList(node.getElseStatement());
            if (elseStatements.size() == 1) {
                return elseStatements.get(0);
            }
            if (elseStatements.isEmpty() && ASTNodes.is(thenStatement, ReturnStatement.class)) {
                return ASTNodes.getNextSibling(node);
            }
            return null;
        }

        private boolean isNullCheck(Expression ifExpression) {
            final InfixExpression condition= ASTNodes.as(ifExpression, InfixExpression.class);
            return ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS) && !condition.hasExtendedOperands()
                    && (ASTNodes.isNullLiteral(condition.getLeftOperand()) || ASTNodes.isNullLiteral(condition.getRightOperand()));
        }

        private boolean simpleStatement(Statement statement) {
            switch (statement.getNodeType()) {
            case ASTNode.IF_STATEMENT:
            case ASTNode.DO_STATEMENT:
            case ASTNode.WHILE_STATEMENT:
            case ASTNode.FOR_STATEMENT:
            case ASTNode.ENHANCED_FOR_STATEMENT:
            case ASTNode.TRY_STATEMENT:
                return false;

            default:
                return true;
            }
        }

        /** Revert condition + swap then and else statements. */
        private void revertIfStatement(IfStatement node, Statement thenStatement, Statement elseStatement) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            final Refactorings r= ctx.getRefactorings();
            r.set(node.getExpression(), InfixExpression.OPERATOR_PROPERTY, InfixExpression.Operator.NOT_EQUALS);
            r.replace(thenStatement, b.move(elseStatement));
            r.replace(elseStatement, b.move(thenStatement));
        }
    }
}
