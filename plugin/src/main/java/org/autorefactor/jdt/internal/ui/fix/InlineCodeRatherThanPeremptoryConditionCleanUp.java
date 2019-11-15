/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2018 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class InlineCodeRatherThanPeremptoryConditionCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_InlineCodeRatherThanPeremptoryConditionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_InlineCodeRatherThanPeremptoryConditionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_InlineCodeRatherThanPeremptoryConditionCleanUp_reason;
    }

    @Override
    public boolean visit(Block node) {
        final IfAndFollowingCodeVisitor ifAndFollowingCodeVisitor= new IfAndFollowingCodeVisitor(ctx, node);
        node.accept(ifAndFollowingCodeVisitor);
        return ifAndFollowingCodeVisitor.getResult();
    }

    private final class IfAndFollowingCodeVisitor extends BlockSubVisitor {
        public IfAndFollowingCodeVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(TryStatement node) {
            if (node.resources().isEmpty()) {
                final List<Statement> tryStatements= ASTNodes.asList(node.getBody());

                if (tryStatements.isEmpty()) {
                    final List<Statement> finallyStatements= ASTNodes.asList(node.getFinally());

                    if (!finallyStatements.isEmpty()) {
                        return maybeInlineBlock(node, node.getFinally());
                    }

                    final Refactorings r= ctx.getRefactorings();

                    if (ASTNodes.canHaveSiblings(node)) {
                        r.remove(node);
                    } else {
                        r.replace(node, ctx.getASTBuilder().block());
                    }

                    setResult(false);
                    return false;
                }
            }

            return true;
        }

        @Override
        public boolean visit(IfStatement node) {
            final Refactorings r= ctx.getRefactorings();

            final Statement thenStatement= node.getThenStatement();
            final Statement elseStatement= node.getElseStatement();
            final Expression condition= node.getExpression();

            final Object constantCondition= peremptoryValue(condition);

            if (Boolean.TRUE.equals(constantCondition)) {
                return maybeInlineBlock(node, thenStatement);
            }

            if (Boolean.FALSE.equals(constantCondition)) {
                if (elseStatement != null) {
                    return maybeInlineBlock(node, elseStatement);
                }

                if (ASTNodes.canHaveSiblings(node)) {
                    r.remove(node);
                } else {
                    r.replace(node, ctx.getASTBuilder().block());
                }

                setResult(false);
                return false;
            }

            return true;
        }

        private boolean maybeInlineBlock(final Statement node, final Statement unconditionnalStatement) {
            if (ASTNodes.fallsThrough(unconditionnalStatement)) {
                replaceBlockByPlainCode(node, unconditionnalStatement);
                removeForwardCode(node, unconditionnalStatement);
                setResult(false);
                return false;
            }

            final Set<String> ifVariableNames= ASTNodes.getLocalVariableIdentifiers(unconditionnalStatement, false);
            final Set<String> followingVariableNames= new HashSet<>();

            for (Statement statement : ASTNodes.getNextSiblings(node)) {
                followingVariableNames.addAll(ASTNodes.getLocalVariableIdentifiers(statement, true));
            }

            if (!ifVariableNames.removeAll(followingVariableNames)) {
                replaceBlockByPlainCode(node, unconditionnalStatement);
                setResult(false);
                return false;
            }

            return true;
        }
    }

    private Object peremptoryValue(final Expression condition) {
        final Object constantCondition= condition.resolveConstantExpressionValue();

        if (constantCondition != null) {
            return constantCondition;
        }

        InfixExpression infixExpression= ASTNodes.as(condition, InfixExpression.class);

        if (infixExpression != null
                && !infixExpression.hasExtendedOperands()
                && ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)
                && ASTNodes.isPassive(infixExpression.getLeftOperand())) {
            if (ASTNodes.match(infixExpression.getLeftOperand(), infixExpression.getRightOperand())) {
                return ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.EQUALS);
            }

            if (ASTSemanticMatcher.INSTANCE.matchOpposite(infixExpression.getLeftOperand(), infixExpression.getRightOperand())) {
                return ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.NOT_EQUALS);
            }
        }

        return null;
    }

    private void replaceBlockByPlainCode(final Statement sourceNode, final Statement unconditionnalStatement) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();
        final Refactorings r= this.ctx.getRefactorings();

        if (unconditionnalStatement instanceof Block && sourceNode.getParent() instanceof Block) {
            r.replace(sourceNode, b.copyRange(ASTNodes.statements((Block) unconditionnalStatement)));
        } else {
            r.replace(sourceNode, b.createMoveTarget(unconditionnalStatement));
        }
    }

    private void removeForwardCode(final Statement astNode, final Statement unconditionnalStatement) {
        if (astNode.getParent() instanceof Block) {
            this.ctx.getRefactorings().remove(ASTNodes.getNextSiblings(astNode));
            removeForwardCode((Block) astNode.getParent(), unconditionnalStatement);
        } else if (astNode.getParent() instanceof TryStatement) {
            removeForwardCode((TryStatement) astNode.getParent(), unconditionnalStatement);
        }
    }
}
