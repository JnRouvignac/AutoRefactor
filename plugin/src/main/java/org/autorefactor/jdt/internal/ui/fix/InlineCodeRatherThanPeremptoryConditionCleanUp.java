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

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
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
    public boolean visit(final Block node) {
        IfAndFollowingCodeVisitor ifAndFollowingCodeVisitor= new IfAndFollowingCodeVisitor(cuRewrite, node);
        node.accept(ifAndFollowingCodeVisitor);
        return ifAndFollowingCodeVisitor.getResult();
    }

    private final class IfAndFollowingCodeVisitor extends BlockSubVisitor {
        public IfAndFollowingCodeVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
            super(cuRewrite, startNode);
        }

        @Override
        public boolean visit(final TryStatement node) {
            if (getResult() && node.resources().isEmpty()) {
                List<Statement> tryStatements= ASTNodes.asList(node.getBody());

                if (tryStatements.isEmpty()) {
                    List<Statement> finallyStatements= ASTNodes.asList(node.getFinally());

                    if (!finallyStatements.isEmpty()) {
                        return maybeInlineBlock(node, node.getFinally());
                    }

                    ASTRewrite rewrite= cuRewrite.getASTRewrite();

                    if (ASTNodes.canHaveSiblings(node)) {
                        rewrite.remove(node, null);
                    } else {
                        rewrite.replace(node, cuRewrite.getASTBuilder().block(), null);
                    }

                    setResult(false);
                    return false;
                }
            }

            return true;
        }

        @Override
        public boolean visit(final IfStatement node) {
            if (getResult()) {
                ASTRewrite rewrite= cuRewrite.getASTRewrite();

                Statement thenStatement= node.getThenStatement();
                Statement elseStatement= node.getElseStatement();
                Expression condition= node.getExpression();

                Object constantCondition= peremptoryValue(condition);

                if (Boolean.TRUE.equals(constantCondition)) {
                    return maybeInlineBlock(node, thenStatement);
                }

                if (Boolean.FALSE.equals(constantCondition)) {
                    if (elseStatement != null) {
                        return maybeInlineBlock(node, elseStatement);
                    }

                    if (ASTNodes.canHaveSiblings(node)) {
                        rewrite.remove(node, null);
                    } else {
                        rewrite.replace(node, cuRewrite.getASTBuilder().block(), null);
                    }

                    setResult(false);
                    return false;
                }
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

            Set<String> ifVariableNames= ASTNodes.getLocalVariableIdentifiers(unconditionnalStatement, false);
            Set<String> followingVariableNames= new HashSet<>();

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
        Object constantCondition= condition.resolveConstantExpressionValue();

        if (constantCondition != null) {
            return constantCondition;
        }

        InfixExpression infixExpression= ASTNodes.as(condition, InfixExpression.class);

        if (infixExpression != null
                && !infixExpression.hasExtendedOperands()
                && ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)
                && ASTNodes.isPassiveWithoutFallingThrough(infixExpression.getLeftOperand())) {
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
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        if (unconditionnalStatement instanceof Block && ASTNodes.canHaveSiblings(sourceNode)) {
            rewrite.replace(sourceNode, ast.copyRange(ASTNodes.statements((Block) unconditionnalStatement)), null);
        } else {
            rewrite.replace(sourceNode, rewrite.createMoveTarget(unconditionnalStatement), null);
        }
    }

    private void removeForwardCode(final Statement astNode, final Statement unconditionnalStatement) {
        if (ASTNodes.canHaveSiblings(astNode)) {
            cuRewrite.getASTRewrite().remove(ASTNodes.getNextSiblings(astNode), null);
            removeForwardCode((Block) astNode.getParent(), unconditionnalStatement);
        } else if (astNode.getParent() instanceof TryStatement) {
            removeForwardCode((TryStatement) astNode.getParent(), unconditionnalStatement);
        }
    }
}
