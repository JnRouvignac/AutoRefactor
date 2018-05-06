/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.asList;
import static org.autorefactor.refactoring.ASTHelper.getLocalVariableIdentifiers;
import static org.autorefactor.refactoring.ASTHelper.getNextSiblings;
import static org.autorefactor.refactoring.ASTHelper.fallsThrough;
import static org.autorefactor.refactoring.ASTHelper.isPassive;
import static org.autorefactor.refactoring.ASTHelper.match;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.BlockSubVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;

/** See {@link #getDescription()} method. */
public class InlineCodeRatherThanPeremptoryConditionRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Inline code rather than peremptory condition";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Replace always true or always false condition by inline code.";
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
    public boolean visit(Block node) {
        final IfAndFollowingCodeVisitor ifAndFollowingCodeVisitor = new IfAndFollowingCodeVisitor(node);
        node.accept(ifAndFollowingCodeVisitor);
        return ifAndFollowingCodeVisitor.getResult();
    }

    private final class IfAndFollowingCodeVisitor extends BlockSubVisitor {

        public IfAndFollowingCodeVisitor(final Block startNode) {
            super(null, startNode);
        }

        @Override
        public boolean visit(TryStatement node) {
            if (node.resources().isEmpty()) {
                final List<Statement> tryStmts = asList(node.getBody());
                if (tryStmts.isEmpty()) {
                    final List<Statement> finallyStmts = asList(node.getFinally());
                    if (!finallyStmts.isEmpty()) {
                        return maybeInlineBlock(node, node.getFinally());
                    } else {
                        InlineCodeRatherThanPeremptoryConditionRefactoring.this.ctx.getRefactorings().remove(node);
                        return DO_NOT_VISIT_SUBTREE;
                    }
                }
            }

            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(IfStatement node) {
            final ASTBuilder b = InlineCodeRatherThanPeremptoryConditionRefactoring.this.ctx.getASTBuilder();
            final Refactorings r = InlineCodeRatherThanPeremptoryConditionRefactoring.this.ctx.getRefactorings();

            final Statement thenStmt = node.getThenStatement();
            final Statement elseStmt = node.getElseStatement();
            final Expression condition = node.getExpression();

            final Object constantCondition = peremptoryValue(condition);
            if (Boolean.TRUE.equals(constantCondition)) {
                return maybeInlineBlock(node, thenStmt);
            } else if (Boolean.FALSE.equals(constantCondition)) {
                if (elseStmt != null) {
                    return maybeInlineBlock(node, elseStmt);
                } else {
                    r.remove(node);
                    setResult(DO_NOT_VISIT_SUBTREE);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
            return VISIT_SUBTREE;
        }

        private boolean maybeInlineBlock(final Statement node, final Statement unconditionnalStatement) {
            if (fallsThrough(unconditionnalStatement)) {
                replaceBlockByPlainCode(node, unconditionnalStatement);
                removeForwardCode(node, unconditionnalStatement);
                setResult(DO_NOT_VISIT_SUBTREE);
                return DO_NOT_VISIT_SUBTREE;
            } else {
                final Set<String> ifVariableNames = getLocalVariableIdentifiers(unconditionnalStatement, false);

                final Set<String> followingVariableNames = new HashSet<String>();
                for (final Statement statement : getNextSiblings(node)) {
                    followingVariableNames.addAll(getLocalVariableIdentifiers(statement, true));
                }

                if (!ifVariableNames.removeAll(followingVariableNames)) {
                    replaceBlockByPlainCode(node, unconditionnalStatement);
                    setResult(DO_NOT_VISIT_SUBTREE);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
            return VISIT_SUBTREE;
        }
    }

    private Object peremptoryValue(final Expression condition) {
        final Object constantCondition = condition.resolveConstantExpressionValue();
        if (constantCondition != null) {
            return constantCondition;
        } else if (condition instanceof InfixExpression) {
            InfixExpression ie = (InfixExpression) condition;
            if ((EQUALS.equals(ie.getOperator()) || NOT_EQUALS.equals(ie.getOperator()))
                    && isPassive(ie.getLeftOperand())
                    && match(new ASTSemanticMatcher(), ie.getLeftOperand(), ie.getRightOperand())) {
                return EQUALS.equals(ie.getOperator());
            }
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    private void replaceBlockByPlainCode(final Statement sourceNode, final Statement unconditionnalStatement) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();

        if (unconditionnalStatement instanceof Block && sourceNode.getParent() instanceof Block) {
            r.replace(sourceNode, b.copyRange(((Block) unconditionnalStatement).statements()));
        } else {
            r.replace(sourceNode, b.copy(unconditionnalStatement));
        }
    }

    private void removeForwardCode(final Statement astNode, final Statement unconditionnalStatement) {
        if (astNode.getParent() instanceof Block) {
            this.ctx.getRefactorings().remove(getNextSiblings(astNode));
            removeForwardCode((Block) astNode.getParent(), unconditionnalStatement);
        } else if (astNode.getParent() instanceof TryStatement) {
            removeForwardCode((TryStatement) astNode.getParent(), unconditionnalStatement);
        }
    }
}
