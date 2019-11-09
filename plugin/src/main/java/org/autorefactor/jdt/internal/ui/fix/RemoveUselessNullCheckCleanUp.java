/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.ASTSemanticMatcher;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;

/** See {@link #getDescription()} method. */
public class RemoveUselessNullCheckCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUselessNullCheckCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUselessNullCheckCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUselessNullCheckCleanUp_reason;
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
            final InfixExpression condition= ASTNodes.as(node.getExpression(), InfixExpression.class);
            final Statement thenStatement= getThenStatement(node);
            final Statement elseStatement= getElseStatement(node, thenStatement);

            if ((condition != null) && !condition.hasExtendedOperands() && (thenStatement != null) && (elseStatement != null)) {
                final Assignment thenAs= ASTNodes.asExpression(thenStatement, Assignment.class);
                final Assignment elseAs= ASTNodes.asExpression(elseStatement, Assignment.class);

                if (ASTNodes.hasOperator(thenAs, Assignment.Operator.ASSIGN) && ASTNodes.hasOperator(elseAs, Assignment.Operator.ASSIGN)
                        && ASTNodes.match(ASTSemanticMatcher.INSTANCE, thenAs.getLeftHandSide(), elseAs.getLeftHandSide())) {
                    if (ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS) && ASTNodes.is(thenAs.getRightHandSide(), NullLiteral.class)) {
                        return maybeReplaceWithStraightAssign(node, condition, elseAs);
                    }

                    if (ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS) && ASTNodes.is(elseAs.getRightHandSide(), NullLiteral.class)) {
                        return maybeReplaceWithStraightAssign(node, condition, thenAs);
                    }
                } else {
                    final ReturnStatement thenRS= ASTNodes.as(thenStatement, ReturnStatement.class);
                    final ReturnStatement elseRS= ASTNodes.as(elseStatement, ReturnStatement.class);

                    if ((thenRS != null) && (elseRS != null)) {
                        if (ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS)) {
                            return maybeReplaceWithStraightReturn(node, condition, elseRS, thenRS, elseRS);
                        }

                        if (ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS)) {
                            return maybeReplaceWithStraightReturn(node, condition, thenRS, elseRS, elseRS);
                        }
                    }
                }
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

            if (ASTNodes.is(thenStatement, ReturnStatement.class)) {
                return ASTNodes.getNextSibling(node);
            }

            return null;
        }

        private boolean maybeReplaceWithStraightAssign(IfStatement node, InfixExpression condition, Assignment as) {
            if (ASTNodes.is(condition.getRightOperand(), NullLiteral.class)
                    && ASTNodes.match(ASTSemanticMatcher.INSTANCE, condition.getLeftOperand(), as.getRightHandSide())) {
                replaceWithStraightAssign(node, as.getLeftHandSide(), condition.getLeftOperand());
                setResult(false);
                return false;
            }

            if (ASTNodes.is(condition.getLeftOperand(), NullLiteral.class)
                    && ASTNodes.match(ASTSemanticMatcher.INSTANCE, condition.getRightOperand(), as.getRightHandSide())) {
                replaceWithStraightAssign(node, as.getLeftHandSide(), condition.getRightOperand());
                setResult(false);
                return false;
            }

            return true;
        }

        private void replaceWithStraightAssign(IfStatement node, Expression leftHandSide, Expression rightHandSide) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            ctx.getRefactorings().replace(node,
                    b.toStatement(b.assign(b.copy(leftHandSide), Assignment.Operator.ASSIGN, b.copy(rightHandSide))));
        }

        private boolean maybeReplaceWithStraightReturn(IfStatement node, InfixExpression condition, ReturnStatement rs,
                ReturnStatement otherRs, Statement toRemove) {
            if (ASTNodes.is(otherRs.getExpression(), NullLiteral.class)) {
                if (ASTNodes.is(condition.getRightOperand(), NullLiteral.class)
                        && ASTNodes.match(ASTSemanticMatcher.INSTANCE, condition.getLeftOperand(), rs.getExpression())) {
                    replaceWithStraightReturn(node, condition.getLeftOperand(), toRemove);
                    setResult(false);
                    return false;
                }

                if (ASTNodes.is(condition.getLeftOperand(), NullLiteral.class)
                        && ASTNodes.match(ASTSemanticMatcher.INSTANCE, condition.getRightOperand(), rs.getExpression())) {
                    replaceWithStraightReturn(node, condition.getRightOperand(), toRemove);
                    setResult(false);
                    return false;
                }
            }

            return true;
        }

        private void replaceWithStraightReturn(IfStatement node, Expression returnedExpression, Statement toRemove) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            final Refactorings r= ctx.getRefactorings();

            r.remove(toRemove);
            r.replace(node, b.return0(b.copy(returnedExpression)));
        }
    }
}
