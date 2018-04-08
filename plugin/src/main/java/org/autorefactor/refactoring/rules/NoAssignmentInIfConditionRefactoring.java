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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.fragments;
import static org.autorefactor.refactoring.ASTHelper.getFirstParentOfType;
import static org.autorefactor.refactoring.ASTHelper.getPreviousSibling;
import static org.autorefactor.refactoring.ASTHelper.isSameVariable;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.eclipse.jdt.core.dom.VariableDeclarationFragment.INITIALIZER_PROPERTY;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.BlockSubVisitor;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class NoAssignmentInIfConditionRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "No assignment in if condition";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Moves assignments inside an if condition before the if node.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility and reduces the bug hazard.";
    }

    @Override
    public boolean visit(Block node) {
        final NewAndPutAllMethodVisitor newAndPutAllMethodVisitor = new NewAndPutAllMethodVisitor(ctx, node);
        node.accept(newAndPutAllMethodVisitor);
        return newAndPutAllMethodVisitor.getResult();
    }

    private static final class NewAndPutAllMethodVisitor extends BlockSubVisitor {

        public NewAndPutAllMethodVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(IfStatement node) {
            if (!(node.getParent() instanceof Block)) {
                return VISIT_SUBTREE;
            }

            final InfixExpression ie = as(node.getExpression(), InfixExpression.class);
            return moveAssignmentBeforeIfStatementIfPossible(node, ie);
        }

        private boolean moveAssignmentBeforeIfStatementIfPossible(IfStatement node, InfixExpression ie) {
            if (ie != null) {
                final InfixExpression leftIe = as(ie.getLeftOperand(), InfixExpression.class);
                final Assignment leftAs = as(ie.getLeftOperand(), Assignment.class);
                final Assignment rightAs = as(ie.getRightOperand(), Assignment.class);
                if (leftAs != null) {
                    return moveAssignmentBeforeIfStatement(node, leftAs);
                } else if (rightAs != null) {
                    return moveAssignmentBeforeIfStatement(node, rightAs);
                } else if (leftIe != null) {
                    return moveAssignmentBeforeIfStatementIfPossible(node, leftIe);
                }
            }
            return VISIT_SUBTREE;
        }

        private boolean moveAssignmentBeforeIfStatement(final IfStatement node, final Assignment a) {
            final Refactorings r = this.getCtx().getRefactorings();
            final ASTBuilder b = this.getCtx().getASTBuilder();
            final VariableDeclarationStatement vds = as(getPreviousSibling(node), VariableDeclarationStatement.class);
            final Expression lhs = removeParentheses(a.getLeftHandSide());
            final VariableDeclarationFragment vdf = findVariableDeclarationFragment(vds, lhs);
            if (vdf != null) {
                r.set(vdf, INITIALIZER_PROPERTY, a.getRightHandSide());
                r.replace(getFirstParentOfType(a, ParenthesizedExpression.class),
                        b.copy(lhs));
                setResult(DO_NOT_VISIT_SUBTREE);
                return DO_NOT_VISIT_SUBTREE;
            } else if (!isAnElseIf(node)) {
                r.insertBefore(b.toStmt(b.move(a)), node);
                r.replace(getFirstParentOfType(a, ParenthesizedExpression.class),
                        b.copy(lhs));
                setResult(DO_NOT_VISIT_SUBTREE);
                return DO_NOT_VISIT_SUBTREE;
            }
            return VISIT_SUBTREE;
        }

        private VariableDeclarationFragment findVariableDeclarationFragment(final VariableDeclarationStatement vds,
                final Expression expr) {
            if (vds != null && expr instanceof SimpleName) {
                for (VariableDeclarationFragment vdf : fragments(vds)) {
                    if (isSameVariable(expr, vdf)) {
                        return vdf;
                    }
                }
            }
            return null;
        }

        private boolean isAnElseIf(IfStatement node) {
            final ASTNode parent = node.getParent();
            return parent instanceof IfStatement
                    && node.equals(((IfStatement) parent).getElseStatement());
        }
    }
}
