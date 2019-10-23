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

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public class NoAssignmentInIfConditionCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_reason;
    }

    @Override
    public boolean visit(Block node) {
        final NewAndPutAllMethodVisitor newAndPutAllMethodVisitor= new NewAndPutAllMethodVisitor(ctx, node);
        node.accept(newAndPutAllMethodVisitor);
        return newAndPutAllMethodVisitor.getResult();
    }

    private static final class NewAndPutAllMethodVisitor extends BlockSubVisitor {
        public NewAndPutAllMethodVisitor(final RefactoringContext ctx, final Block startNode) {
            super(ctx, startNode);
        }

        @Override
        public boolean visit(IfStatement node) {
            final InfixExpression ie= ASTNodes.as(node.getExpression(), InfixExpression.class);
            return moveAssignmentBeforeIfStatementIfPossible(node, ie);
        }

        private boolean moveAssignmentBeforeIfStatementIfPossible(IfStatement node, InfixExpression ie) {
            if (ie != null) {
                final InfixExpression leftIe= ASTNodes.as(ie.getLeftOperand(), InfixExpression.class);
                final Assignment leftAs= ASTNodes.as(ie.getLeftOperand(), Assignment.class);
                final Assignment rightAs= ASTNodes.as(ie.getRightOperand(), Assignment.class);

                if (leftAs != null) {
                    return moveAssignmentBeforeIfStatement(node, leftAs);
                }
                if (rightAs != null) {
                    return moveAssignmentBeforeIfStatement(node, rightAs);
                }
                if (leftIe != null) {
                    return moveAssignmentBeforeIfStatementIfPossible(node, leftIe);
                }
            }

            return true;
        }

        private boolean moveAssignmentBeforeIfStatement(final IfStatement node, final Assignment assignment) {
            final Refactorings r= ctx.getRefactorings();
            final ASTNodeFactory b= ctx.getASTBuilder();

            final VariableDeclarationStatement vds= ASTNodes.as(ASTNodes.getPreviousSibling(node), VariableDeclarationStatement.class);
            final Expression lhs= ASTNodes.getUnparenthesedExpression(assignment.getLeftHandSide());
            final VariableDeclarationFragment vdf= findVariableDeclarationFragment(vds, lhs);

            if (vdf != null && (vdf.getInitializer() == null || ASTNodes.isPassive(vdf.getInitializer()))) {
                r.set(vdf, VariableDeclarationFragment.INITIALIZER_PROPERTY, assignment.getRightHandSide());
                r.replace(ASTNodes.getParent(assignment, ParenthesizedExpression.class), b.copy(lhs));
                setResult(false);
                return false;
            }

            if (!isAnElseIf(node)) {
                r.replace(ASTNodes.getParent(assignment, ParenthesizedExpression.class), b.copy(lhs));
                Statement newAssignment= b.toStatement(b.move(assignment));

                if (node.getParent() instanceof Block) {
                    r.insertBefore(newAssignment, node);
                } else {
                    Block newBlock= b.block(newAssignment, b.move(node));
                    r.replace(node, newBlock);
                }

                setResult(false);
                return false;
            }

            return true;
        }

        private VariableDeclarationFragment findVariableDeclarationFragment(final VariableDeclarationStatement vds,
                final Expression expression) {
            if (vds != null && expression instanceof SimpleName) {
                for (VariableDeclarationFragment vdf : ASTNodes.fragments(vds)) {
                    if (ASTNodes.isSameVariable(expression, vdf)) {
                        return vdf;
                    }
                }
            }

            return null;
        }

        private boolean isAnElseIf(IfStatement node) {
            final ASTNode parent= node.getParent();
            return parent instanceof IfStatement && node.equals(((IfStatement) parent).getElseStatement());
        }
    }
}
