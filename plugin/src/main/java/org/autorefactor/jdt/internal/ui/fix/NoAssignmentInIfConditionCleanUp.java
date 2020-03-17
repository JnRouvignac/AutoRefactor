/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Make sure we do not visit again modified nodes
 * Copyright (C) 2019 Fabrice Tiercelin - Change the parsing of condition
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
import org.autorefactor.jdt.internal.corext.dom.BlockSubVisitor;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.autorefactor.jdt.internal.corext.refactoring.structure.CompilationUnitRewrite;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
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
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_NoAssignmentInIfConditionCleanUp_reason;
    }

    @Override
    public boolean visit(final Block node) {
        IfWithAssignmentVisitor ifWithAssignmentVisitor= new IfWithAssignmentVisitor(cuRewrite, node);
        node.accept(ifWithAssignmentVisitor);
        return ifWithAssignmentVisitor.getResult();
    }

    private static final class IfWithAssignmentVisitor extends BlockSubVisitor {
        public IfWithAssignmentVisitor(final CompilationUnitRewrite cuRewrite, final Block startNode) {
            super(cuRewrite, startNode);
        }

        @Override
        public boolean visit(final IfStatement node) {
            return !getResult() || moveAssignmentBeforeIfStatementIfPossible(node, node.getExpression(), new ArrayList<Expression>());
        }

        private boolean moveAssignmentBeforeIfStatementIfPossible(final IfStatement node, final Expression expression, final List<Expression> evaluatedExpression) {
            Assignment assignment= ASTNodes.as(expression, Assignment.class);

            if (assignment != null) {
                return moveAssignmentBeforeIfStatement(node, assignment, evaluatedExpression);
            }

            PrefixExpression pe= ASTNodes.as(expression, PrefixExpression.class);

            if (pe != null && ASTNodes.hasOperator(pe,
                    PrefixExpression.Operator.NOT,
                    PrefixExpression.Operator.COMPLEMENT,
                    PrefixExpression.Operator.MINUS,
                    PrefixExpression.Operator.PLUS)) {
                return moveAssignmentBeforeIfStatementIfPossible(node, pe.getOperand(), evaluatedExpression);
            }

            InfixExpression ie= ASTNodes.as(expression, InfixExpression.class);

            if (ie != null) {
                List<Expression> operands= ASTNodes.allOperands(ie);
                boolean isAllOperandsEvaluated= ASTNodes.hasOperator(ie,
                        InfixExpression.Operator.EQUALS,
                        InfixExpression.Operator.NOT_EQUALS,
                        InfixExpression.Operator.PLUS,
                        InfixExpression.Operator.MINUS,
                        InfixExpression.Operator.DIVIDE,
                        InfixExpression.Operator.TIMES,
                        InfixExpression.Operator.XOR,
                        InfixExpression.Operator.GREATER,
                        InfixExpression.Operator.GREATER_EQUALS,
                        InfixExpression.Operator.LEFT_SHIFT,
                        InfixExpression.Operator.LESS,
                        InfixExpression.Operator.LESS_EQUALS,
                        InfixExpression.Operator.REMAINDER,
                        InfixExpression.Operator.RIGHT_SHIFT_SIGNED,
                        InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED,
                        InfixExpression.Operator.AND,
                        InfixExpression.Operator.OR);

                for (Expression operand : operands) {
                    if (!moveAssignmentBeforeIfStatementIfPossible(node, operand, evaluatedExpression)) {
                        return false;
                    }

                    if (!isAllOperandsEvaluated || !ASTNodes.isPassive(operand)) {
                        break;
                    }

                    evaluatedExpression.add(operand);
                }
            }

            ConditionalExpression ce= ASTNodes.as(expression, ConditionalExpression.class);

            return ce == null || moveAssignmentBeforeIfStatementIfPossible(node, ce.getExpression(), evaluatedExpression);
        }

        private boolean moveAssignmentBeforeIfStatement(final IfStatement node, final Assignment assignment, final List<Expression> evaluatedExpression) {
            Expression lhs= ASTNodes.getUnparenthesedExpression(assignment.getLeftHandSide());

            if (!evaluatedExpression.isEmpty()) {
                Name mame= ASTNodes.as(lhs, Name.class);
                FieldAccess fieldAccess= ASTNodes.as(lhs, FieldAccess.class);
                IVariableBinding variableBinding;

                if (fieldAccess != null) {
                    variableBinding= fieldAccess.resolveFieldBinding();
                } else if (mame != null) {
                    IBinding binding= mame.resolveBinding();

                    if (!(binding instanceof IVariableBinding)) {
                        return true;
                    }

                    variableBinding= (IVariableBinding) binding;
                } else {
                    return true;
                }

                for (Expression expression : evaluatedExpression) {
                    VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(variableBinding,
                            expression, true).find();

                    if (!variableUseVisitor.getReads().isEmpty()) {
                        return true;
                    }
                }
            }

            VariableDeclarationStatement vds= ASTNodes.as(ASTNodes.getPreviousSibling(node), VariableDeclarationStatement.class);
            VariableDeclarationFragment vdf= findVariableDeclarationFragment(vds, lhs);

            Refactorings r= cuRewrite.getRefactorings();
            ASTNodeFactory b= cuRewrite.getASTBuilder();

            if (vdf != null && (vdf.getInitializer() == null || ASTNodes.isPassive(vdf.getInitializer()))) {
                r.set(vdf, VariableDeclarationFragment.INITIALIZER_PROPERTY, assignment.getRightHandSide());
                r.replace(ASTNodes.getParent(assignment, ParenthesizedExpression.class), b.createCopyTarget(lhs));
                setResult(false);
                return false;
            }

            if (!ASTNodes.isInElse(node)) {
                r.replace(ASTNodes.getParent(assignment, ParenthesizedExpression.class), b.createCopyTarget(lhs));
                Statement newAssignment= b.toStatement(b.createMoveTarget(assignment));

                if (ASTNodes.canHaveSiblings(node)) {
                    r.insertBefore(newAssignment, node);
                } else {
                    Block newBlock= b.block(newAssignment, b.createMoveTarget(node));
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
    }
}
