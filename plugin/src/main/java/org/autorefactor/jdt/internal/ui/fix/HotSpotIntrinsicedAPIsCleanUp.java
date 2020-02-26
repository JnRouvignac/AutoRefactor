/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class HotSpotIntrinsicedAPIsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_HotSpotIntrinsicedAPIsCleanUp_reason;
    }

    private static class SystemArrayCopyParams {
        private IVariableBinding indexVarBinding;
        private Expression indexStartPos;
        private Expression srcArrayExpression;
        private Expression srcPos;
        private Expression destArrayExpression;
        private Expression destPos;
        private Expression length;

        @Override
        public String toString() {
            return "System.arraycopy(" + srcArrayExpression + ", " + srcPos + ", " + destArrayExpression + ", " + destPos + ", " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
                    + length + ")"; //$NON-NLS-1$
        }
    }

    @Override
    public boolean visit(final ForStatement node) {
        SystemArrayCopyParams params= new SystemArrayCopyParams();
        collectUniqueIndex(node, params);
        IVariableBinding incrementedIdx= getUniqueIncrementedVariable(node);
        List<Statement> statements= ASTNodes.asList(node.getBody());

        if (Utils.equalNotNull(params.indexVarBinding, incrementedIdx) && statements.size() == 1) {
            collectLength(node.getExpression(), incrementedIdx, params);

            Assignment as= ASTNodes.asExpression(statements.get(0), Assignment.class);

            if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN)) {
                ArrayAccess aaLHS= ASTNodes.as(as.getLeftHandSide(), ArrayAccess.class);
                ArrayAccess aaRHS= ASTNodes.as(as.getRightHandSide(), ArrayAccess.class);

                if (aaLHS != null && aaRHS != null) {
                    params.destArrayExpression= aaLHS.getArray();
                    params.srcArrayExpression= aaRHS.getArray();

                    if (ASTNodes.haveSameType(params.srcArrayExpression, params.destArrayExpression)) {
                        params.destPos= calcIndex(aaLHS.getIndex(), params);
                        params.srcPos= calcIndex(aaRHS.getIndex(), params);
                        return maybeReplaceWithSystemArrayCopyCloneAll(node, params);
                    }
                }
            }
        }

        return true;
    }

    private Expression calcIndex(final Expression index, final SystemArrayCopyParams params) {
        ASTNodeFactory b= this.ctx.getASTBuilder();

        if (index instanceof SimpleName) {
            IVariableBinding idxVar= getVariableBinding(index);

            if (Utils.equalNotNull(params.indexVarBinding, idxVar)) {
                return b.createCopyTarget(params.indexStartPos);
            }
        } else if (index instanceof InfixExpression) {
            InfixExpression ie= (InfixExpression) index;

            if (!ie.hasExtendedOperands() && ASTNodes.hasOperator(ie, InfixExpression.Operator.PLUS)) {
                Expression leftOp= ie.getLeftOperand();
                Expression rightOp= ie.getRightOperand();

                if (leftOp instanceof SimpleName) {
                    IVariableBinding idxVar= getVariableBinding(leftOp);

                    if (Utils.equalNotNull(params.indexVarBinding, idxVar)) {
                        return plus(rightOp, params.indexStartPos);
                    }
                }

                if (rightOp instanceof SimpleName) {
                    IVariableBinding idxVar= getVariableBinding(rightOp);

                    if (Utils.equalNotNull(params.indexVarBinding, idxVar)) {
                        return plus(leftOp, params.indexStartPos);
                    }
                }
            }
        }

        return null;
    }

    private Expression plus(final Expression expr1, final Expression expr2) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        Integer expr1Value= intValue(expr1);
        Integer expr2Value= intValue(expr2);

        if (expr1Value != null && expr2Value != null) {
            return b.int0(expr1Value + expr2Value);
        }
        if (Utils.equalNotNull(expr1Value, 0)) {
            return b.createCopyTarget(expr2);
        }
        if (Utils.equalNotNull(expr2Value, 0)) {
            return b.createCopyTarget(expr1);
        }

        return b.infixExpression(b.createCopyTarget(expr1), InfixExpression.Operator.PLUS, b.createCopyTarget(expr2));
    }

    private Expression minus(final Expression expr1, final Expression expr2) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        Integer expr1Value= intValue(expr1);
        Integer expr2Value= intValue(expr2);

        if (expr1Value != null && expr2Value != null) {
            return b.int0(expr1Value - expr2Value);
        }
        if (Utils.equalNotNull(expr1Value, 0)) {
            throw new NotImplementedException(expr2, "Code is not implemented for negating expr2: " + expr2); //$NON-NLS-1$
        }
        if (Utils.equalNotNull(expr2Value, 0)) {
            return b.createCopyTarget(expr1);
        }

        return b.infixExpression(b.createCopyTarget(expr1), InfixExpression.Operator.MINUS, b.createCopyTarget(expr2));
    }

    private Expression minusPlusOne(final Expression expr1, final Expression expr2) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        Integer expr1Value= intValue(expr1);
        Integer expr2Value= intValue(expr2);

        if (expr1Value != null && expr2Value != null) {
            return b.int0(expr1Value - expr2Value + 1);
        }
        if (Utils.equalNotNull(expr1Value, 0)) {
            throw new NotImplementedException(expr2, "Code is not implemented for negating expr2: " + expr2); //$NON-NLS-1$
        }
        if (Utils.equalNotNull(expr2Value, 0)) {
            return b.infixExpression(b.createCopyTarget(expr1), InfixExpression.Operator.PLUS, ctx.getAST().newNumberLiteral("1")); //$NON-NLS-1$
        }

        return b.infixExpression(b.infixExpression(b.createCopyTarget(expr1), InfixExpression.Operator.MINUS, b.createCopyTarget(expr2)), InfixExpression.Operator.PLUS, ctx.getAST().newNumberLiteral("1")); //$NON-NLS-1$
    }

    private Integer intValue(final Expression expression) {
        Object literal= expression.resolveConstantExpressionValue();

        if (literal instanceof Number) {
            return ((Number) literal).intValue();
        }

        return null;
    }

    private void collectLength(final Expression condition, final IVariableBinding incrementedIdx,
            final SystemArrayCopyParams params) {
        InfixExpression ie= ASTNodes.as(condition, InfixExpression.class);

        if (ie != null) {
            if (ASTNodes.hasOperator(ie, InfixExpression.Operator.LESS, InfixExpression.Operator.LESS_EQUALS)) {
                collectLength(incrementedIdx, params, ie, ie.getLeftOperand(), ie.getRightOperand());
            } else if (ASTNodes.hasOperator(ie, InfixExpression.Operator.GREATER, InfixExpression.Operator.GREATER_EQUALS)) {
                collectLength(incrementedIdx, params, ie, ie.getRightOperand(), ie.getLeftOperand());
            }
        }
    }

    private void collectLength(final IVariableBinding incrementedIdx, final SystemArrayCopyParams params,
            final InfixExpression ie, final Expression variable, final Expression boundary) {
        IVariableBinding conditionIdx= getVariableBinding(variable);
        if (Utils.equalNotNull(incrementedIdx, conditionIdx)) {
            if (ASTNodes.hasOperator(ie, InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.GREATER_EQUALS)) {
                params.length= minusPlusOne(boundary, params.indexStartPos);
            } else {
                params.length= minus(boundary, params.indexStartPos);
            }
        }
    }

    private boolean maybeReplaceWithSystemArrayCopyCloneAll(final ForStatement node, final SystemArrayCopyParams params) {
        if (params.srcArrayExpression == null || params.srcPos == null || params.destArrayExpression == null
                || params.destPos == null || params.length == null) {
            return true;
        }

        ASTNodeFactory b= this.ctx.getASTBuilder();
        replaceWithSystemArrayCopy(node, b.createCopyTarget(params.srcArrayExpression), params.srcPos,
                b.createCopyTarget(params.destArrayExpression), params.destPos, params.length);
        return false;
    }

    private void replaceWithSystemArrayCopy(final ForStatement node, final Expression srcArrayExpression, final Expression srcPos,
            final Expression destArrayExpression, final Expression destPos, final Expression length) {
        ASTNodeFactory b= this.ctx.getASTBuilder();
        TryStatement tryS= b.try0(
                b.block(b
                        .toStatement(b.invoke(System.class.getSimpleName(), "arraycopy", srcArrayExpression, srcPos, destArrayExpression, destPos, length))), //$NON-NLS-1$
                b.catch0(IndexOutOfBoundsException.class.getSimpleName(), "e", //$NON-NLS-1$
                        b.throw0(b.new0(ArrayIndexOutOfBoundsException.class.getSimpleName(), b.invoke("e", "getMessage"))))); //$NON-NLS-1$ //$NON-NLS-2$

        this.ctx.getRefactorings().replace(node, tryS);
    }

    private void collectUniqueIndex(final ForStatement node, final SystemArrayCopyParams params) {
        if (ASTNodes.initializers(node).size() != 1) {
            return;
        }

        Expression initializer0= ASTNodes.initializers(node).get(0);

        if (initializer0 instanceof VariableDeclarationExpression) {
            VariableDeclarationExpression vde= (VariableDeclarationExpression) initializer0;
            if (ASTNodes.isPrimitive(vde, int.class.getSimpleName()) && ASTNodes.fragments(vde).size() == 1) {
                // This must be the array index
                VariableDeclarationFragment vdf= ASTNodes.fragments(vde).get(0);
                if (vdf.getExtraDimensions() == 0) {
                    params.indexStartPos= vdf.getInitializer();
                    params.indexVarBinding= vdf.resolveBinding();
                }
            }
        } else if (initializer0 instanceof Assignment) {
            Assignment as= (Assignment) initializer0;
            if (ASTNodes.hasOperator(as, Assignment.Operator.ASSIGN) && ASTNodes.isPrimitive(as.resolveTypeBinding(), int.class.getSimpleName())) {
                // This must be the array index
                params.indexStartPos= as.getRightHandSide();
                SimpleName lhs= ASTNodes.as(as.getLeftHandSide(), SimpleName.class);

                if (lhs != null) {
                    IBinding binding= lhs.resolveBinding();
                    if (binding instanceof IVariableBinding) {
                        params.indexVarBinding= (IVariableBinding) binding;
                    }
                }
            }
        }
    }

    private IVariableBinding getUniqueIncrementedVariable(final ForStatement node) {
        if (ASTNodes.updaters(node).size() != 1) {
            return null;
        }
        Expression updater0= ASTNodes.updaters(node).get(0);
        if (updater0 instanceof PostfixExpression) {
            PostfixExpression pe= (PostfixExpression) updater0;
            if (ASTNodes.hasOperator(pe, PostfixExpression.Operator.INCREMENT)) {
                return getVariableBinding(pe.getOperand());
            }
        } else if (updater0 instanceof PrefixExpression) {
            PrefixExpression pe= (PrefixExpression) updater0;
            if (ASTNodes.hasOperator(pe, PrefixExpression.Operator.INCREMENT)) {
                return getVariableBinding(pe.getOperand());
            }
        }

        return null;
    }

    private IVariableBinding getVariableBinding(final Expression e) {
        SimpleName sn= ASTNodes.as(e, SimpleName.class);

        if (sn != null) {
            IBinding binding= sn.resolveBinding();
            if (binding instanceof IVariableBinding) { // This is a local variable or a field
                return (IVariableBinding) binding;
            }
        }

        return null;
    }
}
