/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

import static org.autorefactor.refactoring.ASTHelper.*;

public class HotSpotIntrinsicedAPIsRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private class SystemArrayCopyParams {
        private IVariableBinding indexVarBinding;
        private Expression indexStartPos;
        private Expression srcArrayExpr;
        private Expression srcPos;
        private Expression destArrayExpr;
        private Expression destPos;
        private Expression endPos;
    }

    private RefactoringContext ctx;

    public HotSpotIntrinsicedAPIsRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ForStatement node) {
        final SystemArrayCopyParams params = new SystemArrayCopyParams();
        collectUniqueIndex(node, params);
        final IVariableBinding incrementedIdx = getUniqueIncrementedVariable(node);
        final List<Statement> stmts = asList(node.getBody());
        if (equalsNotNull(params.indexVarBinding, incrementedIdx)
                && stmts.size() == 1) {
            collectLength(node.getExpression(), incrementedIdx, params);

            final Statement stmt = stmts.get(0);
            if (stmt instanceof ExpressionStatement) {
                final Assignment as = asExpression(stmt, Assignment.class);
                final Expression lhs = as.getLeftHandSide();
                final Expression rhs = as.getRightHandSide();
                if (lhs instanceof ArrayAccess && rhs instanceof ArrayAccess) {
                    final ArrayAccess aaLHS = (ArrayAccess) lhs;
                    params.destArrayExpr = aaLHS.getArray();
                    params.destPos = calcIndex(aaLHS.getIndex(), params);

                    final ArrayAccess aaRHS = (ArrayAccess) rhs;
                    params.srcArrayExpr = aaRHS.getArray();
                    params.srcPos = calcIndex(aaRHS.getIndex(), params);
                    return replaceWithSystemArrayCopyCloneAll(node, params);
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private Expression calcIndex(Expression index, SystemArrayCopyParams params) {
        if (index instanceof SimpleName) {
            final IVariableBinding idxVar = getVariableBinding(index);
            if (equalsNotNull(params.indexVarBinding, idxVar)) {
                return params.indexStartPos;
            }
        } else if (index instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) index;
            if (InfixExpression.Operator.PLUS.equals(ie.getOperator())) {
                final Expression leftOp = ie.getLeftOperand();
                final Expression rightOp = ie.getRightOperand();
                if (leftOp instanceof SimpleName) {
                    final IVariableBinding idxVar = getVariableBinding(leftOp);
                    if (equalsNotNull(params.indexVarBinding, idxVar)) {
                        return plus(rightOp, params.indexStartPos);
                    }
                }
                if (rightOp instanceof SimpleName) {
                    final IVariableBinding idxVar = getVariableBinding(rightOp);
                    if (equalsNotNull(params.indexVarBinding, idxVar)) {
                        return plus(leftOp, params.indexStartPos);
                    }
                }
            }
        }
        return null;
    }

    private Expression minus(Expression expr1, Expression expr2) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final Integer expr1Value = intValue(expr1);
        final Integer expr2Value = intValue(expr2);
        if (expr1Value != null && expr2Value != null) {
            return b.int0(expr1Value - expr2Value);
        }
        else if (expr1Value != null && expr1Value == 0) {
            // TODO negate expr2
            throw new NotImplementedException();
        }
        else if (expr2Value != null && expr2Value == 0) {
            return b.copyExpr(expr1);
        }
        return b.infixExpr(
                b.copyExpr(expr1),
                InfixExpression.Operator.MINUS,
                b.copyExpr(expr2));
    }

    private Expression plus(Expression expr1, Expression expr2) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final Integer expr1Value = intValue(expr1);
        final Integer expr2Value = intValue(expr2);
        if (expr1Value != null && expr2Value != null) {
            return b.int0(expr1Value + expr2Value);
        }
        else if (expr1Value != null && expr1Value == 0) {
            return b.copyExpr(expr2);
        }
        else if (expr2Value != null && expr2Value == 0) {
            return b.copyExpr(expr1);
        }
        return b.infixExpr(
                b.copyExpr(expr1),
                InfixExpression.Operator.PLUS,
                b.copyExpr(expr2));
    }

    private Integer intValue(Expression expr) {
        if (expr instanceof NumberLiteral) {
            try {
                return Integer.parseInt(((NumberLiteral) expr).getToken());
            } catch (NumberFormatException ignored) {
                // this is not an int, nothing to do
            }
        }
        return null;
    }

    private void collectLength(final Expression condition,
            final IVariableBinding incrementedIdx, final SystemArrayCopyParams params) {
        if (condition instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) condition;
            if (InfixExpression.Operator.LESS.equals(ie.getOperator())) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getLeftOperand());
                if (equalsNotNull(incrementedIdx, conditionIdx)) {
                    params.endPos = ie.getRightOperand();
                }
            } else if (InfixExpression.Operator.LESS_EQUALS.equals(ie.getOperator())) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getLeftOperand());
                if (equalsNotNull(incrementedIdx, conditionIdx)) {
                    params.endPos = minus(
                            plus(ie.getRightOperand(), ctx.getAST().newNumberLiteral("1")),
                            params.indexStartPos);
                }
            } else if (InfixExpression.Operator.GREATER.equals(ie.getOperator())) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getRightOperand());
                if (equalsNotNull(incrementedIdx, conditionIdx)) {
                    params.endPos = ie.getLeftOperand();
                }
            } else if (InfixExpression.Operator.GREATER_EQUALS.equals(ie.getOperator())) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getRightOperand());
                if (equalsNotNull(incrementedIdx, conditionIdx)) {
                    params.endPos = minus(
                            plus(ie.getLeftOperand(), ctx.getAST().newNumberLiteral("1")),
                            params.indexStartPos);
                }
            }
        }
    }

    private boolean equalsNotNull(final Object o1, final Object o2) {
        return o1 != null && o1.equals(o2);
    }

    private boolean replaceWithSystemArrayCopyCloneAll(ForStatement node,
            SystemArrayCopyParams params) {
        if (params.srcArrayExpr == null
                || params.srcPos == null
                || params.destArrayExpr == null
                || params.destPos == null
                || params.endPos == null) {
            return VISIT_SUBTREE;
        }
        final ASTBuilder b = this.ctx.getASTBuilder();
        return replaceWithSystemArrayCopy(node,
                b.copyExpr(params.srcArrayExpr),
                b.copyExpr(params.srcPos),
                b.copyExpr(params.destArrayExpr),
                b.copyExpr(params.destPos),
                b.copyExpr(params.endPos));
    }

    private boolean replaceWithSystemArrayCopy(ForStatement node,
            Expression srcArrayExpr, Expression srcPos,
            Expression destArrayExpr, Expression destPos,
            Expression length) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final TryStatement tryS = b.try0(
                b.body(
                        b.toStmt(
                                b.invoke("System", "arraycopy",
                                        srcArrayExpr, srcPos, destArrayExpr, destPos, length))),
                b.catch0("IndexOutOfBoundsException", "e",
                        b.throw0(
                                b.new0("ArrayIndexOutOfBoundsException",
                                        b.invoke("e", "getMessage")))));

        this.ctx.getRefactorings().replace(node, tryS);
        return DO_NOT_VISIT_SUBTREE;
    }

    private void collectUniqueIndex(ForStatement node,
            SystemArrayCopyParams params) {
        if (initializers(node).size() != 1) {
            return;
        }
        final Expression initializer0 = initializers(node).get(0);
        if (initializer0 instanceof VariableDeclarationExpression) {
            final VariableDeclarationExpression vde =
                    (VariableDeclarationExpression) initializer0;
            if (isPrimitive(vde, "int") && fragments(vde).size() == 1) {
                // this must be the array index
                VariableDeclarationFragment vdf = fragments(vde).get(0);
                if (vdf.getExtraDimensions() == 0) {
                    params.indexStartPos = vdf.getInitializer();
                    params.indexVarBinding = vdf.resolveBinding();
                    return;
                }
            }
        }
        else if (initializer0 instanceof Assignment) {
            final Assignment as = (Assignment) initializer0;
            if (Assignment.Operator.ASSIGN.equals(as.getOperator())
                    && isPrimitive(as.resolveTypeBinding(), "int")) {
                // this must be the array index
                params.indexStartPos = as.getRightHandSide();
                final Expression lhs = as.getLeftHandSide();
                if (lhs instanceof SimpleName) {
                    final IBinding binding = ((SimpleName) lhs).resolveBinding();
                    if (binding instanceof IVariableBinding) {
                        params.indexVarBinding = (IVariableBinding) binding;
                        return;
                    }
                }
            }
        }
    }

    private IVariableBinding getUniqueIncrementedVariable(ForStatement node) {
        if (updaters(node).size() != 1) {
            return null;
        }
        final Expression updater0 = updaters(node).get(0);
        if (updater0 instanceof PostfixExpression) {
            final PostfixExpression pe = (PostfixExpression) updater0;
            if (PostfixExpression.Operator.INCREMENT.equals(pe.getOperator())) {
                return getVariableBinding(pe.getOperand());
            }
        }
        else if (updater0 instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) updater0;
            if (PrefixExpression.Operator.INCREMENT.equals(pe.getOperator())) {
                return getVariableBinding(pe.getOperand());
            }
        }
        return null;
    }

    // TODO JNR verify that client code null checks the result of calling this method
    private IVariableBinding getVariableBinding(final Expression e) {
        if (e instanceof SimpleName) {
            final SimpleName sn = (SimpleName) e;
            final IBinding binding = sn.resolveBinding();
            if (binding instanceof IVariableBinding) { // this is a local variable or a field
                return (IVariableBinding) binding;
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}
