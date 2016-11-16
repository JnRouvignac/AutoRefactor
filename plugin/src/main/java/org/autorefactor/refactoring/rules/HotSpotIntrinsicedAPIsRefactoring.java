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
package org.autorefactor.refactoring.rules;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ArrayAccess;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
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
import static org.autorefactor.util.Utils.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class HotSpotIntrinsicedAPIsRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Refactors code patterns to use intrinsiced APIs in Hotspot JVM.\n"
            + "Intrinsics are APIs that receive special treatment when JITed:"
            + " they can be compiled down to use very efficient CPU instructions.";
    }

    @Override
    public String getName() {
        return "HotSpot intrinsiced APIs";
    }

    private static class SystemArrayCopyParams {
        private IVariableBinding indexVarBinding;
        private Expression indexStartPos;
        private Expression srcArrayExpr;
        private Expression srcPos;
        private Expression destArrayExpr;
        private Expression destPos;
        private Expression length;

        @Override
        public String toString() {
            return "System.arraycopy("
                    + srcArrayExpr + ", " + srcPos + ", "
                    + destArrayExpr + ", " + destPos + ", "
                    + length + ")";
        }
    }

    @Override
    public boolean visit(ForStatement node) {
        final SystemArrayCopyParams params = new SystemArrayCopyParams();
        collectUniqueIndex(node, params);
        final IVariableBinding incrementedIdx = getUniqueIncrementedVariable(node);
        final List<Statement> stmts = asList(node.getBody());
        if (equalNotNull(params.indexVarBinding, incrementedIdx)
                && stmts.size() == 1) {
            collectLength(node.getExpression(), incrementedIdx, params);

            final Assignment as = asExpression(stmts.get(0), Assignment.class);
            if (hasOperator(as, ASSIGN)) {
                final Expression lhs = as.getLeftHandSide();
                final Expression rhs = as.getRightHandSide();
                if (lhs instanceof ArrayAccess && rhs instanceof ArrayAccess) {
                    final ArrayAccess aaLHS = (ArrayAccess) lhs;
                    final ArrayAccess aaRHS = (ArrayAccess) rhs;
                    params.destArrayExpr = aaLHS.getArray();
                    params.srcArrayExpr = aaRHS.getArray();
                    if (haveSameType(params.srcArrayExpr, params.destArrayExpr)) {
                        params.destPos = calcIndex(aaLHS.getIndex(), params);
                        params.srcPos = calcIndex(aaRHS.getIndex(), params);
                        return replaceWithSystemArrayCopyCloneAll(node, params);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private Expression calcIndex(Expression index, SystemArrayCopyParams params) {
        if (index instanceof SimpleName) {
            final IVariableBinding idxVar = getVariableBinding(index);
            if (equalNotNull(params.indexVarBinding, idxVar)) {
                return params.indexStartPos;
            }
        } else if (index instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) index;
            if (!ie.hasExtendedOperands()
                    && hasOperator(ie, PLUS)) {
                final Expression leftOp = ie.getLeftOperand();
                final Expression rightOp = ie.getRightOperand();
                if (leftOp instanceof SimpleName) {
                    final IVariableBinding idxVar = getVariableBinding(leftOp);
                    if (equalNotNull(params.indexVarBinding, idxVar)) {
                        return plus(rightOp, params.indexStartPos);
                    }
                }
                if (rightOp instanceof SimpleName) {
                    final IVariableBinding idxVar = getVariableBinding(rightOp);
                    if (equalNotNull(params.indexVarBinding, idxVar)) {
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
        } else if (equalNotNull(expr1Value, 0)) {
            throw new NotImplementedException(expr2, "Code is not implemented for negating expr2: " + expr2);
        } else if (equalNotNull(expr2Value, 0)) {
            return expr1;
        }
        return b.infixExpr(
                b.copy(expr1),
                MINUS,
                b.copy(expr2));
    }

    private Expression plus(Expression expr1, Expression expr2) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final Integer expr1Value = intValue(expr1);
        final Integer expr2Value = intValue(expr2);
        if (expr1Value != null && expr2Value != null) {
            return b.int0(expr1Value + expr2Value);
        } else if (equalNotNull(expr1Value, 0)) {
            return expr2;
        } else if (equalNotNull(expr2Value, 0)) {
            return expr1;
        }
        return b.infixExpr(
                b.copy(expr1),
                PLUS,
                b.copy(expr2));
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
            if (hasOperator(ie, LESS)) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getLeftOperand());
                if (equalNotNull(incrementedIdx, conditionIdx)) {
                    params.length = ie.getRightOperand();
                }
            } else if (hasOperator(ie, LESS_EQUALS)) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getLeftOperand());
                if (equalNotNull(incrementedIdx, conditionIdx)) {
                    params.length = minus(
                            plus(ie.getRightOperand(), ctx.getAST().newNumberLiteral("1")),
                            params.indexStartPos);
                }
            } else if (hasOperator(ie, GREATER)) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getRightOperand());
                if (equalNotNull(incrementedIdx, conditionIdx)) {
                    params.length = ie.getLeftOperand();
                }
            } else if (hasOperator(ie, GREATER_EQUALS)) {
                IVariableBinding conditionIdx = getVariableBinding(ie.getRightOperand());
                if (equalNotNull(incrementedIdx, conditionIdx)) {
                    params.length = minus(
                            plus(ie.getLeftOperand(), ctx.getAST().newNumberLiteral("1")),
                            params.indexStartPos);
                }
            }
        }
    }

    private boolean replaceWithSystemArrayCopyCloneAll(ForStatement node, SystemArrayCopyParams params) {
        if (params.srcArrayExpr == null
                || params.srcPos == null
                || params.destArrayExpr == null
                || params.destPos == null
                || params.length == null) {
            return VISIT_SUBTREE;
        }
        final ASTBuilder b = this.ctx.getASTBuilder();
        return replaceWithSystemArrayCopy(node,
                b.copy(params.srcArrayExpr),
                b.copy(params.srcPos),
                b.copy(params.destArrayExpr),
                b.copy(params.destPos),
                b.copy(params.length));
    }

    private boolean replaceWithSystemArrayCopy(ForStatement node,
            Expression srcArrayExpr, Expression srcPos,
            Expression destArrayExpr, Expression destPos,
            Expression length) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final TryStatement tryS = b.try0(
                b.block(
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

    private void collectUniqueIndex(ForStatement node, SystemArrayCopyParams params) {
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
        } else if (initializer0 instanceof Assignment) {
            final Assignment as = (Assignment) initializer0;
            if (hasOperator(as, ASSIGN)
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
            if (hasOperator(pe, PostfixExpression.Operator.INCREMENT)) {
                return getVariableBinding(pe.getOperand());
            }
        } else if (updater0 instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) updater0;
            if (hasOperator(pe, PrefixExpression.Operator.INCREMENT)) {
                return getVariableBinding(pe.getOperand());
            }
        }
        return null;
    }

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
}
