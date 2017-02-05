/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
public class IsEmptyRatherThanSizeRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Replaces some checks on Collection.size() with checks on Collection.isEmpty().";
    }

    @Override
    public String getName() {
        return "Empty test rather than size";
    }

    @Override
    public boolean visit(InfixExpression node) {
        final MethodInvocation leftMi = as(node.getLeftOperand(), MethodInvocation.class);
        final MethodInvocation rightMi = as(node.getRightOperand(), MethodInvocation.class);
        if (isMethod(leftMi, "java.util.Collection", "size")
                && isZero(node.getRightOperand())) {
            return replaceCollectionSize(node, leftMi);
        } else if (isMethod(rightMi, "java.util.Collection", "size")
                && isZero(node.getLeftOperand())) {
            return replaceInverseCollectionSize(node, rightMi);
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceCollectionSize(InfixExpression node, MethodInvocation miToReplace) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(miToReplace);
        if (hasOperator(node, GREATER)) {
            r.replace(node,
                    b.not(b.invoke(copyOfExpr, "isEmpty")));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, GREATER_EQUALS)) {
            r.replace(node,
                    b.boolean0(true));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, LESS_EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, LESS)) {
            r.replace(node,
                    b.boolean0(false));
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInverseCollectionSize(InfixExpression node, MethodInvocation miToReplace) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression copyOfExpr = b.copyExpression(miToReplace);
        if (hasOperator(node, LESS)) {
            r.replace(node,
                    b.not(b.invoke(copyOfExpr, "isEmpty")));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, LESS_EQUALS)) {
            r.replace(node,
                    b.boolean0(true));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, GREATER_EQUALS)) {
            r.replace(node,
                    b.invoke(copyOfExpr, "isEmpty"));
            return DO_NOT_VISIT_SUBTREE;
        } else if (hasOperator(node, GREATER)) {
            r.replace(node,
                    b.boolean0(false));
        }
        return VISIT_SUBTREE;
    }

    private boolean isZero(Expression expr) {
        if (expr != null) {
            final Object val = expr.resolveConstantExpressionValue();
            if (val instanceof Integer) {
                return ((Integer) val).intValue() == 0;
            } else if (val instanceof Long) {
                return ((Long) val).longValue() == 0;
            }
        }
        return false;
    }
}
