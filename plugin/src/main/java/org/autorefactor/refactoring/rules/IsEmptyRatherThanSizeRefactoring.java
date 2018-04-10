/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class IsEmptyRatherThanSizeRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Empty test rather than size";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replaces some checks on Collection.size() or Map.size() with checks on isEmpty().";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility of such simple code to spotlight the complexity of other code.";
    }

    @Override
    public boolean visit(InfixExpression node) {
        final MethodInvocation leftMi = as(node.getLeftOperand(), MethodInvocation.class);
        final Long rightLiteral = asNumber(node.getRightOperand());

        final MethodInvocation rightMi = as(node.getRightOperand(), MethodInvocation.class);
        final Long leftLiteral = asNumber(node.getLeftOperand());

        if ((isMethod(leftMi, "java.util.Collection", "size")
                || isMethod(leftMi, "java.util.Map", "size")
                || isMethod(leftMi, "java.lang.String", "length"))
                && rightLiteral != null) {
            return replaceCollectionSize(node, leftMi, sign(node.getOperator(), true),
                    rightLiteral);
        } else if ((isMethod(rightMi, "java.util.Collection", "size")
                || isMethod(rightMi, "java.util.Map", "size")
                || isMethod(rightMi, "java.lang.String", "length"))
                && leftLiteral != null) {
            return replaceCollectionSize(node, rightMi, sign(node.getOperator(), false),
                    leftLiteral);
        }

        return VISIT_SUBTREE;
    }

    private boolean replaceCollectionSize(final InfixExpression node, final MethodInvocation miToReplace,
            final Operator operator, final long literalSize) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();

        if (literalSize == 0) {
            if (GREATER_EQUALS.equals(operator)) {
                r.replace(node,
                        b.boolean0(true));
                return DO_NOT_VISIT_SUBTREE;
            } else if (LESS.equals(operator)) {
                r.replace(node,
                        b.boolean0(false));
            } else if (GREATER.equals(operator)) {
                r.replace(node,
                        b.not(b.invoke(b.copyExpression(miToReplace), "isEmpty")));
                return DO_NOT_VISIT_SUBTREE;
            } else if (EQUALS.equals(operator)) {
                r.replace(node,
                        b.invoke(b.copyExpression(miToReplace), "isEmpty"));
                return DO_NOT_VISIT_SUBTREE;
            } else if (NOT_EQUALS.equals(operator)) {
                r.replace(node,
                        b.not(b.invoke(b.copyExpression(miToReplace), "isEmpty")));
                return DO_NOT_VISIT_SUBTREE;
            } else if (LESS_EQUALS.equals(operator)) {
                r.replace(node,
                        b.invoke(b.copyExpression(miToReplace), "isEmpty"));
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (literalSize == 1) {
            if (GREATER_EQUALS.equals(operator)) {
                r.replace(node,
                        b.not(b.invoke(b.copyExpression(miToReplace), "isEmpty")));
                return DO_NOT_VISIT_SUBTREE;
            } else if (LESS.equals(operator)) {
                r.replace(node,
                        b.invoke(b.copyExpression(miToReplace), "isEmpty"));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private Long asNumber(final Expression expr) {
        Long longValue = null;
        if (expr != null) {
            final Object val = expr.resolveConstantExpressionValue();
            if (val instanceof Integer) {
                longValue = (long) ((Integer) val).intValue();
            } else if (val instanceof Long) {
                longValue = (Long) val;
            }
        }
        return longValue;
    }

    private Operator sign(final Operator operator, final boolean collectionFirst) {
        if (!collectionFirst) {
            if (LESS.equals(operator)) {
                return GREATER;
            } else if (LESS_EQUALS.equals(operator)) {
                return GREATER_EQUALS;
            } else if (GREATER.equals(operator)) {
                return LESS;
            } else if (GREATER_EQUALS.equals(operator)) {
                return LESS_EQUALS;
            }
        }
        return operator;
    }
}
