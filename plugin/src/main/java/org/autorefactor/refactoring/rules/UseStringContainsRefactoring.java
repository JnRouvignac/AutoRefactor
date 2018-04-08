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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class UseStringContainsRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Use String.contains()";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replaces uses of String.indexOf(String) String.lastIndexOf(String)"
            + " by String.contains(CharSequence) where appropriate.";
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
    public boolean visit(MethodInvocation node) {
        final ASTNode parent = getFirstAncestorWithoutParentheses(node);
        if (parent instanceof InfixExpression
                && (isMethod(node, "java.lang.String", "indexOf", "java.lang.String")
                        || isMethod(node, "java.lang.String", "lastIndexOf", "java.lang.String"))) {
            final InfixExpression ie = (InfixExpression) parent;
            if (is(ie, node, Operator.GREATER_EQUALS, 0)) {
                return replaceWithStringContains(ie, node, false);
            } else if (is(ie, node, Operator.LESS, 0)) {
                return replaceWithStringContains(ie, node, true);
            } else if (is(ie, node, Operator.NOT_EQUALS, -1)) {
                return replaceWithStringContains(ie, node, false);
            } else if (is(ie, node, Operator.EQUALS, -1)) {
                return replaceWithStringContains(ie, node, true);
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithStringContains(InfixExpression ie, MethodInvocation node, boolean negate) {
        final Refactorings r = this.ctx.getRefactorings();
        final ASTBuilder b = this.ctx.getASTBuilder();
        r.set(node, MethodInvocation.NAME_PROPERTY, b.simpleName("contains"));
        if (negate) {
            r.replace(ie, b.not(b.move(node)));
        } else {
            r.replace(ie, b.move(node));
        }
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean is(final InfixExpression ie, MethodInvocation node, Operator operator, Integer constant) {
        final Expression leftOp = removeParentheses(ie.getLeftOperand());
        final Expression rightOp = removeParentheses(ie.getRightOperand());
        if (hasOperator(ie, operator)) {
            if (leftOp.equals(node)
                    && constant.equals(rightOp.resolveConstantExpressionValue())) {
                return true;
            } else if (rightOp.equals(node)
                    && constant.equals(leftOp.resolveConstantExpressionValue())) {
                return true;
            }
        }
        return false;
    }

    private ASTNode getFirstAncestorWithoutParentheses(ASTNode node) {
        final ASTNode parent = node.getParent();
        if (node.getNodeType() == ASTNode.PARENTHESIZED_EXPRESSION) {
            return getFirstAncestorWithoutParentheses(parent);
        }
        return parent;
    }
}
