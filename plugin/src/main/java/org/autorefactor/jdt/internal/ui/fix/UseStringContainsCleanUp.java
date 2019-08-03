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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.hasOperator;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.removeParentheses;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class UseStringContainsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_UseStringContainsCleanUp_reason;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final ASTNode parent= getFirstAncestorWithoutParentheses(node);
        if (parent instanceof InfixExpression && (isMethod(node, String.class.getCanonicalName(), "indexOf", String.class.getCanonicalName())
                || isMethod(node, String.class.getCanonicalName(), "lastIndexOf", String.class.getCanonicalName()))) {
            final InfixExpression ie= (InfixExpression) parent;
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
        return true;
    }

    private boolean replaceWithStringContains(InfixExpression ie, MethodInvocation node, boolean negate) {
        final Refactorings r= this.ctx.getRefactorings();
        final ASTBuilder b= this.ctx.getASTBuilder();
        r.set(node, MethodInvocation.NAME_PROPERTY, b.simpleName("contains"));
        if (negate) {
            r.replace(ie, b.not(b.move(node)));
        } else {
            r.replace(ie, b.move(node));
        }
        return false;
    }

    private boolean is(final InfixExpression ie, MethodInvocation node, Operator operator, Integer constant) {
        final Expression leftOp= removeParentheses(ie.getLeftOperand());
        final Expression rightOp= removeParentheses(ie.getRightOperand());
        return hasOperator(ie, operator)
                && ((leftOp.equals(node) && constant.equals(rightOp.resolveConstantExpressionValue()))
                        || (rightOp.equals(node) && constant.equals(leftOp.resolveConstantExpressionValue())));
    }

    private ASTNode getFirstAncestorWithoutParentheses(ASTNode node) {
        final ASTNode parent= node.getParent();
        if (node.getNodeType() == ASTNode.PARENTHESIZED_EXPRESSION) {
            return getFirstAncestorWithoutParentheses(parent);
        }
        return parent;
    }
}
