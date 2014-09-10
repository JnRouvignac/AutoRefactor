/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/**
 * Removes:
 * <ul>
 * <li>Creating a {@link String} instance from a {@link String} constant or
 * literal.</li>
 * <li>Calling {@link String#toString()} on a {@link String} instance</li>
 * <li>Remove calls to {@link String#toString()} inside String concatenations</li>
 * </ul>
 */
public class StringRefactoring extends AbstractRefactoring {


    /** {@inheritDoc} */
    @Override
    public boolean visit(ClassInstanceCreation node) {
        if (hasType(node.getType(), "java.lang.String")
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);
            if (arg0.resolveConstantExpressionValue() != null) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                this.ctx.getRefactorings().replace(node, b.copy(arg0));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        final Expression expression = node.getExpression();
        final ASTNode parent = node.getParent();
        final ASTBuilder b = this.ctx.getASTBuilder();
        if (isToStringInvocation(node)) {
            if (hasType(expression, "java.lang.String")) {
                // if node is already a String, no need to call toString()
                this.ctx.getRefactorings().replace(node, b.move(expression));
                return DO_NOT_VISIT_SUBTREE;
            } else if (parent.getNodeType() == INFIX_EXPRESSION) {
                // if node is in a String context, no need to call toString()
                final InfixExpression ie = (InfixExpression) node.getParent();
                final Expression lo = ie.getLeftOperand();
                final Expression ro = ie.getRightOperand();
                final MethodInvocation lmi = as(lo, MethodInvocation.class);
                final MethodInvocation rmi = as(ro, MethodInvocation.class);
                final boolean leftIsToString = isToStringInvocation(lmi);
                final boolean rightIsToString = isToStringInvocation(rmi);
                if (hasType(lo, "java.lang.String") && rightIsToString) {
                    this.ctx.getRefactorings().replace(rmi, b.move(rmi.getExpression()));
                    return VISIT_SUBTREE;
                } else if (hasType(ro, "java.lang.String") && leftIsToString) {
                    this.ctx.getRefactorings().replace(lmi, b.move(lmi.getExpression()));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean isToStringInvocation(MethodInvocation node) {
        return node != null
                && "toString".equals(node.getName().getIdentifier())
                && arguments(node).isEmpty();
    }
}
