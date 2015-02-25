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

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.IllegalArgumentException;
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
        if (hasType(node, "java.lang.String")
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);
            if (hasType(arg0, "java.lang.String")) {
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
        if (isMethod(node, "java.lang.Object", "toString")) {
            if (hasType(expression, "java.lang.String")) {
                // if node is already a String, no need to call toString()
                this.ctx.getRefactorings().replace(node, b.move(expression));
                return DO_NOT_VISIT_SUBTREE;
            } else if (parent.getNodeType() == INFIX_EXPRESSION) {
                // if node is in a String context, no need to call toString()
                final InfixExpression ie = (InfixExpression) node.getParent();
                final Expression leftOp = ie.getLeftOperand();
                final Expression rightOp = ie.getRightOperand();
                final boolean leftOpIsString = hasType(leftOp, "java.lang.String");
                final boolean rightOpIsString = hasType(rightOp, "java.lang.String");
                final MethodInvocation lmi = as(leftOp, MethodInvocation.class);
                final MethodInvocation rmi = as(rightOp, MethodInvocation.class);
                if (!node.equals(lmi)
                        && !node.equals(rmi)
                        && (leftOpIsString || rightOpIsString)) {
                    // node is in the extended operands
                    this.ctx.getRefactorings().replace(node, b.move(node.getExpression()));
                    return VISIT_SUBTREE;
                } else if (leftOpIsString && isMethod(rmi, "java.lang.Object", "toString")) {
                    this.ctx.getRefactorings().replace(rmi, b.move(rmi.getExpression()));
                    return VISIT_SUBTREE;
                } else if (rightOpIsString && node.equals(lmi)) {
                    this.ctx.getRefactorings().replace(lmi, b.move(lmi.getExpression()));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        } else if (isToStringForPrimitive(node) || isStringValueOf(node)) {
            if (parent.getNodeType() == INFIX_EXPRESSION) {
                // if node is in a String context, no need to call toString()
                final InfixExpression ie = (InfixExpression) node.getParent();
                final Expression lo = ie.getLeftOperand();
                final Expression ro = ie.getRightOperand();
                final MethodInvocation lmi = as(lo, MethodInvocation.class);
                final MethodInvocation rmi = as(ro, MethodInvocation.class);
                if (hasType(lo, "java.lang.String") && node.equals(rmi)) {
                    this.ctx.getRefactorings().replace(rmi, b.move(arg0(rmi)));
                    return VISIT_SUBTREE;
                } else if (hasType(ro, "java.lang.String") && node.equals(lmi)) {
                    this.ctx.getRefactorings().replace(lmi, b.move(arg0(lmi)));
                    return DO_NOT_VISIT_SUBTREE;
                } else {
                    // left or right operation is necessarily a string, so just replace
                    this.ctx.getRefactorings().replace(node, b.move(arg0(node)));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean isToStringForPrimitive(MethodInvocation node) {
        return "toString".equals(node.getName().getIdentifier()) // fast-path
                && (isMethod(node, "java.lang.Boolean", "toString", "boolean")
                      || isMethod(node, "java.lang.Character", "toString", "char")
                      || isMethod(node, "java.lang.Byte", "toString", "byte")
                      || isMethod(node, "java.lang.Short", "toString", "short")
                      || isMethod(node, "java.lang.Integer", "toString", "int")
                      || isMethod(node, "java.lang.Long", "toString", "long")
                      || isMethod(node, "java.lang.Float", "toString", "float")
                      || isMethod(node, "java.lang.Double", "toString", "double"));
    }

    private boolean isStringValueOf(MethodInvocation node) {
        return hasType(node.getExpression(), "java.lang.String") // fast-path
                && (isMethod(node, "java.lang.String", "valueOf", "boolean")
                      || isMethod(node, "java.lang.String", "valueOf", "char")
                      || isMethod(node, "java.lang.String", "valueOf", "byte")
                      || isMethod(node, "java.lang.String", "valueOf", "short")
                      || isMethod(node, "java.lang.String", "valueOf", "int")
                      || isMethod(node, "java.lang.String", "valueOf", "long")
                      || isMethod(node, "java.lang.String", "valueOf", "float")
                      || isMethod(node, "java.lang.String", "valueOf", "double")
                      || isMethod(node, "java.lang.String", "valueOf", "java.lang.Object"));
    }

    private Expression arg0(final MethodInvocation mi) {
        final List<Expression> args = arguments(mi);
        if (args.isEmpty()) {
            throw new IllegalArgumentException(mi, "The arguments must not be empty for method " + mi);
        }
        return args.get(0);
    }
}
