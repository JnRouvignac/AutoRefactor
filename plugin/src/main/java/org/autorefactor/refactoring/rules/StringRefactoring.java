/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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
import org.eclipse.jdt.core.dom.StringLiteral;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

import java.util.concurrent.atomic.AtomicBoolean;

/** See {@link #getDescription()} method. */
public class StringRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Removes:\n"
            + "- creating a String instance from a String constant or literal,\n"
            + "- calling String.toString() on a String instance,\n"
            + "- remove calls to String.toString() inside String concatenations,\n"
            + "- replace forced string tranformation by String.valueOf(),\n"
            + "- replace useless case shifts for equality by equalsIgnoreCase().";
    }

    @Override
    public String getName() {
        return "String";
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        if (hasType(node, "java.lang.String")
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);
            if (hasType(arg0, "java.lang.String")) {
                final ASTBuilder b = ctx.getASTBuilder();
                ctx.getRefactorings().replace(node, b.parenthesizeIfNeeded(b.copy(arg0)));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final Expression expr = node.getExpression();
        final ASTNode parent = node.getParent();
        final ASTBuilder b = this.ctx.getASTBuilder();
        final boolean isStringValueOf = isStringValueOf(node);
        if (isMethod(node, "java.lang.Object", "toString")) {
            if (hasType(expr, "java.lang.String")) {
                // if node is already a String, no need to call toString()
                this.ctx.getRefactorings().replace(node, b.move(expr));
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
                    ctx.getRefactorings().replace(node, replaceToString(node.getExpression()));
                    return DO_NOT_VISIT_SUBTREE;
                } else if (leftOpIsString && isMethod(rmi, "java.lang.Object", "toString")) {
                    ctx.getRefactorings().replace(rmi, replaceToString(rmi.getExpression()));
                    return DO_NOT_VISIT_SUBTREE;
                } else if (rightOpIsString && node.equals(lmi)) {
                    ctx.getRefactorings().replace(lmi, replaceToString(lmi.getExpression()));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        } else if (isStringValueOf && hasType(arg0(node), "java.lang.String")) {
            this.ctx.getRefactorings().replace(node, b.move(arg0(node)));
            return DO_NOT_VISIT_SUBTREE;
        } else if ((isToStringForPrimitive(node) || isStringValueOf)
                && parent.getNodeType() == INFIX_EXPRESSION) {
            // if node is in a String context, no need to call toString()
            final InfixExpression ie = (InfixExpression) node.getParent();
            final Expression lo = ie.getLeftOperand();
            final Expression ro = ie.getRightOperand();
            if (node.equals(lo)) {
                if (hasType(ro, "java.lang.String")) {
                    replaceStringValueOfByArg0(lo, node);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else if (node.equals(ro)) {
                if (hasType(lo, "java.lang.String")
                        // Do not refactor left and right operand at the same time
                        // to avoid compilation errors post refactoring
                        && !ctx.getRefactorings().hasBeenRefactored(lo)) {
                    replaceStringValueOfByArg0(ro, node);
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else {
                // left or right operation is necessarily a string, so just replace
                replaceStringValueOfByArg0(node, node);
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (isMethod(node, "java.lang.String", "equals", "java.lang.Object")) {
            final MethodInvocation leftInvocation = as(node.getExpression(), MethodInvocation.class);
            final MethodInvocation rightInvocation = as(arg0(node), MethodInvocation.class);

            if (leftInvocation != null && rightInvocation != null
                    && (
                            (isMethod(leftInvocation, "java.lang.String", "toLowerCase")
                                    && isMethod(rightInvocation, "java.lang.String", "toLowerCase"))
                            || (isMethod(leftInvocation, "java.lang.String", "toUpperCase")
                                    && isMethod(rightInvocation, "java.lang.String", "toUpperCase"))
                            )) {
                final Expression leftExpr = leftInvocation.getExpression();
                final Expression rightExpr = rightInvocation.getExpression();
                this.ctx.getRefactorings().replace(node, b.invoke(b.copy(leftExpr),
                        "equalsIgnoreCase", b.copy(rightExpr)));
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (isMethod(node, "java.lang.String", "equalsIgnoreCase", "java.lang.String")) {
            final AtomicBoolean isRefacoringNeeded = new AtomicBoolean(false);

            final Expression leftExpr = getReducedStringExpression(node.getExpression(), isRefacoringNeeded);
            final Expression rightExpr = getReducedStringExpression(arg0(node), isRefacoringNeeded);

            if (isRefacoringNeeded.get()) {
                this.ctx.getRefactorings().replace(node, b.invoke(b.copy(leftExpr),
                        "equalsIgnoreCase", b.copy(rightExpr)));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private Expression getReducedStringExpression(final Expression stringExpr,
            final AtomicBoolean isRefacoringNeeded) {
        final MethodInvocation casingInvocation = as(stringExpr, MethodInvocation.class);
        if (casingInvocation != null && (isMethod(casingInvocation, "java.lang.String", "toLowerCase")
                || isMethod(casingInvocation, "java.lang.String", "toUpperCase"))) {
            isRefacoringNeeded.set(true);
            return casingInvocation.getExpression();
        }
        return stringExpr;
    }

    private void replaceStringValueOfByArg0(final Expression toReplace, final MethodInvocation mi) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        ctx.getRefactorings().replace(toReplace, b.parenthesizeIfNeeded(b.move(arg0(mi))));
    }

    private Expression replaceToString(final Expression expr) {
        final ASTBuilder b = ctx.getASTBuilder();
        if (expr != null) {
            return b.move(expr);
        } else {
            return b.this0();
        }
    }

    private boolean isToStringForPrimitive(final MethodInvocation node) {
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

    private boolean isStringValueOf(final MethodInvocation node) {
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

    @Override
    public boolean visit(InfixExpression node) {
        if (InfixExpression.Operator.PLUS.equals(node.getOperator())
                && extendedOperands(node).isEmpty()) {
            final Expression leftOperand = node.getLeftOperand();
            final Expression rightOperand = node.getRightOperand();

            return maybeReplaceStringConcatenation(node, leftOperand, rightOperand)
                // if not replaced then try the other way round
                && maybeReplaceStringConcatenation(node, rightOperand, leftOperand);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeReplaceStringConcatenation(
            final InfixExpression node, final Expression expr, final Expression variable) {
        if (expr instanceof StringLiteral
                && ((StringLiteral) expr).getLiteralValue().matches("")
                && !hasType(variable, "java.lang.String", "char[]")) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.invoke("String", "valueOf", b.copy(variable)));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}
