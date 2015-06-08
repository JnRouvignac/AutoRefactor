/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.StringLiteral;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.JavaConstants.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class BigDecimalRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Refactors to a proper use of BigDecimals:\n"
            + "- create BigDecimals from Strings rather than floating point values,\n"
            + "- create BigDecimals from integers rather than String representing integers,\n"
            + "- use BigDecimal constants,\n"
            + "- replace calls to BigDecimal.equals(Object) with calls to BigDecimal.compareTo(BigDecimal).";
    }

    @Override
    public String getName() {
        return "BigDecimal";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        final ITypeBinding typeBinding = node.getType().resolveBinding();
        if (typeBinding != null
                && "java.math.BigDecimal".equals(typeBinding.getQualifiedName())
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);
            if (arg0 instanceof NumberLiteral) {
                final NumberLiteral nb = (NumberLiteral) arg0;
                if (nb.getToken().contains(".")) {
                    // Only instantiation from double, not from integer
                    this.ctx.getRefactorings().replace(nb,
                            getStringLiteral(nb.getToken()));
                } else {
                    if (getJavaMinorVersion() < 5) {
                        return VISIT_SUBTREE;
                    }
                    if (ZERO_LONG_LITERAL_RE.matcher(nb.getToken()).matches()) {
                        replaceWithQualifiedName(node, typeBinding, "ZERO");
                    } else if (ONE_LONG_LITERAL_RE.matcher(nb.getToken()).matches()) {
                        replaceWithQualifiedName(node, typeBinding, "ONE");
                    } else if (TEN_LONG_LITERAL_RE.matcher(nb.getToken()).matches()) {
                        replaceWithQualifiedName(node, typeBinding, "TEN");
                    } else {
                        this.ctx.getRefactorings().replace(node,
                                getValueOf(typeBinding.getName(), nb.getToken()));
                    }
                }
                return DO_NOT_VISIT_SUBTREE;
            } else if (arg0 instanceof StringLiteral) {
                if (getJavaMinorVersion() < 5) {
                    return VISIT_SUBTREE;
                }
                final String literalValue = ((StringLiteral) arg0).getLiteralValue();
                if (literalValue.matches("0+")) {
                    replaceWithQualifiedName(node, typeBinding, "ZERO");
                } else if (literalValue.matches("0+1")) {
                    replaceWithQualifiedName(node, typeBinding, "ONE");
                } else if (literalValue.matches("0+10")) {
                    replaceWithQualifiedName(node, typeBinding, "TEN");
                } else if (literalValue.matches("\\d+")) {
                    this.ctx.getRefactorings().replace(node,
                            getValueOf(typeBinding.getName(), literalValue));
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceWithQualifiedName(ASTNode node, ITypeBinding typeBinding, String field) {
        this.ctx.getRefactorings().replace(node,
                this.ctx.getASTBuilder().name(typeBinding.getName(), field));
    }

    private ASTNode getValueOf(String name, String numberLiteral) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.invoke(name, "valueOf", b.number(numberLiteral));
    }

    private StringLiteral getStringLiteral(String numberLiteral) {
        return this.ctx.getASTBuilder().string(numberLiteral);
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return VISIT_SUBTREE;
        }
        if (getJavaMinorVersion() >= 5
                && (isMethod(node, "java.math.BigDecimal", "valueOf", "long")
                    || isMethod(node, "java.math.BigDecimal", "valueOf", "double"))) {
            final ITypeBinding typeBinding = node.getExpression().resolveTypeBinding();
            final Expression arg0 = arg0(node);
            if (arg0 instanceof NumberLiteral) {
                final NumberLiteral nb = (NumberLiteral) arg0;
                if (nb.getToken().contains(".")) {
                    this.ctx.getRefactorings().replace(node,
                            getClassInstanceCreatorNode(
                                    (Name) node.getExpression(),
                                    nb.getToken()));
                } else if (ZERO_LONG_LITERAL_RE.matcher(nb.getToken()).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "ZERO");
                } else if (ONE_LONG_LITERAL_RE.matcher(nb.getToken()).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "ONE");
                } else if (TEN_LONG_LITERAL_RE.matcher(nb.getToken()).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "TEN");
                } else {
                    return VISIT_SUBTREE;
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (isMethod(node, "java.math.BigDecimal", "equals", "java.lang.Object")) {
            final Expression arg0 = arg0(node);
            if (hasType(arg0, "java.math.BigDecimal")) {
                if (isInStringAppend(node.getParent())) {
                    this.ctx.getRefactorings().replace(node,
                            parenthesize(getCompareToNode(node)));
                } else {
                    this.ctx.getRefactorings().replace(node, getCompareToNode(node));
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private ParenthesizedExpression parenthesize(Expression compareToNode) {
        return this.ctx.getASTBuilder().parenthesize(compareToNode);
    }

    private boolean isInStringAppend(ASTNode node) {
        if (node instanceof InfixExpression) {
            final InfixExpression expr = (InfixExpression) node;
            if (hasOperator(expr, PLUS)
                    || hasType(expr.getLeftOperand(), "java.lang.String")
                    || hasType(expr.getRightOperand(), "java.lang.String")) {
                return true;
            }
        }
        return false;
    }

    private ASTNode getClassInstanceCreatorNode(Name className, String numberLiteral) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.new0(className.getFullyQualifiedName(), b.string(numberLiteral));
    }

    private InfixExpression getCompareToNode(MethodInvocation node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final MethodInvocation mi = b.invoke(
                b.copy(node.getExpression()), "compareTo", b.copy(arg0(node)));

        return b.infixExpr(mi, Operator.EQUALS, b.int0(0));
    }
}
