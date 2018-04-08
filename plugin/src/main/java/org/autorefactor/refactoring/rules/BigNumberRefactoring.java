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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arg0;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.JavaConstants.ONE_LONG_LITERAL_RE;
import static org.autorefactor.refactoring.JavaConstants.TEN_LONG_LITERAL_RE;
import static org.autorefactor.refactoring.JavaConstants.ZERO_LONG_LITERAL_RE;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.PLUS;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.StringLiteral;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class BigNumberRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Big number";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Refactors to a proper use of BigDecimals and BigIntegers:\n"
            + "- create BigDecimals or BigIntegers from Strings rather than floating point values,\n"
            + "- create BigDecimals or BigIntegers from integers rather than String representing integers,\n"
            + "- use BigDecimal or BigInteger constants,\n"
            + "- replace calls to equals() with calls to compareTo().";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility.";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        final ITypeBinding typeBinding = node.getType().resolveBinding();
        if (hasType(typeBinding, "java.math.BigDecimal", "java.math.BigInteger")
                && arguments(node).size() == 1) {
            final Expression arg0 = arguments(node).get(0);
            if (arg0 instanceof NumberLiteral && hasType(typeBinding, "java.math.BigDecimal")) {
                final String token = ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", "");
                if (token.contains(".")) {
                    // Only instantiation from double, not from integer
                    ctx.getRefactorings().replace(arg0,
                            getStringLiteral(token));
                    return DO_NOT_VISIT_SUBTREE;
                } else if (getJavaMinorVersion() < 5) {
                    return VISIT_SUBTREE;
                } else if (ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
                    return replaceWithQualifiedName(node, typeBinding, "ZERO");
                } else if (ONE_LONG_LITERAL_RE.matcher(token).matches()) {
                    return replaceWithQualifiedName(node, typeBinding, "ONE");
                } else if (TEN_LONG_LITERAL_RE.matcher(token).matches()) {
                    return replaceWithQualifiedName(node, typeBinding, "TEN");
                } else {
                    ctx.getRefactorings().replace(node,
                            getValueOf(typeBinding.getName(), token));
                    return DO_NOT_VISIT_SUBTREE;
                }
            } else if (arg0 instanceof StringLiteral) {
                if (getJavaMinorVersion() < 5) {
                    return VISIT_SUBTREE;
                }
                final String literalValue = ((StringLiteral) arg0).getLiteralValue().replaceFirst("[lLfFdD]$", "");
                if (literalValue.matches("0+")) {
                    return replaceWithQualifiedName(node, typeBinding, "ZERO");
                } else if (literalValue.matches("0+1")) {
                    return replaceWithQualifiedName(node, typeBinding, "ONE");
                } else if (literalValue.matches("0+10")) {
                    return replaceWithQualifiedName(node, typeBinding, "TEN");
                } else if (literalValue.matches("\\d+")) {
                    this.ctx.getRefactorings().replace(node,
                            getValueOf(typeBinding.getName(), literalValue));
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithQualifiedName(ASTNode node, ITypeBinding typeBinding, String field) {
        this.ctx.getRefactorings().replace(node,
                this.ctx.getASTBuilder().name(typeBinding.getName(), field));
        return DO_NOT_VISIT_SUBTREE;
    }

    private ASTNode getValueOf(String name, String numberLiteral) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.invoke(name, "valueOf", b.number(numberLiteral));
    }

    private StringLiteral getStringLiteral(String numberLiteral) {
        return this.ctx.getASTBuilder().string(numberLiteral);
    }

    @Override
    public boolean visit(PrefixExpression node) {
        final MethodInvocation mi = as(node.getOperand(), MethodInvocation.class);
        if (NOT.equals(node.getOperator()) && mi != null) {
            return maybeReplaceEquals(false, node, mi);
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return VISIT_SUBTREE;
        }
        if (getJavaMinorVersion() >= 5
                && (isMethod(node, "java.math.BigInteger", "valueOf", "long")
                        || isMethod(node, "java.math.BigDecimal", "valueOf", "long")
                        || isMethod(node, "java.math.BigDecimal", "valueOf", "double"))) {
            final ITypeBinding typeBinding = node.getExpression().resolveTypeBinding();
            final Expression arg0 = arg0(node);
            if (arg0 instanceof NumberLiteral) {
                final String token = ((NumberLiteral) arg0).getToken().replaceFirst("[lLfFdD]$", "");
                if (token.contains(".") && hasType(typeBinding, "java.math.BigDecimal")) {
                    this.ctx.getRefactorings().replace(node,
                            getClassInstanceCreatorNode(
                                    (Name) node.getExpression(),
                                    token));
                } else if (ZERO_LONG_LITERAL_RE.matcher(token).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "ZERO");
                } else if (ONE_LONG_LITERAL_RE.matcher(token).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "ONE");
                } else if (TEN_LONG_LITERAL_RE.matcher(token).matches()) {
                    replaceWithQualifiedName(node, typeBinding, "TEN");
                } else {
                    return VISIT_SUBTREE;
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (!(node.getParent() instanceof PrefixExpression)
                || !hasOperator((PrefixExpression) node.getParent(), NOT)) {
            return maybeReplaceEquals(true, node, node);
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeReplaceEquals(final boolean isPositive, final Expression node, final MethodInvocation mi) {
        if (isMethod(mi, "java.math.BigDecimal", "equals", "java.lang.Object")
                || isMethod(mi, "java.math.BigInteger", "equals", "java.lang.Object")) {
            final Expression arg0 = arg0(mi);
            if (hasType(arg0, "java.math.BigDecimal", "java.math.BigInteger")) {
                if (isInStringAppend(mi.getParent())) {
                    this.ctx.getRefactorings().replace(node,
                            parenthesize(getCompareToNode(isPositive, mi)));
                } else {
                    this.ctx.getRefactorings().replace(node, getCompareToNode(isPositive, mi));
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private ParenthesizedExpression parenthesize(final Expression compareToNode) {
        return this.ctx.getASTBuilder().parenthesize(compareToNode);
    }

    private boolean isInStringAppend(final ASTNode node) {
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

    private ASTNode getClassInstanceCreatorNode(final Name className, final String numberLiteral) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.new0(className.getFullyQualifiedName(), b.string(numberLiteral));
    }

    private InfixExpression getCompareToNode(final boolean isPositive, final MethodInvocation node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        final MethodInvocation mi = b.invoke(
                b.copy(node.getExpression()), "compareTo", b.copy(arg0(node)));

        return b.infixExpr(mi, isPositive ? EQUALS : NOT_EQUALS, b.int0(0));
    }
}
