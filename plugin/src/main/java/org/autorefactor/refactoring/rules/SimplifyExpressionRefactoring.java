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

import java.util.Arrays;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.WhileStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.util.Utils.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class SimplifyExpressionRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Simplifies Java expressions:\n"
            + "- remove redundant null checks or useless right-hand side or left-hand sie operands,\n"
            + "- fix Comparable.compareTo() usage,\n"
            + "- remove useless parentheses,\n"
            + "- directly check boolean values instead of comparing tham with true/false.";
    }

    @Override
    public String getName() {
        return "Simplify expressions";
    }

    /** A mapping of child operation to parent operation that mandates using parentheses. */
    private static final List<Pair<Operator, Operator>> SHOULD_HAVE_PARENTHESES = Arrays.asList(
            Pair.of(CONDITIONAL_AND, CONDITIONAL_OR),
            Pair.of(AND, XOR),
            Pair.of(AND, OR),
            Pair.of(XOR, OR),
            Pair.of(LEFT_SHIFT, OR),
            Pair.of(LEFT_SHIFT, AND),
            Pair.of(RIGHT_SHIFT_SIGNED, OR),
            Pair.of(RIGHT_SHIFT_SIGNED, AND),
            Pair.of(RIGHT_SHIFT_UNSIGNED, OR),
            Pair.of(RIGHT_SHIFT_UNSIGNED, AND));

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    // TODO JNR remove avoidable boxing / unboxing

    // TODO Very few parenthesized expressions are actually needed. They are:
    // 1) inside InfixExpressions with logical operators (&&, ||, etc.)
    // Sometimes needed to explicit code, some like it like that too
    // 2) Inside String concatenations if they hold an InfixExpression that does
    // not resolve to String (what about PrefixExpression and
    // PostFixExpression?)
    // 3) Around CastExpression
    // Any others?

    // TODO JNR !true => false and !false => true

    // TODO JNR String s = "some " + " string " + "" + ( "fhj" + "prout" );

    @Override
    public boolean visit(ParenthesizedExpression node) {
        final Expression innerExpr = getExpressionWithoutParentheses(node);
        if (innerExpr != node) {
            return replaceByCopy(node, innerExpr);
        }
        return VISIT_SUBTREE;
    }

    private Expression getExpressionWithoutParentheses(ParenthesizedExpression node) {
        final ASTNode parent = node.getParent();
        final Expression innerExpr = node.getExpression();
        if (innerExpr instanceof ParenthesizedExpression) {
            return getExpressionWithoutParentheses((ParenthesizedExpression) innerExpr);
        }
        if (parent instanceof InfixExpression) {
            final InfixExpression parentInfixExpr = (InfixExpression) parent;
            if (innerExpr instanceof InfixExpression) {
                final Operator innerOp = ((InfixExpression) innerExpr).getOperator();
                if (innerOp == parentInfixExpr.getOperator()
                        && OperatorEnum.isAssociative(innerOp)
                        // Leave String concatenations with mixed type
                        // to other if statements in this method.
                        && equalNotNull(innerExpr.resolveTypeBinding(), parentInfixExpr.resolveTypeBinding())) {
                    return innerExpr;
                }
            }
        }
        if (isHardToRead(innerExpr, parent)) {
            return node;
        }
        if (isUselessParenthesesInStatement(parent, node)) {
            return innerExpr;
        }
        final int compareTo = OperatorEnum.compareTo(innerExpr, parent);
        if (compareTo < 0) {
            return node;
        } else if (compareTo > 0) {
            return innerExpr;
        }
        if (
                // TODO JNR can we revert the condition in the InfixExpression?
                // parentheses are sometimes needed to explicit code,
                // some like it like that
                innerExpr instanceof InfixExpression
                // TODO JNR add additional code to check if the cast is really required
                // or if it can be removed.
                || innerExpr instanceof CastExpression) {
            return node;
        }
        return innerExpr;
    }

    /**
     * Returns whether the supplied expression is complex enough to read.
     *
     * @param innerExpr
     *          the inner expression to test for ease of read
     * @param parent
     *          the parent node to test for ease of read
     * @return true if the expressions is hard to read, false otherwise
     */
    private boolean isHardToRead(final Expression innerExpr, final ASTNode parent) {
        if (parent instanceof InfixExpression) {
            if (innerExpr instanceof InfixExpression) {
                final InfixExpression innerIe = (InfixExpression) innerExpr;
                final Operator innerOp = innerIe.getOperator();
                final Operator parentOp = ((InfixExpression) parent).getOperator();
                if (Operator.EQUALS.equals(parentOp) || shouldHaveParentheses(innerOp, parentOp)) {
                    return true;
                }
                return is(innerIe.getLeftOperand(), Assignment.class)
                        || is(innerIe.getRightOperand(), Assignment.class);
            }
        } else if (parent instanceof ConditionalExpression) {
            return innerExpr instanceof ConditionalExpression
                || innerExpr instanceof Assignment
                || innerExpr instanceof InstanceofExpression
                || innerExpr instanceof InfixExpression;
        }
        return false;
    }

    private boolean isUselessParenthesesInStatement(ASTNode parent, ParenthesizedExpression node) {
        switch (parent.getNodeType()) {
        case ASTNode.ASSIGNMENT:
            final Assignment a = (Assignment) parent;
            return node.equals(a.getRightHandSide());
        case ASTNode.METHOD_INVOCATION:
            final MethodInvocation mi = (MethodInvocation) parent;
            return arguments(mi).contains(node);
        case ASTNode.IF_STATEMENT:
            final IfStatement is = (IfStatement) parent;
            return node.equals(is.getExpression());
        case ASTNode.WHILE_STATEMENT:
            final WhileStatement ws = (WhileStatement) parent;
            return node.equals(ws.getExpression());
        case ASTNode.DO_STATEMENT:
            final DoStatement ds = (DoStatement) parent;
            return node.equals(ds.getExpression());
        case ASTNode.RETURN_STATEMENT:
            final ReturnStatement rs = (ReturnStatement) parent;
            return node.equals(rs.getExpression());
        case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
            final VariableDeclarationFragment vdf = (VariableDeclarationFragment) parent;
            return node.equals(vdf.getInitializer());
        default:
            return false;
        }
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            // TODO JNR handle same class calls and sub classes
            return VISIT_SUBTREE;
        }
        if (isMethod(node, "java.lang.Comparable", "compareTo", "java.lang.Object")) {
            return replaceInfixExpressionIfNeeded(node.getParent());
        } else if (isMethod(node, "java.lang.Comparator", "compare", "java.lang.Object", "java.lang.Object")) {
            return replaceInfixExpressionIfNeeded(node.getParent());
        } else if (getJavaMinorVersion() >= 2
                && isMethod(node, "java.lang.String", "compareToIgnoreCase", "java.lang.String")) {
            return replaceInfixExpressionIfNeeded(node.getParent());
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInfixExpressionIfNeeded(ASTNode expr) {
        if (expr instanceof ParenthesizedExpression) {
            return replaceInfixExpressionIfNeeded(expr.getParent());
        } else if (expr instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) expr;
            checkNoExtendedOperands(ie);
            final Object value = ie.getRightOperand().resolveConstantExpressionValue();
            if (value instanceof Number) {
                final Number nb = (Integer) value;
                if (nb.doubleValue() == 0) {
                    return VISIT_SUBTREE;
                }
                if (hasOperator(ie, EQUALS)) {
                    if (nb.doubleValue() < 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, LESS);
                    } else if (nb.doubleValue() > 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, GREATER);
                    }
                } else if (hasOperator(ie, NOT_EQUALS)) {
                    if (nb.doubleValue() < 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, GREATER_EQUALS);
                    } else if (nb.doubleValue() > 0) {
                        return replaceWithCorrectCheckOnCompareTo(ie, LESS_EQUALS);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithCorrectCheckOnCompareTo(final InfixExpression ie, final Operator operator) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(ie,
            b.infixExpr(
                b.copy(ie.getLeftOperand()),
                operator,
                b.number("0")),
            null);
        return DO_NOT_VISIT_SUBTREE;
    }

    @Override
    public boolean visit(InfixExpression node) {
        final Expression lhs = node.getLeftOperand();
        final Expression rhs = node.getRightOperand();
        final Object lhsConstantValue = lhs.resolveConstantExpressionValue();
        if (hasOperator(node, CONDITIONAL_OR)) {
            if (Boolean.TRUE.equals(lhsConstantValue)) {
                return replaceByCopy(node, lhs);
            } else if (Boolean.FALSE.equals(lhsConstantValue)) {
                checkNoExtendedOperands(node);
                return replaceByCopy(node, rhs);
            }
        } else if (hasOperator(node, CONDITIONAL_AND)) {
            if (Boolean.TRUE.equals(lhsConstantValue)) {
                checkNoExtendedOperands(node);
                return replaceByCopy(node, rhs);
            } else if (Boolean.FALSE.equals(lhsConstantValue)) {
                return replaceByCopy(node, lhs);
            } else {
                final Expression nullCheckedExpressionLHS = getNullCheckedExpression(lhs);
                final Expression nullCheckedExpressionRHS = getNullCheckedExpression(rhs);
                if (nullCheckedExpressionLHS != null) {
                    if (isNullCheckRedundant(rhs, nullCheckedExpressionLHS)) {
                        checkNoExtendedOperands(node);
                        return replaceByCopy(node, rhs);
                    }
                } else if (isNullCheckRedundant(lhs, nullCheckedExpressionRHS)) {
                    return replaceByCopy(node, lhs);
                }
            }
        } else if (hasOperator(node, EQUALS)) {
            boolean result = VISIT_SUBTREE;
            final Boolean blo = getBooleanLiteral(lhs);
            final Boolean bro = getBooleanLiteral(rhs);
            if (blo != null) {
                result = replace(node, !blo.booleanValue(), rhs);
            } else if (bro != null) {
                result = replace(node, !bro.booleanValue(), lhs);
            }
            if (result == DO_NOT_VISIT_SUBTREE) {
                return DO_NOT_VISIT_SUBTREE;
            }
        } else if (hasOperator(node, NOT_EQUALS)) {
            boolean continueVisit = VISIT_SUBTREE;
            final Boolean blo = getBooleanLiteral(lhs);
            final Boolean bro = getBooleanLiteral(rhs);
            if (blo != null) {
                continueVisit = replace(node, blo.booleanValue(), rhs);
            } else if (bro != null) {
                continueVisit = replace(node, bro.booleanValue(), lhs);
            }
            if (!continueVisit) {
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        if (shouldHaveParentheses(node)) {
            addParentheses(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean shouldHaveParentheses(InfixExpression node) {
        final Operator childOp = node.getOperator();
        if (node.getParent() instanceof InfixExpression) {
            final InfixExpression ie = (InfixExpression) node.getParent();
            return shouldHaveParentheses(childOp, ie.getOperator());
        }
        return false;
    }

    private boolean shouldHaveParentheses(Operator actualChildOp, Operator actualParentOp) {
        for (Pair<Operator, Operator> pair : SHOULD_HAVE_PARENTHESES) {
            final Operator childOp = pair.getFirst();
            final Operator parentOp = pair.getSecond();
            if (childOp.equals(actualChildOp) && parentOp.equals(actualParentOp)) {
                return true;
            }
        }
        return false;
    }

    private void addParentheses(Expression e) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(e, b.parenthesize(b.copy(e)), null);
    }

    /**
     * Extended operands are used for deeply nested expressions, mostly string concatenation expressions.
     * <p>
     * This will be implemented only if somebody comes up with code where the runtime exception is thrown.
     * </p>
     */
    private boolean checkNoExtendedOperands(InfixExpression node) {
        if (!hasType(node, "java.lang.String") && node.hasExtendedOperands()) {
            throw new NotImplementedException(node, "for extended operands");
        }
        return true;
    }

    private boolean isPrimitiveBool(Expression expr) {
        ITypeBinding typeBinding = expr.resolveTypeBinding();
        return typeBinding != null
                && typeBinding.isPrimitive()
                && "boolean".equals(typeBinding.getQualifiedName());
    }

    private boolean replace(InfixExpression node, boolean negate, Expression exprToCopy) {
        checkNoExtendedOperands(node);
        if (!isPrimitiveBool(node.getLeftOperand())
                && !isPrimitiveBool(node.getRightOperand())) {
            return VISIT_SUBTREE;
        }
        // Either:
        // - Two boolean primitives: no possible NPE
        // - One boolean primitive and one Boolean object, this code already run
        // the risk of an NPE, so we can replace the infix expression without
        // fearing we would introduce a previously non existing NPE.
        final ASTBuilder b = ctx.getASTBuilder();
        Expression operand;
        if (negate) {
            operand = b.negate(exprToCopy);
        } else {
            operand = b.copy(exprToCopy);
        }
        ctx.getRefactorings().replace(node, operand, null);
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean replaceByCopy(ASTNode node, Expression expr) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(node, b.copy(expr), null);
        return DO_NOT_VISIT_SUBTREE;
    }

    /**
     * The previous null check is redundant if:
     * <ul>
     * <li>the null checked expression is reused in an instanceof expression</li>
     * <li>the null checked expression is reused in an expression checking for
     * object equality against an expression that resolves to a non null
     * constant</li>
     * </ul>
     */
    private boolean isNullCheckRedundant(Expression e, Expression nullCheckedExpression) {
        if (nullCheckedExpression == null) {
            return false;
        } else if (e instanceof InstanceofExpression) {
            final Expression expr = ((InstanceofExpression) e).getLeftOperand();
            return expr.subtreeMatch(new ASTMatcher(), nullCheckedExpression);
        } else if (e instanceof MethodInvocation) {
            final MethodInvocation expr = (MethodInvocation) e;
            if (expr.getExpression() != null
                    && expr.getExpression().resolveConstantExpressionValue() != null
                    && arguments(expr).size() == 1
                    && arguments(expr).get(0).subtreeMatch(
                            new ASTMatcher(), nullCheckedExpression)) {
                // Did we invoke java.lang.Object.equals() or java.lang.String.equalsIgnoreCase()?
                return isMethod(expr, "java.lang.Object", "equals", "java.lang.Object")
                        || isMethod(expr, "java.lang.String", "equalsIgnoreCase", "java.lang.String");
            }
        }
        return false;
    }

    private Expression getNullCheckedExpression(Expression e) {
        if (e instanceof InfixExpression) {
            final InfixExpression expr = (InfixExpression) e;
            if (hasOperator(expr, NOT_EQUALS) && checkNoExtendedOperands(expr)) {
                if (isNullLiteral(expr.getLeftOperand())) {
                    return expr.getRightOperand();
                } else if (isNullLiteral(expr.getRightOperand())) {
                    return expr.getLeftOperand();
                }
            }
        }
        return null;
    }
}
