/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.allOperands;
import static org.autorefactor.refactoring.ASTHelper.arguments;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.checkNoExtendedOperands;
import static org.autorefactor.refactoring.ASTHelper.getBooleanLiteral;
import static org.autorefactor.refactoring.ASTHelper.getNullCheckedExpression;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.is;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.isPrimitive;
import static org.autorefactor.util.Utils.equalNotNull;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LEFT_SHIFT;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.RIGHT_SHIFT_SIGNED;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.XOR;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.DECREMENT;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.INCREMENT;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.MINUS;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.PLUS;

import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ASTSemanticMatcher;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class SimplifyExpressionRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Simplify expressions";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Simplifies Java expressions:\n"
            + "- remove redundant null checks or useless right-hand side or left-hand side operands,\n"
            + "- remove useless parentheses,\n"
            + "- directly check boolean values instead of comparing them with true/false,\n"
            + "- reduce double negation in boolean expression.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility and the performance.";
    }

    /** A mapping of child operation to parent operation that mandates using parentheses. */
    private static final List<Pair<Operator, Operator>> SHOULD_HAVE_PARENTHESES =
            Arrays.<Pair<Operator, Operator>>asList(
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

    // TODO Very few parenthesized expressions are actually needed. They are:
    // 1) inside InfixExpressions with logical operators (&&, ||, etc.)
    // Sometimes needed to explicit code, some like it like that too
    // 2) Inside String concatenations if they hold an InfixExpression that does
    // not resolve to String (what about PrefixExpression and
    // PostFixExpression?)
    // 3) Around CastExpression
    // Any others?

    // TODO JNR String s = "some " + " string " + "" + ( "fhj" + "prout" );

    @Override
    public boolean visit(ParenthesizedExpression node) {
        final Expression innerExpr = getExpressionWithoutParentheses(node);
        if (innerExpr != node) {
            return replaceBy(node, innerExpr);
        }
        return VISIT_SUBTREE;
    }

    private Expression getExpressionWithoutParentheses(ParenthesizedExpression node) {
        final ASTNode parent = node.getParent();
        final Expression innerExpr = node.getExpression();
        if (innerExpr instanceof ParenthesizedExpression) {
            return innerExpr;
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
        // Infix, prefix or postfix without parenthesis is not readable
        if ((parent instanceof InfixExpression
                && (InfixExpression.Operator.PLUS.equals(((InfixExpression) parent).getOperator())
                        || InfixExpression.Operator.MINUS.equals(((InfixExpression) parent).getOperator())))
                || (parent instanceof PrefixExpression
                        && (PLUS.equals(((PrefixExpression) parent).getOperator())
                                || MINUS.equals(((PrefixExpression) parent).getOperator())))) {
            if (innerExpr instanceof PrefixExpression
                    && (DECREMENT.equals(((PrefixExpression) innerExpr).getOperator())
                            || INCREMENT.equals(((PrefixExpression) innerExpr).getOperator())
                            || PLUS.equals(((PrefixExpression) innerExpr).getOperator())
                            || MINUS.equals(((PrefixExpression) innerExpr).getOperator()))) {
                return node;
            }
            if (innerExpr instanceof PostfixExpression
                    && (PostfixExpression.Operator.DECREMENT.equals(((PostfixExpression) innerExpr).getOperator())
                            || PostfixExpression.Operator.INCREMENT.equals(((PostfixExpression) innerExpr)
                                    .getOperator()))) {
                return node;
            }
        }
        if (isInnerExprHardToRead(innerExpr, parent)) {
            // FIXME This is not really that hard to read is it?
            // return (bla != null) ? bla.getSomething() : null;
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
                || innerExpr instanceof CastExpression
                // infix and prefix or postfix without parenthesis is not readable
                || ((parent instanceof InfixExpression
                        || parent instanceof PrefixExpression
                                || parent instanceof PostfixExpression)
                        && (innerExpr instanceof PrefixExpression
                                || innerExpr instanceof PostfixExpression))) {
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
    private boolean isInnerExprHardToRead(final Expression innerExpr, final ASTNode parent) {
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
            return arguments(mi).contains(node) || canRemoveParenthesesAroundExpression(mi, node);
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

    private boolean canRemoveParenthesesAroundExpression(MethodInvocation mi, ParenthesizedExpression node) {
        if (node.equals(mi.getExpression())) {
            switch (node.getExpression().getNodeType()) {
            case ASTNode.ASSIGNMENT:
            case ASTNode.CAST_EXPRESSION:
            case ASTNode.CONDITIONAL_EXPRESSION:
            case ASTNode.INFIX_EXPRESSION:
                return false;
            default:
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean visit(InfixExpression node) {
        final Expression lhs = node.getLeftOperand();
        final Expression rhs = node.getRightOperand();
        if (hasOperator(node, CONDITIONAL_OR)) {
            final List<Expression> remainingOperands = removeUselessOperands(node, true, false);
            if (!remainingOperands.equals(allOperands(node))) {
                return replaceWithNewInfixExpr(node, remainingOperands);
            }
        } else if (hasOperator(node, CONDITIONAL_AND)) {
            final List<Expression> remainingOperands = removeUselessOperands(node, false, true);
            if (!remainingOperands.equals(allOperands(node))) {
                return replaceWithNewInfixExpr(node, remainingOperands);
            } else {
                // FIXME this should actually check anywhere in the infix expression,
                // not only for left and right operands,
                // said otherwise: handle extended operands
                final Expression nullCheckedExpressionLHS = getNullCheckedExpression(lhs);
                final Expression nullCheckedExpressionRHS = getNullCheckedExpression(rhs);
                if (nullCheckedExpressionLHS != null) {
                    if (isNullCheckRedundant(rhs, nullCheckedExpressionLHS)) {
                        checkNoExtendedOperands(node);
                        return replaceBy(node, rhs);
                    }
                } else if (isNullCheckRedundant(lhs, nullCheckedExpressionRHS)) {
                    return replaceBy(node, lhs);
                }
            }
        } else if (hasOperator(node, EQUALS, NOT_EQUALS, XOR)
                && !node.hasExtendedOperands()) {
            if (maybeReduceBooleanExpression(node, lhs, rhs) == DO_NOT_VISIT_SUBTREE) {
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        if (shouldHaveParentheses(node)) {
            addParentheses(node);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeReduceBooleanExpression(final InfixExpression node, final Expression leftExpr,
            final Expression rightExpr) {
        final Boolean leftBoolean = getBooleanLiteral(leftExpr);
        final Boolean rightBoolean = getBooleanLiteral(rightExpr);

        if (leftBoolean != null) {
            return replace(node, leftBoolean.booleanValue(), rightExpr);
        } else if (rightBoolean != null) {
            return replace(node, rightBoolean.booleanValue(), leftExpr);
        }

        Expression leftOppositeExpr = null;
        final PrefixExpression leftPrefix = as(leftExpr, PrefixExpression.class);
        if (leftPrefix != null && hasOperator(leftPrefix, NOT)) {
            leftOppositeExpr = leftPrefix.getOperand();
        }

        Expression rightOppositeExpr = null;
        final PrefixExpression rightPrefix = as(rightExpr, PrefixExpression.class);
        if (rightPrefix != null && hasOperator(rightPrefix, NOT)) {
            rightOppositeExpr = rightPrefix.getOperand();
        }

        final ASTBuilder b = this.ctx.getASTBuilder();
        final Refactorings r = this.ctx.getRefactorings();
        if (leftOppositeExpr != null && rightOppositeExpr != null) {
            r.replace(node, b.infixExpr(b.copy(leftOppositeExpr), getAppropriateOperator(node),
                    b.copy(rightOppositeExpr)));
            return DO_NOT_VISIT_SUBTREE;
        } else if (leftOppositeExpr != null) {
            final Operator reverseOp = getReverseOperator(node);
            r.replace(node, b.infixExpr(b.copy(leftOppositeExpr), reverseOp, b.copy(rightExpr)));
            return DO_NOT_VISIT_SUBTREE;
        } else if (rightOppositeExpr != null) {
            final Operator reverseOp = getReverseOperator(node);
            r.replace(node, b.infixExpr(b.copy(leftExpr), reverseOp, b.copy(rightOppositeExpr)));
            return DO_NOT_VISIT_SUBTREE;
        }

        return VISIT_SUBTREE;
    }

    private Operator getAppropriateOperator(final InfixExpression node) {
        if (NOT_EQUALS.equals(node.getOperator())) {
            return XOR;
        } else {
            return node.getOperator();
        }
    }

    private Operator getReverseOperator(final InfixExpression node) {
        if (EQUALS.equals(node.getOperator())) {
            return XOR;
        } else {
            return EQUALS;
        }
    }

    private boolean replace(final InfixExpression node, final boolean isTrue, final Expression exprToCopy) {
        checkNoExtendedOperands(node);
        if (!isPrimitive(node.getLeftOperand(), "boolean")
                && !isPrimitive(node.getRightOperand(), "boolean")) {
            return VISIT_SUBTREE;
        }
        // Either:
        // - Two boolean primitives: no possible NPE
        // - One boolean primitive and one Boolean object, this code already run
        // the risk of an NPE, so we can replace the infix expression without
        // fearing we would introduce a previously non existing NPE.
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Expression operand;
        if (isTrue == hasOperator(node, EQUALS)) {
            operand = b.copy(exprToCopy);
        } else {
            operand = b.negate(exprToCopy);
        }
        this.ctx.getRefactorings().replace(node, operand);
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean replaceWithNewInfixExpr(InfixExpression node,
            final List<Expression> remainingOperands) {
        if (remainingOperands.size() == 1) {
            replaceBy(node, remainingOperands.get(0));
        } else {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node,
                    b.infixExpr(node.getOperator(), b.move(remainingOperands)));
        }
        return DO_NOT_VISIT_SUBTREE;
    }

    private List<Expression> removeUselessOperands(InfixExpression node,
            Boolean shortCircuitValue, Boolean noOpValue) {
        final List<Expression> allOperands = allOperands(node);
        for (ListIterator<Expression> it = allOperands.listIterator(); it.hasNext();) {
            final Expression operand = it.next();
            final Object value = operand.resolveConstantExpressionValue();
            if (shortCircuitValue.equals(value)) {
                removeRemaining(it);
                break;
            } else if (noOpValue.equals(value)) {
                it.remove();
            }
        }
        return allOperands;
    }

    private void removeRemaining(ListIterator<Expression> it) {
        while (it.hasNext()) {
            it.next();
            it.remove();
        }
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
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(e, b.parenthesize(b.copy(e)));
    }

    private boolean replaceBy(ASTNode node, Expression expr) {
        final ASTBuilder b = ctx.getASTBuilder();
        ctx.getRefactorings().replace(node, b.move(expr));
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
            return expr.subtreeMatch(new ASTSemanticMatcher(), nullCheckedExpression);
        } else if (e instanceof MethodInvocation) {
            final MethodInvocation expr = (MethodInvocation) e;
            if (expr.getExpression() != null
                    && expr.getExpression().resolveConstantExpressionValue() != null
                    && arguments(expr).size() == 1
                    && arguments(expr).get(0).subtreeMatch(
                            new ASTSemanticMatcher(), nullCheckedExpression)) {
                // Did we invoke java.lang.Object.equals() or java.lang.String.equalsIgnoreCase()?
                return isMethod(expr, "java.lang.Object", "equals", "java.lang.Object")
                        || isMethod(expr, "java.lang.String", "equalsIgnoreCase", "java.lang.String");
            }
        }
        return false;
    }
}
