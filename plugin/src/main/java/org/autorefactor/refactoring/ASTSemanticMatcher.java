/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.refactoring;

import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.haveSameType;
import static org.autorefactor.refactoring.ASTHelper.isPassive;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.autorefactor.refactoring.rules.OperatorEnum;
import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.BreakStatement;
import org.eclipse.jdt.core.dom.ConditionalExpression;
import org.eclipse.jdt.core.dom.ConstructorInvocation;
import org.eclipse.jdt.core.dom.ContinueStatement;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.SynchronizedStatement;
import org.eclipse.jdt.core.dom.ThrowStatement;
import org.eclipse.jdt.core.dom.TryStatement;
import org.eclipse.jdt.core.dom.TypeDeclarationStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** Matches two pieces of code on semantic (not on syntax). */
public class ASTSemanticMatcher extends ASTMatcher {
    private static final Map<PrefixExpression.Operator, Operator> PREFIX_TO_INFIX_OPERATOR =
            new HashMap<PrefixExpression.Operator, Operator>() {
            private static final long serialVersionUID = -8949107654517355855L;

            {
                put(PrefixExpression.Operator.INCREMENT, Operator.PLUS);
                put(PrefixExpression.Operator.DECREMENT, Operator.MINUS);
            }
        };

    private static final Map<PrefixExpression.Operator, Assignment.Operator> PREFIX_TO_ASSIGN_OPERATOR =
            new HashMap<PrefixExpression.Operator, Assignment.Operator>() {
            private static final long serialVersionUID = -8949107654517355856L;

            {
                put(PrefixExpression.Operator.INCREMENT, Assignment.Operator.PLUS_ASSIGN);
                put(PrefixExpression.Operator.DECREMENT, Assignment.Operator.MINUS_ASSIGN);
            }
        };

    private static final Map<PostfixExpression.Operator, Operator> POSTFIX_TO_INFIX_OPERATOR =
            new HashMap<PostfixExpression.Operator, Operator>() {
            private static final long serialVersionUID = -8949107654517355857L;

            {
                put(PostfixExpression.Operator.INCREMENT, Operator.PLUS);
                put(PostfixExpression.Operator.DECREMENT, Operator.MINUS);
            }
        };

    private static final Map<PostfixExpression.Operator, Assignment.Operator> POSTFIX_TO_ASSIGN_OPERATOR =
            new HashMap<PostfixExpression.Operator, Assignment.Operator>() {
            private static final long serialVersionUID = -8949107654517355858L;

            {
                put(PostfixExpression.Operator.INCREMENT, Assignment.Operator.PLUS_ASSIGN);
                put(PostfixExpression.Operator.DECREMENT, Assignment.Operator.MINUS_ASSIGN);
            }
        };

    private static final Map<PrefixExpression.Operator, PostfixExpression.Operator> PREFIX_TO_POSTFIX_OPERATOR =
            new HashMap<PrefixExpression.Operator, PostfixExpression.Operator>() {
            private static final long serialVersionUID = -8949107654517355859L;

            {
                put(PrefixExpression.Operator.INCREMENT, PostfixExpression.Operator.INCREMENT);
                put(PrefixExpression.Operator.DECREMENT, PostfixExpression.Operator.DECREMENT);
            }
        };

    private static final Map<Assignment.Operator, Operator> ASSIGN_TO_INFIX_OPERATOR =
            new HashMap<Assignment.Operator, Operator>() {
            private static final long serialVersionUID = -8949107654517355859L;

            {
                put(Assignment.Operator.PLUS_ASSIGN, Operator.PLUS);
                put(Assignment.Operator.MINUS_ASSIGN, Operator.MINUS);
                put(Assignment.Operator.TIMES_ASSIGN, Operator.TIMES);
                put(Assignment.Operator.DIVIDE_ASSIGN, Operator.DIVIDE);
                put(Assignment.Operator.BIT_AND_ASSIGN, Operator.AND);
                put(Assignment.Operator.BIT_OR_ASSIGN, Operator.OR);
                put(Assignment.Operator.BIT_XOR_ASSIGN, Operator.XOR);
                put(Assignment.Operator.REMAINDER_ASSIGN, Operator.REMAINDER);
                put(Assignment.Operator.LEFT_SHIFT_ASSIGN, Operator.LEFT_SHIFT);
                put(Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN, Operator.RIGHT_SHIFT_SIGNED);
                put(Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN,
                        Operator.RIGHT_SHIFT_UNSIGNED);
            }
        };

    private static final Map<Operator, Operator> INFIX_TO_MIRROR_OPERATOR =
            new HashMap<Operator, Operator>() {
            private static final long serialVersionUID = -8949107654517355857L;

            {
                put(Operator.EQUALS, Operator.EQUALS);
                put(Operator.NOT_EQUALS, Operator.NOT_EQUALS);
                put(Operator.CONDITIONAL_AND, Operator.CONDITIONAL_AND);
                put(Operator.CONDITIONAL_OR, Operator.CONDITIONAL_OR);
                put(Operator.AND, Operator.AND);
                put(Operator.OR, Operator.OR);
                put(Operator.XOR, Operator.XOR);
                put(Operator.GREATER, Operator.LESS);
                put(Operator.LESS, Operator.GREATER);
                put(Operator.LESS_EQUALS, Operator.GREATER_EQUALS);
                put(Operator.GREATER_EQUALS, Operator.LESS_EQUALS);
            }
        };

    @Override
    public boolean match(final InfixExpression node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof PrefixExpression) {
            PrefixExpression pe = (PrefixExpression) other;

            if (PrefixExpression.Operator.NOT.equals(pe.getOperator())) {
                return matchOpposite(node, pe.getOperand());
            }
        }

        if (other instanceof InfixExpression) {
            InfixExpression ie = (InfixExpression) other;

            if (!node.hasExtendedOperands()
                    && !ie.hasExtendedOperands()
                    && isPassive(node.getLeftOperand())
                    && isPassive(node.getRightOperand())
                    && safeSubtreeMatch(node.getLeftOperand(), ie.getRightOperand())
                    && safeSubtreeMatch(node.getRightOperand(), ie.getLeftOperand())) {
                if (node.getOperator().equals(INFIX_TO_MIRROR_OPERATOR.get(ie.getOperator()))) {
                    return true;
                } else if (Arrays.asList(InfixExpression.Operator.PLUS,
                        InfixExpression.Operator.TIMES).contains(ie.getOperator())
                        && node.getOperator().equals(ie.getOperator())
                        && hasType(node.getLeftOperand(), "short", "int", "long", "float", "double",
                                "java.lang.Short", "java.lang.Integer", "java.lang.Long", "java.lang.Float",
                                "java.lang.Double")
                        && haveSameType(node.getLeftOperand(), node.getRightOperand())) {
                    return true;
                }
            }

            if (node.getOperator().equals(ie.getOperator())
                    && Arrays.asList(
                            InfixExpression.Operator.PLUS,
                            InfixExpression.Operator.TIMES,
                            InfixExpression.Operator.AND,
                            InfixExpression.Operator.CONDITIONAL_AND,
                            InfixExpression.Operator.OR,
                            InfixExpression.Operator.CONDITIONAL_OR,
                            InfixExpression.Operator.XOR).contains(ie.getOperator())) {
                return isOperandsMatching(node, ie, true);
            }
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ParenthesizedExpression node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        return safeSubtreeMatch(node.getExpression(), other);
    }

    private Object unbracket(final Object otherObject) {
        if (otherObject instanceof ParenthesizedExpression) {
            return ((ParenthesizedExpression) otherObject).getExpression();
        }

        return otherObject;
    }

    @Override
    public boolean match(final PrefixExpression node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (!(other instanceof PrefixExpression) && PrefixExpression.Operator.NOT.equals(node.getOperator())) {
            return matchOpposite(node.getOperand(), other);
        }

        if (node.getParent() instanceof Statement) {
            if (other instanceof Assignment) {
                return match0(node, (Assignment) other);
            } else if (other instanceof PostfixExpression) {
                return match0(node, (PostfixExpression) other);
            }
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final PostfixExpression node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (node.getParent() instanceof Statement) {
            if (other instanceof Assignment) {
                return match0(node, (Assignment) other);
            } else if (other instanceof PrefixExpression) {
                return match0((PrefixExpression) other, node);
            }
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final Assignment node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof PrefixExpression && ((PrefixExpression) other).getParent() instanceof Statement) {
            return match0((PrefixExpression) other, node);
        } else if (other instanceof PostfixExpression
                && ((PostfixExpression) other).getParent() instanceof Statement) {
            return match0((PostfixExpression) other, node);
        } else if (other instanceof Assignment) {
            return matchAssignmentWithAndWithoutEqual(node, (Assignment) other)
                    || matchAssignmentWithAndWithoutEqual((Assignment) other, node)
                    || super.match(node, other);
        }

        return super.match(node, other);
    }

    private boolean matchAssignmentWithAndWithoutEqual(final Assignment node, final Assignment assignment) {
        if (Assignment.Operator.ASSIGN.equals(node.getOperator())
                && node.getRightHandSide() instanceof InfixExpression) {
            InfixExpression infixExpr = (InfixExpression) node.getRightHandSide();

            if (!infixExpr.hasExtendedOperands()
                    && Arrays.asList(Assignment.Operator.PLUS_ASSIGN,
                            Assignment.Operator.MINUS_ASSIGN,
                            Assignment.Operator.TIMES_ASSIGN,
                            Assignment.Operator.DIVIDE_ASSIGN,
                            Assignment.Operator.BIT_AND_ASSIGN,
                            Assignment.Operator.BIT_OR_ASSIGN,
                            Assignment.Operator.BIT_XOR_ASSIGN,
                            Assignment.Operator.REMAINDER_ASSIGN,
                            Assignment.Operator.LEFT_SHIFT_ASSIGN,
                            Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN,
                            Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN).contains(assignment.getOperator())
                    && ASSIGN_TO_INFIX_OPERATOR.get(assignment.getOperator()).equals(infixExpr.getOperator())) {
                return safeSubtreeMatch(node.getLeftHandSide(), assignment.getLeftHandSide())
                        && safeSubtreeMatch(infixExpr.getLeftOperand(), assignment.getLeftHandSide())
                        && safeSubtreeMatch(infixExpr.getRightOperand(), assignment.getRightHandSide());
            }
        }

        return false;
    }

    private boolean match0(final PrefixExpression prefixExpr, final PostfixExpression postfixExpr) {
        return postfixExpr.getOperator().equals(PREFIX_TO_POSTFIX_OPERATOR.get(prefixExpr.getOperator()))
                && safeSubtreeMatch(prefixExpr.getOperand(), postfixExpr.getOperand());
    }

    private boolean match0(final PrefixExpression prefixExpr, final Assignment assignment) {
        return match0(assignment, prefixExpr.getOperand(), PREFIX_TO_INFIX_OPERATOR.get(prefixExpr.getOperator()),
                PREFIX_TO_ASSIGN_OPERATOR.get(prefixExpr.getOperator()));
    }

    private boolean match0(final PostfixExpression postfixExpr, final Assignment assignment) {
        return match0(assignment, postfixExpr.getOperand(), POSTFIX_TO_INFIX_OPERATOR.get(postfixExpr.getOperator()),
                POSTFIX_TO_ASSIGN_OPERATOR.get(postfixExpr.getOperator()));
    }

    private boolean match0(final Assignment assignment, final Expression prefixOrPostfixOperand,
            final Operator infixAssociatedOperator,
            final Assignment.Operator assignmentAssociatedOperator) {
        if (Assignment.Operator.ASSIGN.equals(assignment.getOperator())
                && assignment.getRightHandSide() instanceof InfixExpression) {
            InfixExpression infixExpr = (InfixExpression) assignment.getRightHandSide();
            if (!infixExpr.hasExtendedOperands()
                    && infixAssociatedOperator.equals(infixExpr.getOperator())) {
                if (isOneLiteral(infixExpr.getRightOperand())) {
                    return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide())
                            && safeSubtreeMatch(prefixOrPostfixOperand, infixExpr.getLeftOperand());
                } else if (Operator.PLUS.equals(infixExpr.getOperator())
                        && isOneLiteral(infixExpr.getLeftOperand())) {
                    return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide())
                            && safeSubtreeMatch(prefixOrPostfixOperand, infixExpr.getRightOperand());
                }
            }
        } else if (Arrays.asList(Assignment.Operator.PLUS_ASSIGN,
                Assignment.Operator.MINUS_ASSIGN).contains(assignment.getOperator())
                && assignmentAssociatedOperator.equals(assignment.getOperator())) {
            Object assignmentExpr = assignment.resolveConstantExpressionValue();

            if (assignmentExpr instanceof Number
                    && ((Number) assignmentExpr).longValue() == 1) {
                return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide());
            }
        }

        return false;
    }

    private boolean isOneLiteral(Expression operand) {
        Object rightExpr = operand.resolveConstantExpressionValue();

        return rightExpr instanceof Number
                && ((Number) rightExpr).longValue() == 1;
    }

    @Override
    public boolean match(final Block node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof AssertStatement
            || other instanceof BreakStatement
            || other instanceof ConstructorInvocation
            || other instanceof ContinueStatement
            || other instanceof DoStatement
            || other instanceof EmptyStatement
            || other instanceof EnhancedForStatement
            || other instanceof ExpressionStatement
            || other instanceof ForStatement
            || other instanceof IfStatement
            || other instanceof LabeledStatement
            || other instanceof ReturnStatement
            || other instanceof SuperConstructorInvocation
            || other instanceof SwitchStatement
            || other instanceof SynchronizedStatement
            || other instanceof ThrowStatement
            || other instanceof TryStatement
            || other instanceof TypeDeclarationStatement
            || other instanceof VariableDeclarationStatement
            || other instanceof WhileStatement) {
            return match0(node, (Statement) other);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final AssertStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final BreakStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ConstructorInvocation node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ContinueStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final DoStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final EmptyStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final EnhancedForStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ExpressionStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ConditionalExpression node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (super.match(node, other)) {
            return true;
        }

        if (other instanceof ConditionalExpression) {
            ConditionalExpression ce = (ConditionalExpression) other;

            if (node.getElseExpression() != null
                    && ce.getElseExpression() != null) {
                return matchOpposite(node.getExpression(), ce.getExpression())
                        && safeSubtreeMatch(node.getThenExpression(), ce.getElseExpression())
                        && safeSubtreeMatch(node.getElseExpression(), ce.getThenExpression());
            }
        }

        return false;
    }

    @Override
    public boolean match(final ForStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final IfStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        if (super.match(node, other)) {
            return true;
        }

        if (other instanceof IfStatement) {
            IfStatement is = (IfStatement) other;

            if (node.getElseStatement() != null
                    && is.getElseStatement() != null) {
                return matchOpposite(node.getExpression(), is.getExpression())
                        && safeSubtreeMatch(node.getThenStatement(), is.getElseStatement())
                        && safeSubtreeMatch(node.getElseStatement(), is.getThenStatement());
            }
        }

        return false;
    }

    @Override
    public boolean match(final LabeledStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ReturnStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SuperConstructorInvocation node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SwitchStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SynchronizedStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ThrowStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final TryStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final TypeDeclarationStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final VariableDeclarationStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final WhileStatement node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    private boolean match0(final Block node, final Statement other) {
        if ((node.getParent() instanceof IfStatement
                || node.getParent() instanceof ForStatement
                || node.getParent() instanceof EnhancedForStatement
                || node.getParent() instanceof WhileStatement
                || node.getParent() instanceof DoStatement)
                && node.statements().size() == 1) {
            return safeSubtreeMatch(node.statements().get(0), other)
                    || super.match(node, other);
        }

        return super.match(node, other);
    }

    /**
     * Match the boolean opposite.
     *
     * @param node Node to check
     * @param otherObject Node to compare
     * @return True if it is the boolean opposite.
     */
    public boolean matchOpposite(final ASTNode node, final Object otherObject) {
        final Object other = unbracket(otherObject);

        if (node instanceof ParenthesizedExpression) {
            return matchOpposite(((ParenthesizedExpression) node).getExpression(),
                    other);
        }

        if (node instanceof PrefixExpression) {
            final PrefixExpression pe = (PrefixExpression) node;

            if (PrefixExpression.Operator.NOT.equals(pe.getOperator())) {
                if (other instanceof PrefixExpression
                        && PrefixExpression.Operator.NOT.equals(
                                ((PrefixExpression) other).getOperator())) {
                    return matchOpposite(pe.getOperand(), ((PrefixExpression) other).getOperand());
                } else {
                    return safeSubtreeMatch(pe.getOperand(), other);
                }
            }
        } else if (other instanceof PrefixExpression
                && PrefixExpression.Operator.NOT.equals(
                        ((PrefixExpression) other).getOperator())) {
            return safeSubtreeMatch(node, ((PrefixExpression) other).getOperand());
        }

        if (node instanceof BooleanLiteral && other instanceof BooleanLiteral) {
            return ((BooleanLiteral) node).booleanValue() ^ ((BooleanLiteral) other).booleanValue();
        }

        if (!(node instanceof InfixExpression) || !(other instanceof InfixExpression)) {
            return false;
        }

        final InfixExpression ie1 = (InfixExpression) node;
        final InfixExpression ie2 = (InfixExpression) other;

        if ((ie1.hasExtendedOperands() ^ ie2.hasExtendedOperands())
                || (ie1.hasExtendedOperands() && ie1.extendedOperands().size() != ie2.extendedOperands().size())) {
            return false;
        }

        final Expression leftOperand1 = ie1.getLeftOperand();
        final Expression rightOperand1 = ie1.getRightOperand();
        final Expression leftOperand2 = ie2.getLeftOperand();
        final Expression rightOperand2 = ie2.getRightOperand();

        if (ie1.getOperator().equals(ie2.getOperator())) {
            if (!ie1.hasExtendedOperands()
                    && !ie2.hasExtendedOperands()) {
                if (Arrays.asList(
                        InfixExpression.Operator.EQUALS,
                        InfixExpression.Operator.NOT_EQUALS,
                        InfixExpression.Operator.XOR).contains(ie1.getOperator())) {
                    if (matchOneOppositeOther(leftOperand1, leftOperand2, rightOperand2, rightOperand1)
                            || matchOneOppositeOther(rightOperand2, rightOperand1, leftOperand1, leftOperand2)) {
                        return true;
                    }

                    if (isPassive(leftOperand1)
                            && isPassive(rightOperand1)
                            && isPassive(leftOperand2)
                            && isPassive(rightOperand2)
                            && (matchOneOppositeOther(leftOperand1, leftOperand2, rightOperand2, rightOperand1)
                                    || matchOneOppositeOther(rightOperand2, rightOperand1, leftOperand1,
                                            leftOperand2))) {
                        return true;
                    }
                } else if (isPassive(leftOperand1)
                        && isPassive(rightOperand1)
                        && isPassive(leftOperand2)
                        && isPassive(rightOperand2)
                        && Arrays.asList(
                                InfixExpression.Operator.GREATER,
                                InfixExpression.Operator.GREATER_EQUALS,
                                InfixExpression.Operator.LESS,
                                InfixExpression.Operator.LESS_EQUALS).contains(ie1.getOperator())) {
                    return safeSubtreeMatch(ie1.getLeftOperand(), ie2.getRightOperand())
                            && safeSubtreeMatch(ie1.getRightOperand(), ie2.getLeftOperand());
                }
            }

            return false;
        }

        final Operator reverseOp = (Operator) OperatorEnum.getOperator(ie1).getReverseBooleanOperator();

        if (ie2.getOperator().equals(reverseOp)) {
            if (Arrays.asList(
                    InfixExpression.Operator.AND,
                    InfixExpression.Operator.CONDITIONAL_AND,
                    InfixExpression.Operator.OR,
                    InfixExpression.Operator.CONDITIONAL_OR).contains(ie1.getOperator())) {
                return isOperandsMatching(ie1, ie2, false);
            } else if (Arrays.asList(
                    InfixExpression.Operator.EQUALS,
                    InfixExpression.Operator.NOT_EQUALS).contains(ie1.getOperator())) {
                return isOperandsMatching(ie1, ie2, true);
            } else if (isPassive(leftOperand1)
                    && isPassive(rightOperand1)
                    && isPassive(leftOperand2)
                    && isPassive(rightOperand2)
                    && Arrays.asList(
                            InfixExpression.Operator.GREATER,
                            InfixExpression.Operator.GREATER_EQUALS,
                            InfixExpression.Operator.LESS,
                            InfixExpression.Operator.LESS_EQUALS).contains(ie1.getOperator())) {
                return safeSubtreeMatch(leftOperand1, leftOperand2)
                        && safeSubtreeMatch(rightOperand1, rightOperand2);
            }

            return false;
        }

        if (isPassive(leftOperand1)
                && isPassive(rightOperand1)
                && isPassive(leftOperand2)
                && isPassive(rightOperand2)
                && !ie1.hasExtendedOperands()
                && !ie2.hasExtendedOperands()) {
            if ((InfixExpression.Operator.GREATER.equals(ie1.getOperator())
                    && InfixExpression.Operator.GREATER_EQUALS.equals(ie2.getOperator()))
                    || (InfixExpression.Operator.GREATER_EQUALS.equals(ie1.getOperator())
                            && InfixExpression.Operator.GREATER.equals(ie2.getOperator()))
                    || (InfixExpression.Operator.LESS.equals(ie1.getOperator())
                            && InfixExpression.Operator.LESS_EQUALS.equals(ie2.getOperator()))
                    || (InfixExpression.Operator.LESS_EQUALS.equals(ie1.getOperator())
                            && InfixExpression.Operator.LESS.equals(ie2.getOperator()))) {
                return safeSubtreeMatch(leftOperand1, rightOperand2)
                        && safeSubtreeMatch(rightOperand1, leftOperand2);
            }
        }

        return false;
    }

    private boolean matchOneOppositeOther(final Expression equalOperand1, final Expression equalOperand2,
            final Expression oppositeOperand1,
            final Expression oppositeOperand2) {
        return safeSubtreeMatch(equalOperand1, equalOperand2)
                && matchOpposite(oppositeOperand1, oppositeOperand2);
    }

    private boolean isOperandsMatching(final InfixExpression ie1, final InfixExpression ie2, final boolean equal) {
        final List<Expression> operands1 = new ArrayList<Expression>();
        operands1.add(ie1.getLeftOperand());
        operands1.add(ie1.getRightOperand());
        operands1.addAll(ie1.extendedOperands());

        final List<Expression> operands2 = new ArrayList<Expression>();
        operands2.add(ie2.getLeftOperand());
        operands2.add(ie2.getRightOperand());
        operands2.addAll(ie2.extendedOperands());

        if (operands1.size() != operands2.size()) {
            return false;
        }

        boolean isMatching = true;
        final Iterator<Expression> iterator = operands1.iterator();
        final Iterator<Expression> iterator2 = operands2.iterator();

        while (iterator.hasNext() && iterator2.hasNext()) {
            final Expression expr = iterator.next();
            final Expression otherExpr = iterator2.next();

            if (equal ? !safeSubtreeMatch(expr, otherExpr) : !matchOpposite(expr, otherExpr)) {
                isMatching = false;
                break;
            }
        }

        if (isMatching) {
            return true;
        }

        for (Expression expression : operands1) {
            if (!isPassive(expression)) {
                return false;
            }
        }

        for (Expression expression : operands2) {
            if (!isPassive(expression)) {
                return false;
            }
        }

        for (Iterator<Expression> iterator3 = operands1.iterator(); iterator3.hasNext();) {
            final Expression expr = iterator3.next();

            for (Iterator<Expression> iterator4 = operands2.iterator(); iterator4.hasNext();) {
                final Expression otherExpr = iterator4.next();

                if (equal ? safeSubtreeMatch(expr, otherExpr) : matchOpposite(expr, otherExpr)) {
                    iterator3.remove();
                    iterator4.remove();
                    break;
                }
            }
        }

        return operands1.isEmpty() && operands2.isEmpty();
    }
}
