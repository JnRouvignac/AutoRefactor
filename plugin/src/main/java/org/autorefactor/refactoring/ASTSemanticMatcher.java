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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.core.dom.ASTMatcher;
import org.eclipse.jdt.core.dom.AssertStatement;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BreakStatement;
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
    private static final Map<PrefixExpression.Operator, InfixExpression.Operator> PREFIX_TO_INFIX_OPERATOR =
            new HashMap<PrefixExpression.Operator, InfixExpression.Operator>() {
            private static final long serialVersionUID = -8949107654517355855L;

            {
                this.put(PrefixExpression.Operator.INCREMENT, InfixExpression.Operator.PLUS);
                this.put(PrefixExpression.Operator.DECREMENT, InfixExpression.Operator.MINUS);
            }
        };

    private static final Map<PrefixExpression.Operator, Assignment.Operator> PREFIX_TO_ASSIGN_OPERATOR =
            new HashMap<PrefixExpression.Operator, Assignment.Operator>() {
            private static final long serialVersionUID = -8949107654517355856L;

            {
                this.put(PrefixExpression.Operator.INCREMENT, Assignment.Operator.PLUS_ASSIGN);
                this.put(PrefixExpression.Operator.DECREMENT, Assignment.Operator.MINUS_ASSIGN);
            }
        };

    private static final Map<PostfixExpression.Operator, InfixExpression.Operator> POSTFIX_TO_INFIX_OPERATOR =
            new HashMap<PostfixExpression.Operator, InfixExpression.Operator>() {
            private static final long serialVersionUID = -8949107654517355857L;

            {
                this.put(PostfixExpression.Operator.INCREMENT, InfixExpression.Operator.PLUS);
                this.put(PostfixExpression.Operator.DECREMENT, InfixExpression.Operator.MINUS);
            }
        };

    private static final Map<PostfixExpression.Operator, Assignment.Operator> POSTFIX_TO_ASSIGN_OPERATOR =
            new HashMap<PostfixExpression.Operator, Assignment.Operator>() {
            private static final long serialVersionUID = -8949107654517355858L;

            {
                this.put(PostfixExpression.Operator.INCREMENT, Assignment.Operator.PLUS_ASSIGN);
                this.put(PostfixExpression.Operator.DECREMENT, Assignment.Operator.MINUS_ASSIGN);
            }
        };

    private static final Map<PrefixExpression.Operator, PostfixExpression.Operator> PREFIX_TO_POSTFIX_OPERATOR =
            new HashMap<PrefixExpression.Operator, PostfixExpression.Operator>() {
            private static final long serialVersionUID = -8949107654517355859L;

            {
                this.put(PrefixExpression.Operator.INCREMENT, PostfixExpression.Operator.INCREMENT);
                this.put(PrefixExpression.Operator.DECREMENT, PostfixExpression.Operator.DECREMENT);
            }
        };

    private static final Map<Assignment.Operator, InfixExpression.Operator> ASSIGN_TO_INFIX_OPERATOR =
            new HashMap<Assignment.Operator, InfixExpression.Operator>() {
            private static final long serialVersionUID = -8949107654517355859L;

            {
                this.put(Assignment.Operator.PLUS_ASSIGN, InfixExpression.Operator.PLUS);
                this.put(Assignment.Operator.MINUS_ASSIGN, InfixExpression.Operator.MINUS);
                this.put(Assignment.Operator.TIMES_ASSIGN, InfixExpression.Operator.TIMES);
                this.put(Assignment.Operator.DIVIDE_ASSIGN, InfixExpression.Operator.DIVIDE);
                this.put(Assignment.Operator.BIT_AND_ASSIGN, InfixExpression.Operator.AND);
                this.put(Assignment.Operator.BIT_OR_ASSIGN, InfixExpression.Operator.OR);
                this.put(Assignment.Operator.BIT_XOR_ASSIGN, InfixExpression.Operator.XOR);
                this.put(Assignment.Operator.REMAINDER_ASSIGN, InfixExpression.Operator.REMAINDER);
                this.put(Assignment.Operator.LEFT_SHIFT_ASSIGN, InfixExpression.Operator.LEFT_SHIFT);
                this.put(Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN, InfixExpression.Operator.RIGHT_SHIFT_SIGNED);
                this.put(Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN,
                        InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED);
            }
        };

    private static final Map<InfixExpression.Operator, InfixExpression.Operator> INFIX_TO_MIRROR_OPERATOR =
            new HashMap<InfixExpression.Operator, InfixExpression.Operator>() {
            private static final long serialVersionUID = -8949107654517355857L;

            {
                this.put(InfixExpression.Operator.EQUALS, InfixExpression.Operator.EQUALS);
                this.put(InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.NOT_EQUALS);
                this.put(InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_AND);
                this.put(InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.CONDITIONAL_OR);
                this.put(InfixExpression.Operator.AND, InfixExpression.Operator.AND);
                this.put(InfixExpression.Operator.OR, InfixExpression.Operator.OR);
                this.put(InfixExpression.Operator.XOR, InfixExpression.Operator.XOR);
                this.put(InfixExpression.Operator.GREATER, InfixExpression.Operator.LESS);
                this.put(InfixExpression.Operator.LESS, InfixExpression.Operator.GREATER);
                this.put(InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.GREATER_EQUALS);
                this.put(InfixExpression.Operator.GREATER_EQUALS, InfixExpression.Operator.LESS_EQUALS);
            }
        };

    @Override
    public boolean match(final InfixExpression node, final Object other) {
        if (!node.hasExtendedOperands()
                && other instanceof InfixExpression) {
            InfixExpression ie = (InfixExpression) other;

            if (!ie.hasExtendedOperands()
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
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final PrefixExpression node, final Object other) {
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
    public boolean match(final PostfixExpression node, final Object other) {
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
    public boolean match(final Assignment node, final Object other) {
        if (other instanceof PrefixExpression && (((PrefixExpression) other).getParent() instanceof Statement)) {
            return match0((PrefixExpression) other, node);
        } else if (other instanceof PostfixExpression
                && (((PostfixExpression) other).getParent() instanceof Statement)) {
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
                } else if (InfixExpression.Operator.PLUS.equals(infixExpr.getOperator())
                        && isOneLiteral(infixExpr.getLeftOperand())) {
                    return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide())
                            && safeSubtreeMatch(prefixOrPostfixOperand, infixExpr.getRightOperand());
                }
            }
        } else if (Arrays.asList(Assignment.Operator.PLUS_ASSIGN,
                Assignment.Operator.MINUS_ASSIGN).contains(assignment.getOperator())
                && assignmentAssociatedOperator.equals(assignment.getOperator())) {
            Object assignmentExpr = assignment.resolveConstantExpressionValue();

            if (assignmentExpr != null && assignmentExpr instanceof Number
                    && ((Number) assignmentExpr).longValue() == 1) {
                return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide());
            }
        }

        return false;
    }

    private boolean isOneLiteral(Expression operand) {
        Object rightExpr = operand.resolveConstantExpressionValue();

        return rightExpr != null && rightExpr instanceof Number
                && ((Number) rightExpr).longValue() == 1;
    }

    @Override
    public boolean match(final Block node, final Object other) {
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
    public boolean match(final AssertStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final BreakStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ConstructorInvocation node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ContinueStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final DoStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final EmptyStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final EnhancedForStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ExpressionStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ForStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final IfStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final LabeledStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ReturnStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SuperConstructorInvocation node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SwitchStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SynchronizedStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ThrowStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final TryStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final TypeDeclarationStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final VariableDeclarationStatement node, final Object other) {
        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final WhileStatement node, final Object other) {
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
                    || super.match(node, (Object) other);
        }

        return super.match(node, (Object) other);
    }
}
