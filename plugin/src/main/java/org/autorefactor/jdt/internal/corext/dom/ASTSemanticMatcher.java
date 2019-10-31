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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.autorefactor.jdt.internal.ui.fix.OperatorEnum;
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
import org.eclipse.jdt.core.dom.LabeledStatement;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
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
    private static final Map<PrefixExpression.Operator, InfixExpression.Operator> PREFIX_TO_INFIX_OPERATOR= new HashMap<PrefixExpression.Operator, InfixExpression.Operator>() {
        private static final long serialVersionUID= -8949107654517355855L;

        {
            put(PrefixExpression.Operator.INCREMENT, InfixExpression.Operator.PLUS);
            put(PrefixExpression.Operator.DECREMENT, InfixExpression.Operator.MINUS);
        }
    };

    private static final Map<PrefixExpression.Operator, Assignment.Operator> PREFIX_TO_ASSIGN_OPERATOR= new HashMap<PrefixExpression.Operator, Assignment.Operator>() {
        private static final long serialVersionUID= -8949107654517355856L;

        {
            put(PrefixExpression.Operator.INCREMENT, Assignment.Operator.PLUS_ASSIGN);
            put(PrefixExpression.Operator.DECREMENT, Assignment.Operator.MINUS_ASSIGN);
        }
    };

    private static final Map<PostfixExpression.Operator, InfixExpression.Operator> POSTFIX_TO_INFIX_OPERATOR= new HashMap<PostfixExpression.Operator, InfixExpression.Operator>() {
        private static final long serialVersionUID= -8949107654517355857L;

        {
            put(PostfixExpression.Operator.INCREMENT, InfixExpression.Operator.PLUS);
            put(PostfixExpression.Operator.DECREMENT, InfixExpression.Operator.MINUS);
        }
    };

    private static final Map<PostfixExpression.Operator, Assignment.Operator> POSTFIX_TO_ASSIGN_OPERATOR= new HashMap<PostfixExpression.Operator, Assignment.Operator>() {
        private static final long serialVersionUID= -8949107654517355858L;

        {
            put(PostfixExpression.Operator.INCREMENT, Assignment.Operator.PLUS_ASSIGN);
            put(PostfixExpression.Operator.DECREMENT, Assignment.Operator.MINUS_ASSIGN);
        }
    };

    private static final Map<PrefixExpression.Operator, PostfixExpression.Operator> PREFIX_TO_POSTFIX_OPERATOR= new HashMap<PrefixExpression.Operator, PostfixExpression.Operator>() {
        private static final long serialVersionUID= -8949107654517355859L;

        {
            put(PrefixExpression.Operator.INCREMENT, PostfixExpression.Operator.INCREMENT);
            put(PrefixExpression.Operator.DECREMENT, PostfixExpression.Operator.DECREMENT);
        }
    };

    private static final Map<Assignment.Operator, InfixExpression.Operator> ASSIGN_TO_INFIX_OPERATOR= new HashMap<Assignment.Operator, InfixExpression.Operator>() {
        private static final long serialVersionUID= -8949107654517355859L;

        {
            put(Assignment.Operator.PLUS_ASSIGN, InfixExpression.Operator.PLUS);
            put(Assignment.Operator.MINUS_ASSIGN, InfixExpression.Operator.MINUS);
            put(Assignment.Operator.TIMES_ASSIGN, InfixExpression.Operator.TIMES);
            put(Assignment.Operator.DIVIDE_ASSIGN, InfixExpression.Operator.DIVIDE);
            put(Assignment.Operator.BIT_AND_ASSIGN, InfixExpression.Operator.AND);
            put(Assignment.Operator.BIT_OR_ASSIGN, InfixExpression.Operator.OR);
            put(Assignment.Operator.BIT_XOR_ASSIGN, InfixExpression.Operator.XOR);
            put(Assignment.Operator.REMAINDER_ASSIGN, InfixExpression.Operator.REMAINDER);
            put(Assignment.Operator.LEFT_SHIFT_ASSIGN, InfixExpression.Operator.LEFT_SHIFT);
            put(Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN, InfixExpression.Operator.RIGHT_SHIFT_SIGNED);
            put(Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN, InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED);
        }
    };

    private static final Map<InfixExpression.Operator, InfixExpression.Operator> INFIX_TO_MIRROR_OPERATOR= new HashMap<InfixExpression.Operator, InfixExpression.Operator>() {
        private static final long serialVersionUID= -8949107654517355857L;

        {
            put(InfixExpression.Operator.EQUALS, InfixExpression.Operator.EQUALS);
            put(InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.NOT_EQUALS);
            put(InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_AND);
            put(InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.CONDITIONAL_OR);
            put(InfixExpression.Operator.AND, InfixExpression.Operator.AND);
            put(InfixExpression.Operator.OR, InfixExpression.Operator.OR);
            put(InfixExpression.Operator.XOR, InfixExpression.Operator.XOR);
            put(InfixExpression.Operator.GREATER, InfixExpression.Operator.LESS);
            put(InfixExpression.Operator.LESS, InfixExpression.Operator.GREATER);
            put(InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.GREATER_EQUALS);
            put(InfixExpression.Operator.GREATER_EQUALS, InfixExpression.Operator.LESS_EQUALS);
        }
    };

    @Override
    public boolean match(final InfixExpression node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof PrefixExpression) {
            PrefixExpression pe= (PrefixExpression) other;

            if (ASTNodes.hasOperator(pe, PrefixExpression.Operator.NOT)) {
                return matchOpposite(node, pe.getOperand());
            }
        }

        if (other instanceof InfixExpression) {
            InfixExpression ie= (InfixExpression) other;

            if (!node.hasExtendedOperands() && !ie.hasExtendedOperands() && ASTNodes.isPassive(node.getLeftOperand())
                    && ASTNodes.isPassive(node.getRightOperand())
                    && safeSubtreeMatch(node.getLeftOperand(), ie.getRightOperand())
                    && safeSubtreeMatch(node.getRightOperand(), ie.getLeftOperand()) && (node.getOperator().equals(INFIX_TO_MIRROR_OPERATOR.get(ie.getOperator())) || (ASTNodes.hasOperator(ie, InfixExpression.Operator.PLUS, InfixExpression.Operator.TIMES)
                    && node.getOperator().equals(ie.getOperator())
                    && ASTNodes.hasType(node.getLeftOperand(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName(), float.class.getSimpleName(), double.class.getSimpleName(), Short.class.getCanonicalName(),
                            Integer.class.getCanonicalName(), Long.class.getCanonicalName(), Float.class.getCanonicalName(), Double.class.getCanonicalName())
                    && ASTNodes.haveSameType(node.getLeftOperand(), node.getRightOperand())))) {
                return true;
            }

            if (node.getOperator().equals(ie.getOperator()) && ASTNodes.hasOperator(ie, InfixExpression.Operator.PLUS, InfixExpression.Operator.TIMES, InfixExpression.Operator.AND,
                            InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.OR,
                            InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.XOR)) {
                return isOperandsMatching(node, ie, true);
            }
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ParenthesizedExpression node, final Object otherObject) {
        final Object other= unbracket(otherObject);

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
        final Object other= unbracket(otherObject);

        if (!(other instanceof PrefixExpression) && ASTNodes.hasOperator(node, PrefixExpression.Operator.NOT)) {
            return matchOpposite(node.getOperand(), other);
        }

        if (node.getParent() instanceof Statement) {
            if (other instanceof Assignment) {
                return match0(node, (Assignment) other);
            }
            if (other instanceof PostfixExpression) {
                return match0(node, (PostfixExpression) other);
            }
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final PostfixExpression node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (node.getParent() instanceof Statement) {
            if (other instanceof Assignment) {
                return match0(node, (Assignment) other);
            }
            if (other instanceof PrefixExpression) {
                return match0((PrefixExpression) other, node);
            }
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final Assignment node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof PrefixExpression && ((PrefixExpression) other).getParent() instanceof Statement) {
            return match0((PrefixExpression) other, node);
        }
        if (other instanceof PostfixExpression && ((PostfixExpression) other).getParent() instanceof Statement) {
            return match0((PostfixExpression) other, node);
        }
        if (other instanceof Assignment) {
            return matchAssignmentWithAndWithoutEqual(node, (Assignment) other)
                    || matchAssignmentWithAndWithoutEqual((Assignment) other, node) || super.match(node, other);
        }

        return super.match(node, other);
    }

    private boolean matchAssignmentWithAndWithoutEqual(final Assignment node, final Assignment assignment) {
        if (ASTNodes.hasOperator(node, Assignment.Operator.ASSIGN)
                && node.getRightHandSide() instanceof InfixExpression) {
            InfixExpression infixExpression= (InfixExpression) node.getRightHandSide();

            if (!infixExpression.hasExtendedOperands()
                    && ASTNodes.hasOperator(assignment, Assignment.Operator.PLUS_ASSIGN, Assignment.Operator.MINUS_ASSIGN,
                            Assignment.Operator.TIMES_ASSIGN, Assignment.Operator.DIVIDE_ASSIGN,
                            Assignment.Operator.BIT_AND_ASSIGN, Assignment.Operator.BIT_OR_ASSIGN,
                            Assignment.Operator.BIT_XOR_ASSIGN, Assignment.Operator.REMAINDER_ASSIGN,
                            Assignment.Operator.LEFT_SHIFT_ASSIGN, Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN,
                            Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN)
                    && ASSIGN_TO_INFIX_OPERATOR.get(assignment.getOperator()).equals(infixExpression.getOperator())) {
                return safeSubtreeMatch(node.getLeftHandSide(), assignment.getLeftHandSide())
                        && safeSubtreeMatch(infixExpression.getLeftOperand(), assignment.getLeftHandSide())
                        && safeSubtreeMatch(infixExpression.getRightOperand(), assignment.getRightHandSide());
            }
        }

        return false;
    }

    private boolean match0(final PrefixExpression prefixExpression, final PostfixExpression postfixExpression) {
        return postfixExpression.getOperator().equals(PREFIX_TO_POSTFIX_OPERATOR.get(prefixExpression.getOperator()))
                && safeSubtreeMatch(prefixExpression.getOperand(), postfixExpression.getOperand());
    }

    private boolean match0(final PrefixExpression prefixExpression, final Assignment assignment) {
        return match0(assignment, prefixExpression.getOperand(), PREFIX_TO_INFIX_OPERATOR.get(prefixExpression.getOperator()),
                PREFIX_TO_ASSIGN_OPERATOR.get(prefixExpression.getOperator()));
    }

    private boolean match0(final PostfixExpression postfixExpression, final Assignment assignment) {
        return match0(assignment, postfixExpression.getOperand(), POSTFIX_TO_INFIX_OPERATOR.get(postfixExpression.getOperator()),
                POSTFIX_TO_ASSIGN_OPERATOR.get(postfixExpression.getOperator()));
    }

    private boolean match0(final Assignment assignment, final Expression prefixOrPostfixOperand,
            final InfixExpression.Operator infixAssociatedOperator, final Assignment.Operator assignmentAssociatedOperator) {
        if (ASTNodes.hasOperator(assignment, Assignment.Operator.ASSIGN)
                && assignment.getRightHandSide() instanceof InfixExpression) {
            InfixExpression infixExpression= (InfixExpression) assignment.getRightHandSide();
            if (!infixExpression.hasExtendedOperands() && infixAssociatedOperator.equals(infixExpression.getOperator())) {
                if (isOneLiteral(infixExpression.getRightOperand())) {
                    return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide())
                            && safeSubtreeMatch(prefixOrPostfixOperand, infixExpression.getLeftOperand());
                }
                if (ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.PLUS) && isOneLiteral(infixExpression.getLeftOperand())) {
                    return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide())
                            && safeSubtreeMatch(prefixOrPostfixOperand, infixExpression.getRightOperand());
                }
            }
        } else if (ASTNodes.hasOperator(assignment, Assignment.Operator.PLUS_ASSIGN, Assignment.Operator.MINUS_ASSIGN) && assignmentAssociatedOperator.equals(assignment.getOperator())) {
            Object assignmentExpression= assignment.resolveConstantExpressionValue();

            if (assignmentExpression instanceof Number && ((Number) assignmentExpression).longValue() == 1) {
                return safeSubtreeMatch(prefixOrPostfixOperand, assignment.getLeftHandSide());
            }
        }

        return false;
    }

    private boolean isOneLiteral(Expression operand) {
        Object rightExpression= operand.resolveConstantExpressionValue();

        return rightExpression instanceof Number && ((Number) rightExpression).longValue() == 1;
    }

    @Override
    public boolean match(final Block node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof AssertStatement || other instanceof BreakStatement
                || other instanceof ConstructorInvocation || other instanceof ContinueStatement
                || other instanceof DoStatement || other instanceof EmptyStatement
                || other instanceof EnhancedForStatement || other instanceof ExpressionStatement
                || other instanceof ForStatement || other instanceof IfStatement || other instanceof LabeledStatement
                || other instanceof ReturnStatement || other instanceof SuperConstructorInvocation
                || other instanceof SwitchStatement || other instanceof SynchronizedStatement
                || other instanceof ThrowStatement || other instanceof TryStatement
                || other instanceof TypeDeclarationStatement || other instanceof VariableDeclarationStatement
                || other instanceof WhileStatement) {
            return match0(node, (Statement) other);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final AssertStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final BreakStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ConstructorInvocation node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ContinueStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final DoStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final EmptyStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final EnhancedForStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ExpressionStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ConditionalExpression node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (super.match(node, other)) {
            return true;
        }

        if (other instanceof ConditionalExpression) {
            ConditionalExpression ce= (ConditionalExpression) other;

            if (node.getElseExpression() != null && ce.getElseExpression() != null) {
                return matchOpposite(node.getExpression(), ce.getExpression())
                        && safeSubtreeMatch(node.getThenExpression(), ce.getElseExpression())
                        && safeSubtreeMatch(node.getElseExpression(), ce.getThenExpression());
            }
        }

        return false;
    }

    @Override
    public boolean match(final ForStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final IfStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        if (super.match(node, other)) {
            return true;
        }

        if (other instanceof IfStatement) {
            IfStatement is= (IfStatement) other;

            if (node.getElseStatement() != null && is.getElseStatement() != null) {
                return matchOpposite(node.getExpression(), is.getExpression())
                        && safeSubtreeMatch(node.getThenStatement(), is.getElseStatement())
                        && safeSubtreeMatch(node.getElseStatement(), is.getThenStatement());
            }
        }

        return false;
    }

    @Override
    public boolean match(final LabeledStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ReturnStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SuperConstructorInvocation node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SwitchStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final SynchronizedStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final ThrowStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final TryStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final TypeDeclarationStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final VariableDeclarationStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    @Override
    public boolean match(final WhileStatement node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (other instanceof Block) {
            return match0((Block) other, (Statement) node);
        }

        return super.match(node, other);
    }

    private boolean match0(final Block node, final Statement other) {
        if ((node.getParent() instanceof IfStatement || node.getParent() instanceof ForStatement
                || node.getParent() instanceof EnhancedForStatement || node.getParent() instanceof WhileStatement
                || node.getParent() instanceof DoStatement) && node.statements().size() == 1) {
            return safeSubtreeMatch(node.statements().get(0), other) || super.match(node, other);
        }

        return super.match(node, other);
    }

    /**
     * Match the boolean opposite.
     *
     * @param node        Node to check
     * @param otherObject Node to compare
     * @return True if it is the boolean opposite.
     */
    public boolean matchOpposite(final ASTNode node, final Object otherObject) {
        final Object other= unbracket(otherObject);

        if (node instanceof ParenthesizedExpression) {
            return matchOpposite(((ParenthesizedExpression) node).getExpression(), other);
        }

        if (node instanceof PrefixExpression) {
            final PrefixExpression pe= (PrefixExpression) node;

            if (ASTNodes.hasOperator(pe, PrefixExpression.Operator.NOT)) {
                if (other instanceof PrefixExpression
                        && ASTNodes.hasOperator((PrefixExpression) other, PrefixExpression.Operator.NOT)) {
                    return matchOpposite(pe.getOperand(), ((PrefixExpression) other).getOperand());
                }
                return safeSubtreeMatch(pe.getOperand(), other);
            }
        } else if (other instanceof PrefixExpression
                && ASTNodes.hasOperator((PrefixExpression) other, PrefixExpression.Operator.NOT)) {
            return safeSubtreeMatch(node, ((PrefixExpression) other).getOperand());
        }

        if (node instanceof BooleanLiteral && other instanceof BooleanLiteral) {
            return ((BooleanLiteral) node).booleanValue() ^ ((BooleanLiteral) other).booleanValue();
        }

        if (!(node instanceof InfixExpression) || !(other instanceof InfixExpression)) {
            return false;
        }

        final InfixExpression ie1= (InfixExpression) node;
        final InfixExpression ie2= (InfixExpression) other;

        if (ie1.hasExtendedOperands() ^ ie2.hasExtendedOperands()
                || (ie1.hasExtendedOperands() && ie1.extendedOperands().size() != ie2.extendedOperands().size())) {
            return false;
        }

        final Expression leftOperand1= ie1.getLeftOperand();
        final Expression rightOperand1= ie1.getRightOperand();
        final Expression leftOperand2= ie2.getLeftOperand();
        final Expression rightOperand2= ie2.getRightOperand();

        if (ie1.getOperator().equals(ie2.getOperator())) {
            if (!ie1.hasExtendedOperands() && !ie2.hasExtendedOperands()) {
                if (ASTNodes.hasOperator(ie1, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS,
                        InfixExpression.Operator.XOR)) {
                    if (matchOneOppositeOther(leftOperand1, leftOperand2, rightOperand2, rightOperand1)
                            || matchOneOppositeOther(rightOperand2, rightOperand1, leftOperand1, leftOperand2) || (ASTNodes.isPassive(leftOperand1) && ASTNodes.isPassive(rightOperand1) && ASTNodes.isPassive(leftOperand2)
                            && ASTNodes.isPassive(rightOperand2)
                            && (matchOneOppositeOther(leftOperand1, leftOperand2, rightOperand2, rightOperand1)
                                    || matchOneOppositeOther(rightOperand2, rightOperand1, leftOperand1,
                                            leftOperand2)))) {
                        return true;
                    }
                } else if (ASTNodes.isPassive(leftOperand1) && ASTNodes.isPassive(rightOperand1) && ASTNodes.isPassive(leftOperand2)
                        && ASTNodes.isPassive(rightOperand2)
                        && ASTNodes.hasOperator(ie1, InfixExpression.Operator.GREATER, InfixExpression.Operator.GREATER_EQUALS,
                                        InfixExpression.Operator.LESS, InfixExpression.Operator.LESS_EQUALS)) {
                    return safeSubtreeMatch(ie1.getLeftOperand(), ie2.getRightOperand())
                            && safeSubtreeMatch(ie1.getRightOperand(), ie2.getLeftOperand());
                }
            }

            return false;
        }

        final InfixExpression.Operator reverseOp= (InfixExpression.Operator) OperatorEnum.getOperator(ie1).getReverseBooleanOperator();

        if (ie2.getOperator().equals(reverseOp)) {
            if (ASTNodes.hasOperator(ie1, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR,
                    InfixExpression.Operator.AND, InfixExpression.Operator.OR)) {
                return isOperandsMatching(ie1, ie2, false);
            }
            if (ASTNodes.hasOperator(ie1, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)) {
                return isOperandsMatching(ie1, ie2, true);
            }
            if (ASTNodes.isPassive(leftOperand1) && ASTNodes.isPassive(rightOperand1) && ASTNodes.isPassive(leftOperand2)
                    && ASTNodes.isPassive(rightOperand2)
                    && ASTNodes.hasOperator(ie1, InfixExpression.Operator.GREATER, InfixExpression.Operator.GREATER_EQUALS,
                                    InfixExpression.Operator.LESS, InfixExpression.Operator.LESS_EQUALS)) {
                return safeSubtreeMatch(leftOperand1, leftOperand2) && safeSubtreeMatch(rightOperand1, rightOperand2);
            }

            return false;
        }

        return ASTNodes.isPassive(leftOperand1) && ASTNodes.isPassive(rightOperand1) && ASTNodes.isPassive(leftOperand2) && ASTNodes.isPassive(rightOperand2)
                && !ie1.hasExtendedOperands() && !ie2.hasExtendedOperands() && ((ASTNodes.hasOperator(ie1, InfixExpression.Operator.GREATER)
                && ASTNodes.hasOperator(ie2, InfixExpression.Operator.GREATER_EQUALS))
                || (ASTNodes.hasOperator(ie1, InfixExpression.Operator.GREATER_EQUALS)
                        && ASTNodes.hasOperator(ie2, InfixExpression.Operator.GREATER))
                || (ASTNodes.hasOperator(ie1, InfixExpression.Operator.LESS)
                        && ASTNodes.hasOperator(ie2, InfixExpression.Operator.LESS_EQUALS))
                || (ASTNodes.hasOperator(ie1, InfixExpression.Operator.LESS_EQUALS)
                        && ASTNodes.hasOperator(ie2, InfixExpression.Operator.LESS))) && safeSubtreeMatch(leftOperand1, rightOperand2) && safeSubtreeMatch(rightOperand1, leftOperand2);
    }

    private boolean matchOneOppositeOther(final Expression equalOperand1, final Expression equalOperand2,
            final Expression oppositeOperand1, final Expression oppositeOperand2) {
        return safeSubtreeMatch(equalOperand1, equalOperand2) && matchOpposite(oppositeOperand1, oppositeOperand2);
    }

    private boolean isOperandsMatching(final InfixExpression ie1, final InfixExpression ie2, final boolean equal) {
        final List<Expression> operands1 = getConsistentOperands(ie1);
        final List<Expression> operands2 = getConsistentOperands(ie2);

        if (operands1.size() != operands2.size()) {
            return false;
        }

        boolean isMatching= true;
        final Iterator<Expression> iterator1= operands1.iterator();
        final Iterator<Expression> iterator2= operands2.iterator();

        while (iterator1.hasNext() && iterator2.hasNext()) {
            final Expression expression= iterator1.next();
            final Expression otherExpression= iterator2.next();

            if (equal ? !safeSubtreeMatch(expression, otherExpression) : !matchOpposite(expression, otherExpression)) {
                isMatching= false;
                break;
            }
        }

        if (isMatching) {
            return true;
        }

        for (Expression expression : operands1) {
            if (!ASTNodes.isPassive(expression)) {
                return false;
            }
        }

        for (Expression expression : operands2) {
            if (!ASTNodes.isPassive(expression)) {
                return false;
            }
        }

        for (Iterator<Expression> iterator3= operands1.iterator(); iterator3.hasNext();) {
            final Expression expression= iterator3.next();

            for (Iterator<Expression> iterator4= operands2.iterator(); iterator4.hasNext();) {
                final Expression otherExpression= iterator4.next();

                if (equal ? safeSubtreeMatch(expression, otherExpression) : matchOpposite(expression, otherExpression)) {
                    iterator3.remove();
                    iterator4.remove();
                    break;
                }
            }
        }

        return operands1.isEmpty() && operands2.isEmpty();
    }

    private List<Expression> getConsistentOperands(final InfixExpression ie) {
        final List<Expression> operands= ASTNodes.allOperands(ie);

        for (Iterator<Expression> iterator= operands.iterator(); iterator.hasNext() && operands.size() > 1;) {
            final Expression operand= iterator.next();

            final Long numberLiteral= ASTNodes.integerLiteral(operand);
            final BooleanLiteral booleanLiteral= ASTNodes.as(operand, BooleanLiteral.class);
            final QualifiedName booleanConstant= ASTNodes.as(operand, QualifiedName.class);

            if (ASTNodes.hasOperator(ie, InfixExpression.Operator.CONDITIONAL_AND)) {
                if (ASTNodes.isField(booleanConstant, Boolean.class.getCanonicalName(), "TRUE") || (booleanLiteral != null && Boolean.TRUE.equals(booleanLiteral.booleanValue()))) { //$NON-NLS-1$
                    iterator.remove();
                }
            } else if (ASTNodes.hasOperator(ie, InfixExpression.Operator.CONDITIONAL_OR)) {
                if (ASTNodes.isField(booleanConstant, Boolean.class.getCanonicalName(), "FALSE") || (booleanLiteral != null && Boolean.FALSE.equals(booleanLiteral.booleanValue()))) { //$NON-NLS-1$
                    iterator.remove();
                }
            } else if (ASTNodes.hasOperator(ie, InfixExpression.Operator.PLUS)) {
                if (numberLiteral != null && numberLiteral == 0) {
                    iterator.remove();
                }
            } else if (ASTNodes.hasOperator(ie, InfixExpression.Operator.TIMES) && numberLiteral != null && numberLiteral == 1) {
                iterator.remove();
            }
        }

        return operands;
    }
}
