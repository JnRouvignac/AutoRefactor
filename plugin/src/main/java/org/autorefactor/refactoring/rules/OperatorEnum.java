/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.core.dom.*;

/**
 * @see <a href="http://introcs.cs.princeton.edu/java/11precedence/"
 *      >Introduction to programming in Java - Appendix A: Operator Precedence in Java</a>
 * @see <a href="http://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html"
 *      >Operators (The Java&trade; Tutorials &gt; Learning the Java Language &gt; Language Basics)</a>
 */
public enum OperatorEnum {

    DOT                        (null,                                             1, false, false),
    PARENTHESES                (null,                                             1, false, false),
    ARRAY_ACCESS               (null,                                             1, false, false),

    POSTFIX_DECREMENT          (PostfixExpression.Operator.DECREMENT,             2, false, false),
    POSTFIX_INCREMENT          (PostfixExpression.Operator.INCREMENT,             2, false, false),

    PREFIX_DECREMENT           (PrefixExpression.Operator.DECREMENT,              3, false, false),
    PREFIX_INCREMENT           (PrefixExpression.Operator.INCREMENT,              3, false, false),
    PREFIX_MINUS               (PrefixExpression.Operator.MINUS,                  3, false, false),
    PREFIX_PLUS                (PrefixExpression.Operator.PLUS,                   3, false, false),
    PREFIX_COMPLEMENT          (PrefixExpression.Operator.COMPLEMENT,             3, false, false),
    PREFIX_NOT                 (PrefixExpression.Operator.NOT,                    3, false, false),

    CAST                       (null,                                             4,  true, false),
    NEW                        (null,                                             4, false, false),

    INFIX_TIMES                (InfixExpression.Operator.TIMES,                   5,  true, false),
    INFIX_DIVIDE               (InfixExpression.Operator.DIVIDE,                  5, false, false),
    INFIX_REMAINDER            (InfixExpression.Operator.REMAINDER,               5, false, false),

    INFIX_MINUS                (InfixExpression.Operator.MINUS,                   6, false, false),
    INFIX_PLUS                 (InfixExpression.Operator.PLUS,                    6,  true, false),

    INFIX_LEFT_SHIFT           (InfixExpression.Operator.LEFT_SHIFT,              7, false, false),
    INFIX_RIGHT_SHIFT_SIGNED   (InfixExpression.Operator.RIGHT_SHIFT_SIGNED,      7, false, false),
    INFIX_RIGHT_SHIFT_UNSIGNED (InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED,    7, false, false),

    INFIX_LESS                 (InfixExpression.Operator.LESS,                    8, false,  true),
    INFIX_LESS_EQUALS          (InfixExpression.Operator.LESS_EQUALS,             8, false,  true),
    INFIX_GREATER              (InfixExpression.Operator.GREATER,                 8, false,  true),
    INFIX_GREATER_EQUALS       (InfixExpression.Operator.GREATER_EQUALS,          8, false,  true),
    INSTANCEOF                 (null,                                             8, false,  true),

    INFIX_EQUALS               (InfixExpression.Operator.EQUALS,                  9, false,  true),
    INFIX_NOT_EQUALS           (InfixExpression.Operator.NOT_EQUALS,              9, false,  true),

    INFIX_BIT_AND              (InfixExpression.Operator.AND,                    10,  true, false),

    INFIX_BIT_XOR              (InfixExpression.Operator.XOR,                    11,  true, false),

    INFIX_BIT_OR               (InfixExpression.Operator.OR,                     12,  true, false),

    INFIX_CONDITIONAL_AND      (InfixExpression.Operator.CONDITIONAL_AND,        13,  true,  true),

    INFIX_CONDITIONAL_OR       (InfixExpression.Operator.CONDITIONAL_OR,         14,  true,  true),

    TERNARY                    (null,                                            15, false, false),

    ASSIGN                     (Assignment.Operator.ASSIGN,                      16, false, false),
    PLUS_ASSIGN                (Assignment.Operator.PLUS_ASSIGN,                 16, false, false),
    MINUS_ASSIGN               (Assignment.Operator.MINUS_ASSIGN,                16, false, false),
    TIMES_ASSIGN               (Assignment.Operator.TIMES_ASSIGN,                16, false, false),
    DIVIDE_ASSIGN              (Assignment.Operator.DIVIDE_ASSIGN,               16, false, false),
    REMAINDER_ASSIGN           (Assignment.Operator.REMAINDER_ASSIGN,            16, false, false),
    BIT_AND_ASSIGN             (Assignment.Operator.BIT_AND_ASSIGN,              16, false, false),
    BIT_OR_ASSIGN              (Assignment.Operator.BIT_OR_ASSIGN,               16, false, false),
    BIT_XOR_ASSIGN             (Assignment.Operator.BIT_XOR_ASSIGN,              16, false, false),
    LEFT_SHIFT_ASSIGN          (Assignment.Operator.LEFT_SHIFT_ASSIGN,           16, false, false),
    RIGHT_SHIFT_SIGNED_ASSIGN  (Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN,   16, false, false),
    RIGHT_SHIFT_UNSIGNED_ASSIGN(Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN, 16, false, false),

    COMMA                      (null,                                            17, false, false);

    /**
     * @see <a
     *      href="http://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html">Java
     *      tutorial - Operators</a>
     */
    private static final Map<Object, OperatorEnum> OPERATORS;
    static {
        final Map<Object, OperatorEnum> m = new HashMap<Object, OperatorEnum>();
        for (OperatorEnum op : OperatorEnum.values()) {
            m.put(op.operator, op);
        }
        OPERATORS = Collections.unmodifiableMap(m);
    }

    private final Object operator;
    private final int precedence;
    private final boolean isAssociative;
    private final boolean isBoolean;

    private OperatorEnum(Object operator, int precedence, boolean isAssociative, boolean isBoolean) {
        this.operator = operator;
        this.precedence = precedence;
        this.isAssociative = isAssociative;
        this.isBoolean = isBoolean;
    }

    public int getPrecedence() {
        return precedence;
    }

    public static boolean isAssociative(InfixExpression.Operator operator) {
        return OPERATORS.get(operator).isAssociative;
    }

    public static boolean isBoolean(InfixExpression.Operator operator) {
        return OPERATORS.get(operator).isBoolean;
    }

    public static int compareTo(ASTNode node1, ASTNode node2) {
        final OperatorEnum op1 = getOperator(node1);
        final OperatorEnum op2 = getOperator(node2);
        if (op1 == null || op2 == null) {
            // uncomparable results
            return 0;
        }
        return compareTo(op1, op2);
    }

    public static int compareTo(OperatorEnum operator1, OperatorEnum operator2) {
        final Integer prec1 = operator1.precedence;
        final Integer prec2 = operator2.precedence;
        // Reverse the precedence because of the values we are assigning
        return -prec1.compareTo(prec2);
    }

    public static OperatorEnum getOperator(ASTNode expr) {
        if (expr instanceof PrefixExpression) {
            return OPERATORS.get(((PrefixExpression) expr).getOperator());
        } else if (expr instanceof PostfixExpression) {
            return OPERATORS.get(((PostfixExpression) expr).getOperator());
        } else if (expr instanceof InfixExpression) {
            return OPERATORS.get(((InfixExpression) expr).getOperator());
        } else if (expr instanceof MethodInvocation
                || expr instanceof SuperMethodInvocation) {
            return PARENTHESES;
        } else if (expr instanceof Assignment) {
            return OPERATORS.get(((Assignment) expr).getOperator());
        } else if (expr instanceof VariableDeclarationFragment) {
            return ASSIGN;
        } else if (expr instanceof FieldAccess
                || expr instanceof SuperFieldAccess
                || expr instanceof ThisExpression) {
            return DOT;
        } else if (expr instanceof InstanceofExpression) {
            return INSTANCEOF;
        } else if (expr instanceof CastExpression) {
            return CAST;
        } else if (expr instanceof ClassInstanceCreation
                || expr instanceof ArrayCreation) {
            return NEW;
        } else if (expr instanceof ArrayAccess) {
            return ARRAY_ACCESS;
        } else if (expr instanceof ConditionalExpression) {
            return TERNARY;
        }
        return null;
    }
}
