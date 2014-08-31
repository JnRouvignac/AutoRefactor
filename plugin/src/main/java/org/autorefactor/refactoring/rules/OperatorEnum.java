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

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/**
 * Enum listing all the Java operators in the operator precedence order.
 * It also contains additional information about the operators.
 *
 * @see <a href="http://introcs.cs.princeton.edu/java/11precedence/"
 *      >Introduction to programming in Java - Appendix A: Operator Precedence in Java</a>
 * @see <a href="http://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html"
 *      >Operators (The Java&trade; Tutorials &gt; Learning the Java Language &gt; Language Basics)</a>
 * @see <a href="http://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html"
 *      >Java tutorial - Operators</a>
 */
public enum OperatorEnum {

    /** The dot operator '.'. For example <code>i.fieldName</code>. */
    DOT                        (null,                                             1, false, false),
    /** The parentheses operator '()'. For example <code>f()</code>. */
    PARENTHESES                (null,                                             1, false, false),
    /** The array access operator '[]'. For example <code>a[]</code>. */
    ARRAY_ACCESS               (null,                                             1, false, false),

    /** The postfix decrement operator '--'. For example <code>i--</code>. */
    POSTFIX_DECREMENT          (PostfixExpression.Operator.DECREMENT,             2, false, false),
    /** The postfix increment operator '++'. For example <code>i++</code>. */
    POSTFIX_INCREMENT          (PostfixExpression.Operator.INCREMENT,             2, false, false),

    /** The prefix decrement operator '--'. For example <code>--i</code>. */
    PREFIX_DECREMENT           (PrefixExpression.Operator.DECREMENT,              3, false, false),
    /** The prefix increment operator '++'. For example <code>++i</code>. */
    PREFIX_INCREMENT           (PrefixExpression.Operator.INCREMENT,              3, false, false),
    /** The prefix minus operator '-'. For example <code>-i</code>. */
    PREFIX_MINUS               (PrefixExpression.Operator.MINUS,                  3, false, false),
    /** The prefix plus operator '+'. For example <code>+i</code>. */
    PREFIX_PLUS                (PrefixExpression.Operator.PLUS,                   3, false, false),
    /** The prefix bitwise complement operator '~'. For example <code>~i</code>. */
    PREFIX_COMPLEMENT          (PrefixExpression.Operator.COMPLEMENT,             3, false, false),
    /** The prefix boolean not operator '!'. For example <code>!i</code>. */
    PREFIX_NOT                 (PrefixExpression.Operator.NOT,                    3, false, false),

    /** The cast operator '(type)'. For example <code>(int) l</code>. */
    CAST                       (null,                                             4,  true, false),
    /** The new operator 'new'. For example <code>new Object()</code>. */
    NEW                        (null,                                             4, false, false),

    /** The infix times operator '*'. For example <code>1 * 2</code>. */
    INFIX_TIMES                (InfixExpression.Operator.TIMES,                   5,  true, false),
    /** The infix divide operator '/'. For example <code>1 / 2</code>. */
    INFIX_DIVIDE               (InfixExpression.Operator.DIVIDE,                  5, false, false),
    /** The infix remainder operator '%', also known as modulo. For example <code>1 % 2</code>. */
    INFIX_REMAINDER            (InfixExpression.Operator.REMAINDER,               5, false, false),

    /** The infix minus operator '-'. For example <code>1 - 2</code>. */
    INFIX_MINUS                (InfixExpression.Operator.MINUS,                   6, false, false),
    /** The infix minus operator '+'. For example <code>1 + 2</code>. */
    INFIX_PLUS                 (InfixExpression.Operator.PLUS,                    6,  true, false),

    /** The infix left shift operator '<<'. For example <code>1 << 2</code>. */
    INFIX_LEFT_SHIFT           (InfixExpression.Operator.LEFT_SHIFT,              7, false, false),
    /** The infix signed right shift operator '>>'. For example <code>1 >> 2</code>. */
    INFIX_RIGHT_SHIFT_SIGNED   (InfixExpression.Operator.RIGHT_SHIFT_SIGNED,      7, false, false),
    /** The infix unsigned right shift operator '>>>'. For example <code>1 >>> 2</code>. */
    INFIX_RIGHT_SHIFT_UNSIGNED (InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED,    7, false, false),

    /** The infix less than operator '<'. For example <code>1 < 2</code>. */
    INFIX_LESS                 (InfixExpression.Operator.LESS,                    8, false,
                                InfixExpression.Operator.GREATER_EQUALS),
    /** The infix less than or equals operator '<='. For example <code>1 <= 2</code>. */
    INFIX_LESS_EQUALS          (InfixExpression.Operator.LESS_EQUALS,             8, false,
                                InfixExpression.Operator.GREATER),
    /** The infix greater than operator '>'. For example <code>1 > 2</code>. */
    INFIX_GREATER              (InfixExpression.Operator.GREATER,                 8, false,
                                InfixExpression.Operator.LESS_EQUALS),
    /** The infix greater than or equals operator '>='. For example <code>1 >= 2</code>. */
    INFIX_GREATER_EQUALS       (InfixExpression.Operator.GREATER_EQUALS,          8, false,
                                InfixExpression.Operator.LESS),
    /** The instanceof operator 'instanceof'. For example <code>o instanceof String</code>. */
    INSTANCEOF                 (null,                                             8, false,  true),

    /** The infix equals operator '=='. For example <code>1 == 2</code>. */
    INFIX_EQUALS               (InfixExpression.Operator.EQUALS,                  9, false,
                                InfixExpression.Operator.NOT_EQUALS),
    /** The infix not equals operator '=='. For example <code>1 != 2</code>. */
    INFIX_NOT_EQUALS           (InfixExpression.Operator.NOT_EQUALS,              9, false,
                                InfixExpression.Operator.EQUALS),

    /** The infix bitwise and operator '&'. For example <code>1 & 2</code>. */
    INFIX_BIT_AND              (InfixExpression.Operator.AND,                    10,  true, false),

    /** The infix bitwise xor operator '^'. For example <code>1 ^ 2</code>. */
    INFIX_BIT_XOR              (InfixExpression.Operator.XOR,                    11,  true, false),

    /** The infix bitwise or operator '|'. For example <code>1 | 2</code>. */
    INFIX_BIT_OR               (InfixExpression.Operator.OR,                     12,  true, false),

    /** The infix conditional and operator '&&'. For example <code>true && false</code>. */
    INFIX_CONDITIONAL_AND      (InfixExpression.Operator.CONDITIONAL_AND,        13,  true,
                                InfixExpression.Operator.CONDITIONAL_OR),

    /** The infix conditional or operator '||'. For example <code>true || false</code>. */
    INFIX_CONDITIONAL_OR       (InfixExpression.Operator.CONDITIONAL_OR,         14,  true,
                                InfixExpression.Operator.CONDITIONAL_AND),

    /** The ternary operator '?:'. For example <code>b ? 1 : 2</code>. */
    TERNARY                    (null,                                            15, false, false),

    /** The assign operator '='. For example <code>i = 1</code>. */
    ASSIGN                     (Assignment.Operator.ASSIGN,                      16, false, false),
    /** The plus assign operator '+='. For example <code>i += 1</code>. */
    PLUS_ASSIGN                (Assignment.Operator.PLUS_ASSIGN,                 16, false, false),
    /** The minus assign operator '-='. For example <code>i -= 1</code>. */
    MINUS_ASSIGN               (Assignment.Operator.MINUS_ASSIGN,                16, false, false),
    /** The times assign operator '*='. For example <code>i *= 1</code>. */
    TIMES_ASSIGN               (Assignment.Operator.TIMES_ASSIGN,                16, false, false),
    /** The divide assign operator '/='. For example <code>i /= 1</code>. */
    DIVIDE_ASSIGN              (Assignment.Operator.DIVIDE_ASSIGN,               16, false, false),
    /** The remainder assign operator '%='. For example <code>i %= 1</code>. */
    REMAINDER_ASSIGN           (Assignment.Operator.REMAINDER_ASSIGN,            16, false, false),
    /** The bitwise and assign operator '&='. For example <code>i &= 1</code>. */
    BIT_AND_ASSIGN             (Assignment.Operator.BIT_AND_ASSIGN,              16, false, false),
    /** The bitwise or assign operator '|='. For example <code>i |= 1</code>. */
    BIT_OR_ASSIGN              (Assignment.Operator.BIT_OR_ASSIGN,               16, false, false),
    /** The bitwise xor assign operator '^='. For example <code>i ^= 1</code>. */
    BIT_XOR_ASSIGN             (Assignment.Operator.BIT_XOR_ASSIGN,              16, false, false),
    /** The left shift assign operator '<<='. For example <code>i <<= 1</code>. */
    LEFT_SHIFT_ASSIGN          (Assignment.Operator.LEFT_SHIFT_ASSIGN,           16, false, false),
    /** The right signed shift assign operator '>>='. For example <code>i >>= 1</code>. */
    RIGHT_SHIFT_SIGNED_ASSIGN  (Assignment.Operator.RIGHT_SHIFT_SIGNED_ASSIGN,   16, false, false),
    /** The right unsigned shift assign operator '>>>='. For example <code>i >>>= 1</code>. */
    RIGHT_SHIFT_UNSIGNED_ASSIGN(Assignment.Operator.RIGHT_SHIFT_UNSIGNED_ASSIGN, 16, false, false),

    /** The comma operator ','. For example <code>i++, j++</code>. */
    COMMA                      (null,                                            17, false, false);


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
    private final Object reverseBooleanOperator;

    private OperatorEnum(Object operator, int precedence, boolean isAssociative, boolean isBoolean) {
        this.operator = operator;
        this.precedence = precedence;
        this.isAssociative = isAssociative;
        this.isBoolean = isBoolean;
        this.reverseBooleanOperator = null;
    }

    private OperatorEnum(Object operator, int precedence, boolean isAssociative, Object reverseBooleanOperator) {
        this.operator = operator;
        this.precedence = precedence;
        this.isAssociative = isAssociative;
        this.isBoolean = true;
        this.reverseBooleanOperator = reverseBooleanOperator;
    }

    /**
     * Returns the operator precedence as an integer representing the priority. Lower means higher priority.
     *
     * @return the operator precedence
     */
    public int getPrecedence() {
        return precedence;
    }

    /**
     * Returns whether the provided operator is associative.
     *
     * @param operator the operator
     * @return true if the supplied operator is associative, false otherwise
     */
    public static boolean isAssociative(InfixExpression.Operator operator) {
        return OPERATORS.get(operator).isAssociative;
    }

    /**
     * Returns whether the provided operator evaluates to a boolean value.
     *
     * @param operator the operator
     * @return true if the supplied operator evaluates to a boolean value, false otherwise
     */
    public static boolean isBoolean(InfixExpression.Operator operator) {
        return OPERATORS.get(operator).isBoolean;
    }

    /**
     * Returns the reverse boolean operator if it exists.
     *
     * @return the reverse boolean operator if it exists, false if none exist
     */
    public Object getReverseBooleanOperator() {
        return reverseBooleanOperator;
    }

    /**
     * Compares the provided nodes according to the Java operator precedence rules.
     *
     * @param node1 the first node to compare
     * @param node2 the second node to compare
     * @return a value smaller than 0 if the first node is less than the second node,
     *         0 if the two nodes are equal or cannot be compared,
     *         a value greater than 0 if the first node is greater than the second node
     */
    public static int compareTo(ASTNode node1, ASTNode node2) {
        final OperatorEnum op1 = getOperator(node1);
        final OperatorEnum op2 = getOperator(node2);
        if (op1 == null || op2 == null) {
            // uncomparable results
            return 0;
        }
        return compareTo(op1, op2);
    }

    /**
     * Compares the provided OperatorEnums according to the Java operator precedence rules.
     *
     * @param operator1 the first operator to compare
     * @param operator2 the second operator to compare
     * @return a value smaller than 0 if the first operator is less than the second node,
     *         0 if the two operators are equal or cannot be compared,
     *         a value greater than 0 if the first operator is greater than the second node
     */
    public static int compareTo(OperatorEnum operator1, OperatorEnum operator2) {
        final Integer prec1 = operator1.precedence;
        final Integer prec2 = operator2.precedence;
        // Reverse the precedence because of the values we are assigning
        return -prec1.compareTo(prec2);
    }

    /**
     * Returns the equivalent {@link OperatorEnum} for the operator used inside
     * the supplied node. Although this method accepts an {@link ASTNode}, the
     * provided node must be an expression.
     *
     * @param expr
     *            the expression node
     * @return the equivalent {@link OperatorEnum} for the operator used inside
     *         the supplied node, null if the supplied node is not an expression
     *         or if it does not use an operator
     */
    public static OperatorEnum getOperator(ASTNode expr) {
        if (expr == null) {
            return null;
        }
        switch (expr.getNodeType()) {
        case ASTNode.PREFIX_EXPRESSION:
            return OPERATORS.get(((PrefixExpression) expr).getOperator());
        case ASTNode.POSTFIX_EXPRESSION:
            return OPERATORS.get(((PostfixExpression) expr).getOperator());
        case ASTNode.INFIX_EXPRESSION:
            return OPERATORS.get(((InfixExpression) expr).getOperator());
        case ASTNode.METHOD_INVOCATION:
        case ASTNode.SUPER_METHOD_INVOCATION:
            return PARENTHESES;
        case ASTNode.ASSIGNMENT:
            return OPERATORS.get(((Assignment) expr).getOperator());
        case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
            return ASSIGN;
        case ASTNode.FIELD_ACCESS:
        case ASTNode.SUPER_FIELD_ACCESS:
        case ASTNode.THIS_EXPRESSION:
            return DOT;
        case ASTNode.INSTANCEOF_EXPRESSION:
            return INSTANCEOF;
        case ASTNode.CAST_EXPRESSION:
            return CAST;
        case ASTNode.CLASS_INSTANCE_CREATION:
        case ASTNode.ARRAY_CREATION:
            return NEW;
        case ASTNode.ARRAY_ACCESS:
            return ARRAY_ACCESS;
        case ASTNode.CONDITIONAL_EXPRESSION:
            return TERNARY;
        default:
            return null;
        }
    }
}
