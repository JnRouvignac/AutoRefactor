/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice TIERCELIN - initial API and implementation
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

import static org.eclipse.jdt.core.dom.Assignment.Operator.DIVIDE_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.MINUS_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.PLUS_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.REMAINDER_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.TIMES_ASSIGN;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.COMPLEMENT;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.DECREMENT;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.INCREMENT;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class DoublePrimitiveRatherThanWrapperRefactoring extends AbstractPrimitiveRatherThanWrapperRefactoring {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Double primitive rather than wrapper";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace Double wrapper object by double primitive type when an object is not necessary.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility and reduces null pointer check."
                + " It also improves the space performance.";
    }

    @Override
    public String getWrapperFullyQualifiedName() {
        return "java.lang.Double";
    }

    @Override
    public String getPrimitiveTypeName() {
        return "double";
    }

    @Override
    public Class<? extends Expression> getLiteralClass() {
        return NumberLiteral.class;
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(INCREMENT, PrefixExpression.Operator.MINUS, DECREMENT,
                PrefixExpression.Operator.PLUS, COMPLEMENT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixInSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(InfixExpression.Operator.DIVIDE,
                InfixExpression.Operator.LEFT_SHIFT,
                InfixExpression.Operator.MINUS,
                InfixExpression.Operator.PLUS,
                InfixExpression.Operator.REMAINDER,
                InfixExpression.Operator.RIGHT_SHIFT_SIGNED,
                InfixExpression.Operator.RIGHT_SHIFT_UNSIGNED,
                InfixExpression.Operator.TIMES);
    }

    @Override
    public List<PostfixExpression.Operator> getPostfixInSafeOperators() {
        return Arrays.<PostfixExpression.Operator>asList(
                PostfixExpression.Operator.INCREMENT,
                PostfixExpression.Operator.DECREMENT);
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(INCREMENT, PrefixExpression.Operator.MINUS, DECREMENT,
                PrefixExpression.Operator.PLUS,
                COMPLEMENT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixOutSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(InfixExpression.Operator.DIVIDE,
                InfixExpression.Operator.GREATER,
                InfixExpression.Operator.GREATER_EQUALS,
                InfixExpression.Operator.LESS,
                InfixExpression.Operator.LESS_EQUALS,
                InfixExpression.Operator.MINUS,
                InfixExpression.Operator.PLUS,
                InfixExpression.Operator.REMAINDER,
                InfixExpression.Operator.TIMES);
    }

    @Override
    public List<PostfixExpression.Operator> getPostfixOutSafeOperators() {
        return Arrays.<PostfixExpression.Operator>asList(
                PostfixExpression.Operator.INCREMENT,
                PostfixExpression.Operator.DECREMENT);
    }

    @Override
    public List<Assignment.Operator> getAssignmentOutSafeOperators() {
        return Arrays.<Assignment.Operator>asList(PLUS_ASSIGN,
                MINUS_ASSIGN,
                TIMES_ASSIGN,
                DIVIDE_ASSIGN,
                REMAINDER_ASSIGN);
    }

    @Override
    public String[] getSafeInConstants() {
        return new String[]{"MIN_VALUE", "MAX_VALUE"};
    }
}
