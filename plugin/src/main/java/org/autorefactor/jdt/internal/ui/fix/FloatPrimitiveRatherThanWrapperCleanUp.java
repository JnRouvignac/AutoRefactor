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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.PostfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class FloatPrimitiveRatherThanWrapperCleanUp extends AbstractPrimitiveRatherThanWrapperCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_FloatPrimitiveRatherThanWrapperCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_FloatPrimitiveRatherThanWrapperCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_FloatPrimitiveRatherThanWrapperCleanUp_reason;
    }

    @Override
    public String getWrapperFullyQualifiedName() {
        return Float.class.getCanonicalName();
    }

    @Override
    public String getPrimitiveTypeName() {
        return float.class.getSimpleName();
    }

    @Override
    public Class<? extends Expression> getLiteralClass() {
        return NumberLiteral.class;
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.MINUS, PrefixExpression.Operator.DECREMENT,
                PrefixExpression.Operator.PLUS, PrefixExpression.Operator.COMPLEMENT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixInSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(InfixExpression.Operator.DIVIDE, InfixExpression.Operator.MINUS,
                InfixExpression.Operator.PLUS, InfixExpression.Operator.REMAINDER, InfixExpression.Operator.TIMES);
    }

    @Override
    public List<PostfixExpression.Operator> getPostfixInSafeOperators() {
        return Arrays.<PostfixExpression.Operator>asList(PostfixExpression.Operator.INCREMENT,
                PostfixExpression.Operator.DECREMENT);
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(PrefixExpression.Operator.INCREMENT, PrefixExpression.Operator.MINUS, PrefixExpression.Operator.DECREMENT,
                PrefixExpression.Operator.PLUS, PrefixExpression.Operator.COMPLEMENT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixOutSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(InfixExpression.Operator.DIVIDE,
                InfixExpression.Operator.GREATER, InfixExpression.Operator.GREATER_EQUALS,
                InfixExpression.Operator.LESS, InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.MINUS,
                InfixExpression.Operator.PLUS, InfixExpression.Operator.REMAINDER, InfixExpression.Operator.TIMES);
    }

    @Override
    public List<PostfixExpression.Operator> getPostfixOutSafeOperators() {
        return Arrays.<PostfixExpression.Operator>asList(PostfixExpression.Operator.INCREMENT,
                PostfixExpression.Operator.DECREMENT);
    }

    @Override
    public List<Assignment.Operator> getAssignmentOutSafeOperators() {
        return Arrays.<Assignment.Operator>asList(Assignment.Operator.PLUS_ASSIGN, Assignment.Operator.MINUS_ASSIGN, Assignment.Operator.TIMES_ASSIGN, Assignment.Operator.DIVIDE_ASSIGN,
                Assignment.Operator.REMAINDER_ASSIGN);
    }

    @Override
    public String[] getSafeInConstants() {
        return new String[] { "MIN_VALUE", "MAX_VALUE" }; //$NON-NLS-1$ //$NON-NLS-2$
    }
}
