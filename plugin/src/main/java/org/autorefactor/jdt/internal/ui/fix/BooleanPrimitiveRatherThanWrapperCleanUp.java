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

import static org.eclipse.jdt.core.dom.ASTNode.DO_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.WHILE_STATEMENT;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class BooleanPrimitiveRatherThanWrapperCleanUp extends AbstractPrimitiveRatherThanWrapperCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_BooleanPrimitiveRatherThanWrapperCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_BooleanPrimitiveRatherThanWrapperCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_BooleanPrimitiveRatherThanWrapperCleanUp_reason;
    }

    @Override
    public String getWrapperFullyQualifiedName() {
        return Boolean.class.getCanonicalName();
    }

    @Override
    public String getPrimitiveTypeName() {
        return boolean.class.getSimpleName();
    }

    @Override
    public Class<? extends Expression> getLiteralClass() {
        return BooleanLiteral.class;
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(PrefixExpression.Operator.NOT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixInSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(InfixExpression.Operator.AND, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.EQUALS, InfixExpression.Operator.GREATER,
                InfixExpression.Operator.GREATER_EQUALS, InfixExpression.Operator.LESS, InfixExpression.Operator.LESS_EQUALS, InfixExpression.Operator.NOT_EQUALS, InfixExpression.Operator.OR, InfixExpression.Operator.XOR);
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(PrefixExpression.Operator.NOT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixOutSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(InfixExpression.Operator.AND, InfixExpression.Operator.OR, InfixExpression.Operator.CONDITIONAL_AND, InfixExpression.Operator.CONDITIONAL_OR, InfixExpression.Operator.XOR);
    }

    @Override
    public List<Assignment.Operator> getAssignmentOutSafeOperators() {
        return Arrays.<Assignment.Operator>asList(Assignment.Operator.BIT_AND_ASSIGN, Assignment.Operator.BIT_OR_ASSIGN, Assignment.Operator.BIT_XOR_ASSIGN);
    }

    @Override
    public String[] getSafeInConstants() {
        return new String[] { "TRUE", "FALSE" }; //$NON-NLS-1$ //$NON-NLS-2$
    }

    @Override
    public boolean isSpecificPrimitiveAllowed(final ASTNode node) {
        ASTNode parentNode= node.getParent();

        switch (parentNode.getNodeType()) {
        case IF_STATEMENT:
        case WHILE_STATEMENT:
        case DO_STATEMENT:
            return true;

        default:
            return false;
        }
    }
}
