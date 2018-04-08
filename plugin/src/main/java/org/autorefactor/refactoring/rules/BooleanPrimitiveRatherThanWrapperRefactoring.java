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

import static org.eclipse.jdt.core.dom.ASTNode.DO_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.IF_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.WHILE_STATEMENT;
import static org.eclipse.jdt.core.dom.Assignment.Operator.BIT_AND_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.BIT_OR_ASSIGN;
import static org.eclipse.jdt.core.dom.Assignment.Operator.BIT_XOR_ASSIGN;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_AND;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.CONDITIONAL_OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.GREATER_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.LESS_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.NOT_EQUALS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.OR;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.XOR;
import static org.eclipse.jdt.core.dom.PrefixExpression.Operator.NOT;

import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression;

/** See {@link #getDescription()} method. */
public class BooleanPrimitiveRatherThanWrapperRefactoring extends AbstractPrimitiveRatherThanWrapperRefactoring {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Boolean primitive rather than wrapper";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace Boolean wrapper object by boolean primitive type when an object is not necessary.";
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
        return "java.lang.Boolean";
    }

    @Override
    public String getPrimitiveTypeName() {
        return "boolean";
    }

    @Override
    public Class<? extends Expression> getLiteralClass() {
        return BooleanLiteral.class;
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixInSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(NOT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixInSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(AND,
                CONDITIONAL_AND,
                CONDITIONAL_OR,
                EQUALS,
                GREATER,
                GREATER_EQUALS,
                LESS,
                LESS_EQUALS,
                NOT_EQUALS,
                OR,
                XOR);
    }

    @Override
    public List<PrefixExpression.Operator> getPrefixOutSafeOperators() {
        return Arrays.<PrefixExpression.Operator>asList(NOT);
    }

    @Override
    public List<InfixExpression.Operator> getInfixOutSafeOperators() {
        return Arrays.<InfixExpression.Operator>asList(AND, OR, CONDITIONAL_AND, CONDITIONAL_OR, XOR);
    }

    @Override
    public List<Assignment.Operator> getAssignmentOutSafeOperators() {
        return Arrays.<Assignment.Operator>asList(BIT_AND_ASSIGN, BIT_OR_ASSIGN, BIT_XOR_ASSIGN);
    }

    @Override
    public String[] getSafeInConstants() {
        return new String[]{"TRUE", "FALSE"};
    }

    @Override
    public boolean isSpecificPrimitiveAllowed(final ASTNode node) {
        final ASTNode parentNode = node.getParent();

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
