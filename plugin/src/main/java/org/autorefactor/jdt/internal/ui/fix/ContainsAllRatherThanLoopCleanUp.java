/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice TIERCELIN - initial API and implementation
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

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.arg0;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.as;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isSameVariable;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.match;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.removeParentheses;

import java.util.Collection;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.PrefixExpression.Operator;

/** See {@link #getDescription()} method. */
public class ContainsAllRatherThanLoopCleanUp extends AbstractCollectionMethodRatherThanLoopCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_ContainsAllRatherThanLoopCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_ContainsAllRatherThanLoopCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_ContainsRatherThanLoopCleanUp_reason;
    }

    @Override
    protected Expression getExpressionToFind(MethodInvocation cond, Expression forVar) {
        Expression expr= removeParentheses(cond.getExpression());
        Expression arg0= removeParentheses(arg0(cond));

        if (isSameVariable(forVar, arg0) || match(forVar, arg0)) {
            return expr;
        }

        return null;
    }

    @Override
    protected MethodInvocation getMethodToReplace(Expression condition) {
        PrefixExpression negation= as(condition, PrefixExpression.class);

        if (negation == null || !Operator.NOT.equals(negation.getOperator())) {
            return null;
        }

        MethodInvocation method= as(negation.getOperand(), MethodInvocation.class);

        if (!isMethod(method, Collection.class.getCanonicalName(), "contains", Object.class.getCanonicalName())) { //$NON-NLS-1$
            return null;
        }

        return method;
    }

    @Override
    protected Expression newMethod(Expression iterable, Expression toFind, boolean isPositive, ASTBuilder b) {
        final MethodInvocation invoke= b.invoke(b.move(toFind), "containsAll", b.move(iterable)); //$NON-NLS-1$

        if (isPositive) {
            return b.not(invoke);
        }

        return invoke;
    }
}
