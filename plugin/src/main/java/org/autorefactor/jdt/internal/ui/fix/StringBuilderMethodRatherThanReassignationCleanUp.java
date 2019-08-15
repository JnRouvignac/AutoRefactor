/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;

/** See {@link #getDescription()} method. */
public class StringBuilderMethodRatherThanReassignationCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_StringBuilderMethodRatherThanReassignationCleanUp_reason;
    }

    @Override
    public boolean visit(Assignment node) {
        final Expression targetVar= node.getLeftHandSide();
        Expression var= node.getRightHandSide();
        if (Assignment.Operator.ASSIGN.equals(node.getOperator()) && ASTNodes.hasType(targetVar, StringBuffer.class.getCanonicalName(), StringBuilder.class.getCanonicalName())
                && var instanceof MethodInvocation) {
            var= getVar(var);

            if (ASTNodes.isSameVariable(targetVar, var)) {
                final ASTNodeFactory b= this.ctx.getASTBuilder();
                ctx.getRefactorings().replace(node, b.copy(node.getRightHandSide()));
                return false;
            }
        }
        return true;
    }

    private Expression getVar(final Expression var) {
        final MethodInvocation mi= ASTNodes.as(var, MethodInvocation.class);
        if (var instanceof Name) {
            return var;
        } else if (mi != null && ASTNodes.hasType(mi.getExpression(), StringBuffer.class.getCanonicalName(), StringBuilder.class.getCanonicalName())
                && Arrays.asList("append", "appendCodePoint", "delete", "deleteCharAt", "insert", "replace", "reverse") //$NON-NLS-1$ $NON-NLS-2$ $NON-NLS-3$ $NON-NLS-4$ $NON-NLS-5$ $NON-NLS-6$ $NON-NLS-7$
                        .contains(mi.getName().getIdentifier())) {
            return getVar(mi.getExpression());
        }
        return null;
    }
}
