/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/**
 * See {@link #getDescription()} method.
 * <p>
 * TODO JNR use CFG and expression analysis to find extra information about
 * expression nullness.
 * </p>
 */
public class InvertEqualsCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_InvertEqualsCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_InvertEqualsCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_InvertEqualsCleanUp_reason;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return true;
        }
        boolean isEquals= ASTNodes.usesGivenSignature(node, Object.class.getCanonicalName(), "equals", Object.class.getCanonicalName()); //$NON-NLS-1$
        boolean isStringEqualsIgnoreCase= ASTNodes.usesGivenSignature(node, String.class.getCanonicalName(), "equalsIgnoreCase", String.class.getCanonicalName()); //$NON-NLS-1$
        if (isEquals || isStringEqualsIgnoreCase) {
            final Expression expression= node.getExpression();
            final Expression arg0= ASTNodes.arg0(node);
            if (!ASTNodes.isConstant(expression) && ASTNodes.isConstant(arg0) && !ASTNodes.isPrimitive(arg0)) {
                invertEqualsInvocation(node, isEquals, expression, arg0);
                return false;
            }
        }
        return true;
    }

    private void invertEqualsInvocation(final MethodInvocation node, final boolean isEquals, final Expression expression,
            final Expression arg0) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();

        final String methodName= isEquals ? "equals" : "equalsIgnoreCase"; //$NON-NLS-1$ $NON-NLS-2$
        this.ctx.getRefactorings().replace(node,
                b.invoke(b.parenthesizeIfNeeded(b.copy(arg0)), methodName, b.copy(expression)));
    }
}
