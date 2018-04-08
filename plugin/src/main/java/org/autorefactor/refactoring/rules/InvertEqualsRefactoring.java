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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.arg0;
import static org.autorefactor.refactoring.ASTHelper.isConstant;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.isPrimitive;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MethodInvocation;

/**
 * See {@link #getDescription()} method.
 * <p>
 * TODO JNR use CFG and expression analysis to find extra information about expression nullness.
 * </p>
 */
public class InvertEqualsRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Equals on constant rather than on variable";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Inverts calls to Object.equals(Object) and String.equalsIgnoreCase(String)"
            + " when it is known that the second operand is not null and the first can be null.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It avoids null pointer.";
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() == null) {
            return VISIT_SUBTREE;
        }
        boolean isEquals = isMethod(node, "java.lang.Object", "equals", "java.lang.Object");
        boolean isStringEqualsIgnoreCase =
                isMethod(node, "java.lang.String", "equalsIgnoreCase", "java.lang.String");
        if (isEquals || isStringEqualsIgnoreCase) {
            final Expression expr = node.getExpression();
            final Expression arg0 = arg0(node);
            if (!isConstant(expr) && isConstant(arg0) && !isPrimitive(arg0)) {
                invertEqualsInvocation(node, isEquals, expr, arg0);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private void invertEqualsInvocation(final MethodInvocation node, final boolean isEquals, final Expression expr,
            final Expression arg0) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final String methodName = isEquals ? "equals" : "equalsIgnoreCase";
        this.ctx.getRefactorings().replace(node,
                b.invoke(b.parenthesizeIfNeeded(b.copy(arg0)), methodName, b.copy(expr)));
    }
}
