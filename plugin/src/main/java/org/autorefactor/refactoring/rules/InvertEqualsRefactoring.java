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

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Inverts calls to {@link Object#equals(Object)} and
 * {@link String#equalsIgnoreCase(String)} when it is known that the second
 * operand is not null and the first can be null.
 * <p>
 * TODO JNR use CFG and expression analysis to find extra information about
 * expression nullness.
 * </p>
 */
public class InvertEqualsRefactoring extends AbstractRefactoringRule {

    /** {@inheritDoc} */
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
            final Expression arg0 = arguments(node).get(0);
            if (!isConstant(expr) && isConstant(arg0)) {
                this.ctx.getRefactorings().replace(node,
                        invertEqualsInvocation(expr, arg0, isEquals));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean isConstant(final Expression expr) {
        return (expr != null && expr.resolveConstantExpressionValue() != null)
                || isEnumConstant(expr);
    }

    private boolean isEnumConstant(final Expression expr) {
        // TODO JNR make it work for enums fields which are static final, but not null
        if (expr instanceof Name) {
            final IBinding binding = ((Name) expr).resolveBinding();
            if (binding instanceof IVariableBinding) {
                return ((IVariableBinding) binding).isEnumConstant();
            }
        }
        return false;
    }

    private ASTNode invertEqualsInvocation(Expression lhs, Expression rhs, boolean isEquals) {
        final String methodName = isEquals ? "equals" : "equalsIgnoreCase";
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.invoke(b.parenthesizeIfNeeded(b.copy(rhs)), methodName, b.copy(lhs));
    }
}
