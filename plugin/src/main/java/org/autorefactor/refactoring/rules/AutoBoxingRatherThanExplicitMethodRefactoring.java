/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.getDestinationType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class AutoBoxingRatherThanExplicitMethodRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "AutoBoxing rather than explicit method";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Remove useless valueOf() call to use AutoBoxing.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "AutoBoxing methods are automatically added by the compiler so it is useless. "
                + "It improves the readibility. It also upgrades legacy code.";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if ("valueOf".equals(node.getName().getIdentifier())
                && (node.getExpression() != null)
                && getJavaMinorVersion() >= 5
                && (isMethod(node, "java.lang.Boolean", "valueOf", "boolean")
                || isMethod(node, "java.lang.Byte", "valueOf", "byte")
                || isMethod(node, "java.lang.Character", "valueOf", "char")
                || isMethod(node, "java.lang.Short", "valueOf", "short")
                || isMethod(node, "java.lang.Integer", "valueOf", "int")
                || isMethod(node, "java.lang.Long", "valueOf", "long")
                || isMethod(node, "java.lang.Float", "valueOf", "float")
                || isMethod(node, "java.lang.Double", "valueOf", "double"))) {
            final ITypeBinding primitiveType = node.resolveMethodBinding().getParameterTypes()[0];
            final ITypeBinding wrapperClass = node.resolveMethodBinding().getDeclaringClass();

            final ITypeBinding actualResultType = getDestinationType(node);
            final ITypeBinding actualParameterType = arg0(node).resolveTypeBinding();

            if ((actualResultType != null && (actualResultType.equals(primitiveType)
                    || actualResultType.equals(wrapperClass)))
                    || (actualParameterType != null && actualParameterType.equals(wrapperClass))) {
                useAutoBoxing(node, primitiveType, wrapperClass, actualParameterType, actualResultType);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private void useAutoBoxing(final MethodInvocation node, final ITypeBinding primitiveType,
            final ITypeBinding wrapperClass, final ITypeBinding actualParameterType,
            final ITypeBinding actualResultType) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        if (primitiveType != null
                && !primitiveType.equals(actualParameterType) && !primitiveType.equals(actualResultType)
                && (wrapperClass == null || !wrapperClass.equals(actualParameterType))) {
            this.ctx.getRefactorings().replace(node, b.cast(b.type(primitiveType.getName()), b.copy(arg0(node))));
        } else {
            this.ctx.getRefactorings().replace(node, b.copy(arg0(node)));
        }
    }
}
