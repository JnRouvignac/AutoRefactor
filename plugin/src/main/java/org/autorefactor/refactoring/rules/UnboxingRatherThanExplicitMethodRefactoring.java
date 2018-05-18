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
import static org.autorefactor.refactoring.ASTHelper.getDestinationType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class UnboxingRatherThanExplicitMethodRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Unboxing rather than explicit method";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Remove useless primitiveValue() call to use unboxing.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "UnBoxing methods are automatically added by the compiler so it is useless. "
                + "It improves the readibility. It also upgrades legacy code.";
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if ((node.getExpression() != null)
                && getJavaMinorVersion() >= 5
                && (isMethod(node, "java.lang.Boolean", "booleanValue")
                || isMethod(node, "java.lang.Byte", "byteValue")
                || isMethod(node, "java.lang.Character", "charValue")
                || isMethod(node, "java.lang.Short", "shortValue")
                || isMethod(node, "java.lang.Integer", "intValue")
                || isMethod(node, "java.lang.Long", "longValue")
                || isMethod(node, "java.lang.Float", "floatValue")
                || isMethod(node, "java.lang.Double", "doubleValue"))) {
            final ITypeBinding actualResultType = getDestinationType(node);

            if (actualResultType != null && actualResultType.isAssignmentCompatible(node.resolveTypeBinding())) {
                useUnboxing(node);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private void useUnboxing(final MethodInvocation node) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node, b.copy(node.getExpression()));
    }
}
