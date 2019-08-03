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
package org.autorefactor.jdt.internal.ui.fix;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getTargetType;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.isMethod;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class UnboxingRatherThanExplicitMethodCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_UnboxingRatherThanExplicitMethodCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 5;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        if (node.getExpression() != null
                && (isMethod(node, Boolean.class.getCanonicalName(), "booleanValue") || isMethod(node, Byte.class.getCanonicalName(), "byteValue")
                        || isMethod(node, Character.class.getCanonicalName(), "charValue")
                        || isMethod(node, Short.class.getCanonicalName(), "shortValue")
                        || isMethod(node, Integer.class.getCanonicalName(), "intValue")
                        || isMethod(node, Long.class.getCanonicalName(), "longValue")
                        || isMethod(node, Float.class.getCanonicalName(), "floatValue")
                        || isMethod(node, Double.class.getCanonicalName(), "doubleValue"))) {
            final ITypeBinding actualResultType= getTargetType(node);

            if (actualResultType != null && actualResultType.isAssignmentCompatible(node.resolveTypeBinding())) {
                useUnboxing(node);
                return false;
            }
        }
        return true;
    }

    private void useUnboxing(final MethodInvocation node) {
        final ASTBuilder b= this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(node, b.copy(node.getExpression()));
    }
}
