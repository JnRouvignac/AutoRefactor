/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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

import java.util.Map;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.MethodInvocation;

/** See {@link #getDescription()} method. */
public class MethodOnMapRatherThanMethodOnKeySetCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_MethodOnMapRatherThanMethodOnKeySetCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_MethodOnMapRatherThanMethodOnKeySetCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_MethodOnMapRatherThanMethodOnKeySetCleanUp_reason;
    }

    @Override
    public boolean visit(MethodInvocation mi) {
        MethodInvocation miExpression= ASTNodes.as(mi.getExpression(), MethodInvocation.class);

        if (miExpression != null && ASTNodes.usesGivenSignature(miExpression, Map.class.getCanonicalName(), "keySet")) { //$NON-NLS-1$
            if (ASTNodes.usesGivenSignature(mi, Set.class.getCanonicalName(), "clear")) { //$NON-NLS-1$
                return removeInvocationOfMapKeySet(miExpression, mi, "clear"); //$NON-NLS-1$
            }

            if (ASTNodes.usesGivenSignature(mi, Set.class.getCanonicalName(), "size")) { //$NON-NLS-1$
                return removeInvocationOfMapKeySet(miExpression, mi, "size"); //$NON-NLS-1$
            }

            if (ASTNodes.usesGivenSignature(mi, Set.class.getCanonicalName(), "isEmpty")) { //$NON-NLS-1$
                return removeInvocationOfMapKeySet(miExpression, mi, "isEmpty"); //$NON-NLS-1$
            }

            if (ASTNodes.usesGivenSignature(mi, Set.class.getCanonicalName(), "remove", Object.class.getCanonicalName()) //$NON-NLS-1$
                    // If parent is not an expression statement, the MethodInvocation must return a
                    // boolean.
                    // In that case, we cannot replace because `Map.removeKey(key) != null`
                    // is not strictly equivalent to `Map.keySet().remove(key)`
                    && mi.getParent().getNodeType() == ASTNode.EXPRESSION_STATEMENT) {
                return removeInvocationOfMapKeySet(miExpression, mi, "remove"); //$NON-NLS-1$
            }

            if (ASTNodes.usesGivenSignature(mi, Set.class.getCanonicalName(), "contains", Object.class.getCanonicalName())) { //$NON-NLS-1$
                return removeInvocationOfMapKeySet(miExpression, mi, "containsKey"); //$NON-NLS-1$
            }
        }

        return true;
    }

    private boolean removeInvocationOfMapKeySet(MethodInvocation mapKeySetMi, MethodInvocation actualMi,
            String methodName) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        ctx.getRefactorings().replace(actualMi,
                b.invoke(b.copyExpression(mapKeySetMi), methodName, b.copyRange(ASTNodes.arguments(actualMi))));
        return false;
    }
}
