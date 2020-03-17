/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public final class RemoveUncheckedThrowsClausesCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUncheckedThrowsClausesCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUncheckedThrowsClausesCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUncheckedThrowsClausesCleanUp_reason;
    }

    @Override
    public boolean visit(final MethodDeclaration node) {
        Collection<ASTNode> nodesToRemove= getUncheckedExceptions(node);
        if (!nodesToRemove.isEmpty()) {
            for (ASTNode n : nodesToRemove) {
                cuRewrite.getASTRewrite().replace(n, null);
            }

            return false;
        }

        return true;
    }

    /**
     * Returns list of unchecked exception nodes in this method declaration (below
     * JLS8 API only).
     *
     * @exception UnsupportedOperationException if this operation is used in a JLS8
     *                                          or later AST In the JLS8 API,<br>
     *                                          this method is replaced by
     *                                          {@link MethodDeclaration#thrownExceptionTypes}.
     */
    private Collection<ASTNode> getUncheckedExceptions(final MethodDeclaration node) {
        List<ASTNode> result= new ArrayList<>();
        for (Type n : ASTNodes.thrownExceptionTypes(node)) {
            if (isUnchecked(n)) {
                result.add(n);
            }
        }

        return result;
    }

    private boolean isUnchecked(final Type type) {
        ITypeBinding binding= type.resolveBinding();
        return ASTNodes.instanceOf(binding, RuntimeException.class.getCanonicalName()) || ASTNodes.instanceOf(binding, Error.class.getCanonicalName());
    }
}
