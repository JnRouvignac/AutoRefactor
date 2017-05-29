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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.instanceOf;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.preferences.Preferences;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Name;

/** See {@link #getDescription()} method. */
public final class RemoveUncheckedThrowsClausesRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Remove unchecked exceptions from throws clause";
    }

    @Override
    public String getName() {
        return "Remove unchecked exceptions from throws clause";
    }

    @Override
    public boolean isEnabled(Preferences preferences) {
        // TODO: remove check for java8 implementation
        return super.isEnabled(preferences) && apiLevel7orLower();
    }

    private boolean apiLevel7orLower() {
        return ctx.getAST().apiLevel() <= AST.JLS4;
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        if (apiLevel7orLower()) {
            List<ASTNode> nodesToRemove = getUncheckedExceptions(node);
            if (!nodesToRemove.isEmpty()) {
                for (ASTNode n:nodesToRemove) {
                    ctx.getRefactorings().replace(n, null);
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    /**
     * Returns list of unchecked exception nodes in this method declaration (below JLS8 API only).
     *
     * @exception UnsupportedOperationException
     *                if this operation is used in a JLS8 or later AST In the JLS8 API,<br>
     *                this method is replaced by {@link MethodDeclaration#thrownExceptionTypes}.
     */
    private List<ASTNode> getUncheckedExceptions(MethodDeclaration node) {
        List<ASTNode> result = new ArrayList<ASTNode>();
        // TODO: add a check for api level and invoke corresponding method
        List<Name> exceptions = node.thrownExceptions();
        for (Name n:exceptions) {
            if (isUnchecked(n)) {
                result.add(n);
            }
        }
        return result;
    }

    private boolean isUnchecked(Name n) {
        return instanceOf(n, "java.lang.RuntimeException")
                || instanceOf(n, "java.lang.Error");
    }
}
