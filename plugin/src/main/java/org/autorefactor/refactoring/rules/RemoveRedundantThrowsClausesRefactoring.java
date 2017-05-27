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

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Type;

/** See {@link #getDescription()} method. */
public final class RemoveRedundantThrowsClausesRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Remove unchecked exceptions from throws clause";
    }

    @Override
    public String getName() {
        return "Remove unchecked exceptions from throws clause";
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        boolean versionIs8orHigher = ctx.getAST().apiLevel() >= AST.JLS8;
        final List<ASTNode> nodesToRemove;
        if (versionIs8orHigher) {
            nodesToRemove = getForVersion8orHigher(node);
        } else {
            nodesToRemove = getForVersion7orLower(node);
        }
        if (!nodesToRemove.isEmpty()) {
            for (ASTNode n:nodesToRemove) {
                ctx.getRefactorings().replace(n, null);
            }
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private List<ASTNode> getForVersion8orHigher(MethodDeclaration node) {
        List<Type> exceptions = node.thrownExceptionTypes();
        List<ASTNode> result = new ArrayList<ASTNode>();
        for (Type t:exceptions) {
            if (instanceOf(t.resolveBinding(), "java.lang.RuntimeException")
                    || instanceOf(t.resolveBinding(), "java.lang.Error")) {
                result.add(t);
            }
        }
        return result;
    }

    private List<ASTNode> getForVersion7orLower(MethodDeclaration node) {
        // will not be invoked for version >= 8
        @SuppressWarnings({ "deprecation" })
        List<Name> exceptions = node.thrownExceptions();
        List<ASTNode> result = new ArrayList<ASTNode>();
        for (Name n:exceptions) {
            if (instanceOf(n, "java.lang.RuntimeException")
                    || instanceOf(n, "java.lang.Error")) {
                result.add(n);
            }
        }
        return result;
    }
}
