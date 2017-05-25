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

import org.eclipse.jdt.core.dom.EmptyStatement;
import org.eclipse.jdt.core.dom.SuperConstructorInvocation;

/** See {@link #getDescription()} method. */
public class RemoveEmptySuperConstrInvocationRefactoring extends
        AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Remove call to super constructor with empty arguments "
                + "since it is redundant. See JLS section 12.5 for more info.";
    }

    @Override
    public String getName() {
        return "Remove super() call in constructor";
    }

    @Override
    public boolean visit(SuperConstructorInvocation node) {
        if (node.arguments().isEmpty()) {
            // For some reason, deletion of super() invocation deletes comment on the previous line.
            // Therefore it replaces node with empty statement and removes it afterwards. This trick
            // has no impact on comments.
            EmptyStatement tempStatement = node.getAST().newEmptyStatement();
            ctx.getRefactorings().replace(node, tempStatement);
            ctx.getRefactorings().remove(tempStatement);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}
