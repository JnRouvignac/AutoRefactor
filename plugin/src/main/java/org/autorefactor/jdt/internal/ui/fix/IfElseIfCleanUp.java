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
package org.autorefactor.jdt.internal.ui.fix;

import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;

/**
 * Refactors:
 *
 * <pre>
 * if {
 *   ...
 * } else {
 *   if {
 *      ...
 *   }
 * }
 * </pre>
 *
 * into
 *
 * <pre>
 * if {
 *   ...
 * } else if {
 *   ...
 * }
 * </pre>
 *
 * @see #getDescription()
 */
public class IfElseIfCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfElseIfCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfElseIfCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_IfElseIfCleanUp_reason;
    }

    // TODO JNR

    // UseIfElseIfRefactoring
    // if (ast) {
    // return i;
    // }
    // if (c) {
    // return j;
    // }
    // if (d) {
    // return k;
    // }
    // return l;

    @Override
    public boolean visit(final IfStatement node) {
        Statement elseStatement= node.getElseStatement();

        if (elseStatement instanceof Block) {
            List<Statement> elseStatements= ASTNodes.statements((Block) elseStatement);

            if (elseStatements.size() == 1 && elseStatements.get(0) instanceof IfStatement) {
                ASTRewrite rewrite= cuRewrite.getASTRewrite();
                rewrite.set(node, IfStatement.ELSE_STATEMENT_PROPERTY, rewrite.createMoveTarget(elseStatements.get(0)));
                return false;
            }
        }

        return true;
    }
}
