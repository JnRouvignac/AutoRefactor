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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.statements;
import static org.eclipse.jdt.core.dom.IfStatement.ELSE_STATEMENT_PROPERTY;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
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
public class IfElseIfRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "if-elseif";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactors \"else { if (...) {} }\" to \"else if (...) {}\".";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    // TODO JNR

    // UseIfElseIfRefactoring
    // if (b) {
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
    public boolean visit(IfStatement node) {
        final Statement elseStmt = node.getElseStatement();
        if (elseStmt instanceof Block) {
            List<Statement> elseStmts = statements((Block) elseStmt);
            if (elseStmts.size() == 1
                    && elseStmts.get(0) instanceof IfStatement) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                this.ctx.getRefactorings().set(node, ELSE_STATEMENT_PROPERTY, b.copy(elseStmts.get(0)));
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }
}
