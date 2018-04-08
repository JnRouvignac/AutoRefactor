/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.DoStatement;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.WhileStatement;

/** See {@link #getDescription()} method. */
public class AddBracketsToControlStatementRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Add brackets to control statement";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Adds brackets to:\n"
            + "- if then/else clauses,\n"
            + "- for loop body,\n"
            + "- do ... while loop body,\n"
            + "- while loop body.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility and reduce the bug hazard.";
    }

    @Override
    public boolean visit(IfStatement node) {
        boolean result = VISIT_SUBTREE;
        if (node.getThenStatement() != null
                && !(node.getThenStatement() instanceof Block)) {
            result = setBlock(node.getThenStatement());
        }
        if (node.getElseStatement() != null
                && !(node.getElseStatement() instanceof Block)
                && !(node.getElseStatement() instanceof IfStatement)) {
            return setBlock(node.getElseStatement()) || result;
        }
        return result;
    }

    @Override
    public boolean visit(EnhancedForStatement node) {
        if (node.getBody() != null && !(node.getBody() instanceof Block)) {
            return setBlock(node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(ForStatement node) {
        if (node.getBody() != null && !(node.getBody() instanceof Block)) {
            return setBlock(node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(WhileStatement node) {
        if (node.getBody() != null && !(node.getBody() instanceof Block)) {
            return setBlock(node.getBody());
        }
        return VISIT_SUBTREE;
    }

    @Override
    public boolean visit(DoStatement node) {
        if (node.getBody() != null && !(node.getBody() instanceof Block)) {
            return setBlock(node.getBody());
        }
        return VISIT_SUBTREE;
    }

    private boolean setBlock(Statement statement) {
        if (statement == null) {
            return VISIT_SUBTREE;
        }
        final ASTBuilder b = this.ctx.getASTBuilder();
        final Block block = b.block(b.copy(statement));
        block.accept(this);
        this.ctx.getRefactorings().replace(statement, block);
        return DO_NOT_VISIT_SUBTREE;
    }
}
