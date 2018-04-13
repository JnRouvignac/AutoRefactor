/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;

import org.autorefactor.refactoring.rules.RefactoringContext;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Block;

/**
 * A visitor to refactor several sibling nodes and flag the parent block to visited.
 */
public class BlockSubVisitor extends ASTVisitor {
    /**
     * The refactoring context.
     */
    protected RefactoringContext ctx;

    private Block startNode;

    private boolean result = VISIT_SUBTREE;

    /**
     * Constructor.
     *
     * @param ctx The refactoring context
     * @param startNode The start node block
     */
    public BlockSubVisitor(final RefactoringContext ctx, final Block startNode) {
        this.ctx = ctx;
        this.startNode = startNode;
    }

    /**
     * Get the refactoring context.
     *
     * @return The refactoring context
     * @deprecated Call ctx field instead
     */
    @Deprecated
    public RefactoringContext getCtx() {
        return ctx;
    }

    /**
     * Get the result.
     *
     * @return The result
     */
    public boolean getResult() {
        return result;
    }

    /**
     * Set the result.
     *
     * @param result The result
     */
    public void setResult(final boolean result) {
        this.result = result;
    }

    @Override
    public boolean visit(Block node) {
        return (startNode == node) ? VISIT_SUBTREE : DO_NOT_VISIT_SUBTREE;
    }
}
