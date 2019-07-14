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
package org.autorefactor.jdt.internal.corext.dom;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.*;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;

/**
 * A visitor that stops visiting as fast as possible once a result has been
 * found.
 *
 * @param <R> type of the results returned by this finder visitor
 */
public class FinderVisitor<R> extends ASTVisitor {
    /** Whether the result has been found. */
    private boolean resultFound;
    /** The actual boolean result. */
    private R result;

    /**
     * Sets the result.
     *
     * @param result the result
     */
    protected void setResult(R result) {
        this.resultFound= true;
        this.result= result;
    }

    /**
     * Calls {@link ASTNode#accept(ASTVisitor)} on the provided node and returns the
     * found result if one exists, or the default value.
     *
     * @param nodeToVisit   the node to visit
     * @param defaultResult the default result if no result could be found
     * @return the result found, or the default result when none exist
     */
    public R findOrDefault(final ASTNode nodeToVisit, final R defaultResult) {
        if (nodeToVisit != null) {
            nodeToVisit.accept(this);
        }
        return resultFound ? result : defaultResult;
    }

    @Override
    public boolean preVisit2(ASTNode node) {
        // exit has fast as possible when the result is found
        return resultFound ? DO_NOT_VISIT_SUBTREE : VISIT_SUBTREE;
    }
}
