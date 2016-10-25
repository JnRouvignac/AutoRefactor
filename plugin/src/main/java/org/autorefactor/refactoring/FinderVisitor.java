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

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;

/**
 * A visitor that stops visiting as fast as possible once a result has been found.
 *
 * @param <R> type of the results returned by this finder visitor
 */
public class FinderVisitor<R> extends ASTVisitor {
    /** Whether the result has been found. */
    private boolean resultFound;
    /** The actual boolean result. */
    private R result;

    /**
     * Returns the result.
     *
     * @return the result
     */
    public R result() {
        return result;
    }

    /**
     * Sets the default result.
     *
     * @param result the result
     * @return this visitor
     */
    public FinderVisitor<R> defaultResult(R result) {
        this.result = result;
        return this;
    }

    /**
     * Sets the result.
     *
     * @param result the result
     */
    public void setResult(R result) {
        this.resultFound = true;
        this.result = result;
    }

    @Override
    public boolean preVisit2(ASTNode node) {
        // exit has fast as possible when the result is found
        return resultFound ? DO_NOT_VISIT_SUBTREE : VISIT_SUBTREE;
    }
}
