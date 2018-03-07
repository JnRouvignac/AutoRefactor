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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;

/**
 * A visitor that collects a list of results.
 *
 * @param <R>
 *          type of the results returned by this finder visitor
 */
public class CollectorVisitor<R> extends ASTVisitor {
    /** The actual boolean result. */
    private final List<R> results = new ArrayList<R>();

    /**
     * Adds the provided result to the list of results.
     *
     * @param result a new result
     */
    protected void addResult(R result) {
        this.results.add(result);
    }

    /**
     * Adds the provided result to the list of results.
     *
     * @param results the new results
     */
    protected void addResults(Collection<R> results) {
        this.results.addAll(results);
    }

    /**
     * Calls {@link ASTNode#accept(ASTVisitor)} on the provided node
     * and returns the found result if one exists, or the default value.
     *
     * @param nodeToVisit the node to visit
     * @return the results found, may be empty
     */
    public List<R> collect(final ASTNode nodeToVisit) {
        nodeToVisit.accept(this);
        return results;
    }
}
