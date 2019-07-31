/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import org.eclipse.jdt.core.dom.ASTNode;

/**
 * NodeMatcher.
 *
 * @param <N> ASTNode type.
 */
public abstract class NodeMatcher<N extends ASTNode> {
    /**
     * Returns true if it matches, false if it is a boolean expression and it matches the opposite, or null otherwise.
     *
     * @param node The node
     * @return true if it matches, false if it is a boolean expression and it matches the opposite, or null otherwise.
     */
    public abstract Boolean isMatching(N node);

    /**
     * Returns true if it is a boolean expression and it matches the opposite, false if it matches, or null otherwise.
     *
     * @return true if it is a boolean expression and it matches the opposite, false if it matches, or null otherwise.
     */
    public NodeMatcher<N> negate() {
        final NodeMatcher<N> thisNodeMatcher= this;
        return new NodeMatcher<N>() {
            @Override
            public Boolean isMatching(N node) {
                Boolean isMatching= thisNodeMatcher.isMatching(node);

                if (isMatching == null) {
                    return null;
                }

                return !isMatching;
            }
        };
    }
}
