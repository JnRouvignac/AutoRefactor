/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.util;

import static org.autorefactor.util.Utils.*;

/**
 * An immutable pair of objects.
 *
 * @param <F>
 *            The type of the first element in the pair
 * @param <S>
 *            The type of the second element in the pair
 */
public final class Pair<F, S> {

    /** first object. */
    private final F first;
    /** second object. */
    private final S second;

    /**
     * Returns an immutable pair made of the two objects.
     *
     * @param <F>
     *            the first element type
     * @param <S>
     *            the second element type
     * @param first
     *            the first element, can be null
     * @param second
     *            the second element, can be null
     * @return an immutable pair made of the two objects.
     */
    public static <F, S> Pair<F, S> of(F first, S second) {
        return new Pair<F, S>(first, second);
    }

    /**
     * Creates a new pair.
     *
     * @param first
     *            the first element, can be null
     * @param second
     *            the second element, can be null
     */
    private Pair(F first, S second) {
        super();
        this.first = first;
        this.second = second;
    }

    /**
     * Gets the first element of this pair.
     *
     * @return the first element, can be null
     */
    public F getFirst() {
        return first;
    }

    /**
     * Gets the second element of this pair.
     *
     * @return the second element, can be null
     */
    public S getSecond() {
        return second;
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((first == null) ? 0 : first.hashCode());
        result = prime * result + ((second == null) ? 0 : second.hashCode());
        return result;
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final Pair<?, ?> other = (Pair<?, ?>) obj;
        return equal(first, other.first)
                && equal(second, other.second);
    }

    @Override
    public String toString() {
        return "FIRST: " + first + " SECOND: " + second;
    }

}
