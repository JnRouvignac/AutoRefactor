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
package org.autorefactor.util;

import java.util.Objects;

/**
 * An immutable pair of objects.
 *
 * @param <F> The type of the first element in the pair
 * @param <S> The type of the second element in the pair
 */
public final class Pair<F, S> {
    /** First object. */
    private final F first;
    /** Second object. */
    private final S second;

    /**
     * Returns an immutable empty pair.
     *
     * @param <F> the first element type
     * @param <S> the second element type
     * @return an immutable empty
     */
    public static <F, S> Pair<F, S> empty() {
        return new Pair<>(null, null);
    }

    /**
     * Returns an immutable pair made of the two objects.
     *
     * @param <F>    the first element type
     * @param <S>    the second element type
     * @param first  the first element, can be null
     * @param second the second element, can be null
     * @return an immutable pair made of the two objects.
     */
    public static <F, S> Pair<F, S> of(final F first, final S second) {
        return new Pair<>(first, second);
    }

    /**
     * Creates a new pair.
     *
     * @param first  the first element, can be null
     * @param second the second element, can be null
     */
    private Pair(final F first, final S second) {
        this.first= first;
        this.second= second;
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

    @Override
    public int hashCode() {
        return Objects.hash(first, second);
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        Pair<?, ?> other= (Pair<?, ?>) obj;
        return Objects.equals(first, other.first) && Objects.equals(second, other.second);
    }

    @Override
    public String toString() {
        return "FIRST: " + first + " SECOND: " + second; //$NON-NLS-1$ //$NON-NLS-2$
    }
}
