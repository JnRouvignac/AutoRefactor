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
package org.autorefactor.refactoring;

import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.dom.ASTNode;

import static org.autorefactor.util.Utils.*;

/**
 * Represents a source location in a file, i.e. a position or a range in a source file.
 */
public class SourceLocation implements ISourceRange, Comparable<ISourceRange> {

    private final int offset;
    private final int length;

    /**
     * Builds a source location instance from an offset and a length in a source file.
     *
     * @param offset the position offset in the file
     * @param length the length starting from the offset
     */
    public SourceLocation(int offset, int length) {
        this.offset = offset;
        this.length = length;
    }

    /**
     * Builds a source location instance from an {@link ASTNode} location.
     *
     * @param node the {@link ASTNode} where to read positions from
     */
    public SourceLocation(ASTNode node) {
        this(node.getStartPosition(), node.getLength());
    }

    /**
     * Factory method for a source range in a file.
     *
     * @param startPos the start position in the file
     * @param endPos the end position in the file
     * @return a source location instance representing a range in a source file.
     */
    public static SourceLocation fromPositions(int startPos, int endPos) {
        if (startPos > endPos) {
            throw new IllegalArgumentException("start position (" + startPos
                    + ") should be situated before end position (" + startPos + ")");
        }
        return new SourceLocation(startPos, endPos - startPos);
    }

    /**
     * Factory method for a position in a source file.
     *
     * @param startPosition the start position in the file
     * @return a source location instance representing a point in a source file.
     */
    public static SourceLocation fromStartPosition(int startPosition) {
        return new SourceLocation(startPosition, 0);
    }

    /**
     * Returns the end position of the provided {@link ASTNode} (start position + length).
     *
     * @param node the node for which to compute the end position
     * @return the end position of the provided {@link ASTNode}
     */
    public static int getEndPosition(ASTNode node) {
        return node.getStartPosition() + node.getLength();
    }

    /** {@inheritDoc} */
    @Override
    public int getLength() {
        return this.length;
    }

    /** {@inheritDoc} */
    @Override
    public int getOffset() {
        return this.offset;
    }

    /**
     * Returns the start position of this source location in the source file.
     *
     * @return the start position of this source location in the source file.
     */
    public int getStartPosition() {
        return this.offset;
    }

    /**
     * Returns the end position of this source location in the source file.
     *
     * @return the end position of this source location in the source file.
     */
    public int getEndPosition() {
        return this.offset + this.length;
    }

    /**
     * Whether the provided position is inside the current source location.
     *
     * @param position the provided position
     * @return true if the provided position is inside the current source location, false otherwise
     */
    public boolean contains(int position) {
        return getStartPosition() <= position && position <= getEndPosition();
    }

    /**
     * Whether the provided source range is inside the current source location.
     *
     * @param sourceRange the provided source range
     * @return true if the provided source range is inside the current source location, false otherwise
     */
    public boolean contains(ISourceRange sourceRange) {
        return getStartPosition() <= sourceRange.getOffset()
                && sourceRange.getOffset() + sourceRange.getLength() <= getEndPosition();
    }

    /** {@inheritDoc} */
    @Override
    public int compareTo(ISourceRange sourceRange) {
        final int offsetDiff = this.offset - sourceRange.getOffset();
        if (offsetDiff != 0) {
            return offsetDiff;
        }
        return this.length - sourceRange.getLength();
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + length;
        result = prime * result + offset;
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
        final SourceLocation other = (SourceLocation) obj;
        return equal(length, other.length)
                && equal(offset, other.offset);
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "SourceLocation [offset=" + offset + ", length=" + length + "]";
    }

}
