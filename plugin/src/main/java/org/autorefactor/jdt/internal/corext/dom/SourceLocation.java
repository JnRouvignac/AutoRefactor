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
package org.autorefactor.jdt.internal.corext.dom;

import java.util.Objects;

import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.dom.ASTNode;

/**
 * Represents a source location in a file, i.e. a position or a range in a
 * source file.
 */
public class SourceLocation implements ISourceRange, Comparable<ISourceRange> {
	private final int offset;
	private final int length;

	/**
	 * Builds a source location instance from an offset and a length in a source
	 * file.
	 *
	 * @param offset the position offset in the file
	 * @param length the length starting from the offset
	 */
	public SourceLocation(final int offset, final int length) {
		this.offset= offset;
		this.length= length;
	}

	/**
	 * Builds a source location instance from an {@link ASTNode} location.
	 *
	 * @param node the {@link ASTNode} where to read positions from
	 */
	public SourceLocation(final ASTNode node) {
		this(node.getStartPosition(), node.getLength());
	}

	/**
	 * Factory method for a source range in a file.
	 *
	 * @param startPos the start position in the file
	 * @param endPos   the end position in the file
	 * @return a source location instance representing a range in a source file.
	 */
	public static SourceLocation fromPositions(final int startPos, final int endPos) {
		if (startPos > endPos) {
			throw new IllegalArgumentException(
					"start position (" + startPos + ") should be situated before end position (" + startPos + ")"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}

		return new SourceLocation(startPos, endPos - startPos);
	}

	/**
	 * Factory method for a position in a source file.
	 *
	 * @param startPosition the start position in the file
	 * @return a source location instance representing a point in a source file.
	 */
	public static SourceLocation fromStartPosition(final int startPosition) {
		return new SourceLocation(startPosition, 0);
	}

	/**
	 * Returns the end position of the provided {@link ASTNode} (start position +
	 * length).
	 *
	 * @param node the node for which to compute the end position
	 * @return the end position of the provided {@link ASTNode}
	 */
	public static int getEndPosition(final ASTNode node) {
		return node.getStartPosition() + node.getLength();
	}

	private static int getEndPosition(final ISourceRange range) {
		return range.getOffset() + range.getLength();
	}

	/**
	 * Get the length.
	 *
	 * @return the length.
	 */
	@Override
	public int getLength() {
		return this.length;
	}

	/**
	 * Get the offset.
	 *
	 * @return the offset.
	 */
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
	 * @return true if the provided position is inside the current source location,
	 *         false otherwise
	 */
	public boolean contains(final int position) {
		return getStartPosition() <= position && position <= getEndPosition();
	}

	/**
	 * Whether the provided source range is inside the current source location.
	 *
	 * @param sourceRange the provided source range
	 * @return true if the provided source range is inside the current source
	 *         location, false otherwise
	 */
	public boolean contains(final ISourceRange sourceRange) {
		return getStartPosition() <= sourceRange.getOffset()
				&& sourceRange.getOffset() + sourceRange.getLength() <= getEndPosition();
	}

	/**
	 * Returns the substring of the provided String by using the current source
	 * location.
	 *
	 * @param s the string for which to provide the substring
	 * @return the substring of the provided string
	 */
	public String substring(final String s) {
		return s.substring(getStartPosition(), getEndPosition());
	}

	/**
	 * Whether the provided source range overlaps with the current source location.
	 *
	 * @param sourceRange the provided source range
	 * @return true if the provided source range overlaps with the current source
	 *         location, false otherwise
	 */
	public boolean overlapsWith(final ISourceRange sourceRange) {
		return overlapsLeft(sourceRange) || overlapsRight(sourceRange);
	}

	private boolean overlapsLeft(final ISourceRange range) {
		return getStartPosition() <= range.getOffset() && range.getOffset() <= getEndPosition();
	}

	private boolean overlapsRight(final ISourceRange range) {
		return range.getOffset() <= getStartPosition() && getStartPosition() <= getEndPosition(range);
	}

	/**
	 * Compare objects.
	 *
	 * @param sourceRange Second item
	 *
	 * @return -1, 0 or 1
	 */
	@Override
	public int compareTo(final ISourceRange sourceRange) {
		int offsetDiff= this.offset - sourceRange.getOffset();
		if (offsetDiff != 0) {
			return offsetDiff;
		}

		return this.length - sourceRange.getLength();
	}

	@Override
	public int hashCode() {
		return Objects.hash(length, offset);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		SourceLocation other= (SourceLocation) obj;
		return Objects.equals(length, other.length) && Objects.equals(offset, other.offset);
	}

	@Override
	public String toString() {
		return "SourceLocation [offset=" + offset + ", length=" + length + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
}
