/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

public class SourceLocation implements ISourceRange, Comparable<ISourceRange> {

    private int offset;
    private int length;

    public SourceLocation(int offset, int length) {
        this.offset = offset;
        this.length = length;
    }

    public SourceLocation(int start, int end, boolean todoRemove) {
        this(start, end - start);
    }

    public SourceLocation(int start) {
        this(start, 0);
    }

    public SourceLocation(ASTNode node) {
        this(node.getStartPosition(), node.getLength());
    }

    public int getLength() {
        return this.length;
    }

    public int getOffset() {
        return this.offset;
    }

    public int getStart() {
        return this.offset;
    }

    public int getEnd() {
        return this.offset + this.length;
    }

    public boolean contains(int position) {
        return getStart() <= position && position <= getEnd();
    }

    public boolean contains(ISourceRange sourceRange) {
        return getStart() <= sourceRange.getOffset()
                && sourceRange.getOffset() + sourceRange.getLength() <= getEnd();
    }

    public int compareTo(ISourceRange sourceRange) {
        final int offsetDiff = this.offset - sourceRange.getOffset();
        if (offsetDiff != 0) {
            return offsetDiff;
        }
        return this.length - sourceRange.getLength();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + length;
        result = prime * result + offset;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        final Boolean equal = basicEqual(this, obj);
        if (equal != null) {
            return equal;
        }
        final SourceLocation other = (SourceLocation) obj;
        return equal(length, other.length)
                && equal(offset, other.offset);
    }

    @Override
    public String toString() {
        return "SourceLocation [offset=" + offset + ", length=" + length + "]";
    }

}
