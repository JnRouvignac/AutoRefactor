/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.cfg;

import java.util.Objects;

/**
 * Contains a position in the source code with the start position from the start
 * of the file, the line number and the column number.
 */
class LineAndColumn {
    private final int startPosition;
    private final int lineNo;
    private final int colNo;

    public LineAndColumn(final int position, final int lineNo, final int colNo) {
        this.startPosition= position;
        this.lineNo= lineNo;
        this.colNo= colNo;
    }

    public int getStartPosition() {
        return startPosition;
    }

    public int getLine() {
        return lineNo;
    }

    public int getColumn() {
        return colNo;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        final LineAndColumn other= (LineAndColumn) obj;
        return Objects.equals(startPosition, other.startPosition);
    }

    @Override
    public int hashCode() {
        return Objects.hash(startPosition);
    }

    @Override
    public String toString() {
        return "LineAndColumn [startPosition=" + startPosition + ", lineNo=" + lineNo + ", colNo=" + colNo + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
}
