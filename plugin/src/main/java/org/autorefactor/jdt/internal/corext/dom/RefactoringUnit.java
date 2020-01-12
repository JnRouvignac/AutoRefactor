/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

import org.eclipse.jdt.core.ICompilationUnit;

/** Work item for the {@link ApplyRefactoringsJob}. */
public class RefactoringUnit implements Comparable {
    private final ICompilationUnit compilationUnit;
    private final JavaProjectOptions options;

    /**
     * CleanupUnit.
     *
     * @param compilationUnit compilationUnit
     * @param options         options
     */
    public RefactoringUnit(ICompilationUnit compilationUnit, JavaProjectOptions options) {
        this.compilationUnit= compilationUnit;
        this.options= options;
    }

    ICompilationUnit getCompilationUnit() {
        return compilationUnit;
    }

    JavaProjectOptions getOptions() {
        return options;
    }

    @Override
    public String toString() {
        return getCompilationUnit().toString();
    }

    @Override
    public int compareTo(Object o) {
        if (equals(o)) {
            return 0;
        }

        if (o == null || !(o instanceof RefactoringUnit)) {
            return 1;
        }

        RefactoringUnit other= (RefactoringUnit) o;

        if (this.compilationUnit == null) {
            if (other.compilationUnit == null) {
                return 0;
            }

            return 1;
        }
        if (other.compilationUnit == null) {
            return -1;
        }

        return compilationUnit.getElementName().compareTo(other.compilationUnit.getElementName());
    }

    @Override
    public int hashCode() {
        return Objects.hash(compilationUnit);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        RefactoringUnit other= (RefactoringUnit) obj;
        return Objects.equals(compilationUnit, other.compilationUnit);
    }
}
