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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.CompilationUnit;

public class SourceCode {

    public class Line extends SourceLocation {

        private String lineText;
        private SourceCode sourceCode;

        public Line(String lineText, int offset, int length, SourceCode sourceCode) {
            super(offset, length);
            this.lineText = lineText;
            this.sourceCode = sourceCode;
        }

        public String getLineText() {
            return lineText;
        }

        @Override
        public String toString() {
            return "[(" + sourceCode.astRoot.getLineNumber(getStart()) + ","
                    + sourceCode.astRoot.getColumnNumber(getStart()) + ")"
                    + " => (" + sourceCode.astRoot.getLineNumber(getEnd())
                    + "," + sourceCode.astRoot.getColumnNumber(getEnd()) + ")]";
        }

    }

    private CompilationUnit astRoot;
    private String text;
    private ICompilationUnit compilationUnit;
    private List<Line> lines = new ArrayList<Line>();

    public SourceCode(String text, CompilationUnit astRoot,
            ICompilationUnit compilationUnit) {
        this.astRoot = astRoot;
        this.text = text;
        this.compilationUnit = compilationUnit;
        computeLines();
    }

    private void computeLines() {
        try {
            final String lineSeparator = this.compilationUnit.findRecommendedLineSeparator();
            int fromIndex = 0;
            Matcher matcher = Pattern.compile(".*?" + lineSeparator).matcher(this.text);
            while (fromIndex < this.text.length() && matcher.find(fromIndex)) {
                String lineText = matcher.group();
                int offset = this.text.indexOf(lineText, fromIndex);
                int length = lineText.length();
                this.lines.add(new Line(lineText, offset, length, this));
                fromIndex += offset + length;
            }
        } catch (JavaModelException e) {
            throw new UnhandledException(e);
        }
    }

}
