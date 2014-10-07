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
package org.autorefactor.refactoring;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * Represents a source file.
 */
public class SourceCode {

    /**
     * Represents a line in a source file.
     */
    public static class Line extends SourceLocation {

        private final String lineText;
        private final SourceCode sourceCode;

        /**
         * Builds an instance of this class.
         *
         * @param lineText the text of this line
         * @param offset the start position of this line in the source file
         * @param length the length of this line
         * @param sourceCode the enclosing source file
         */
        public Line(String lineText, int offset, int length, SourceCode sourceCode) {
            super(offset, length);
            this.lineText = lineText;
            this.sourceCode = sourceCode;
        }

        /**
         * Returns the line text as a String.
         *
         * @return the line text as a String.
         */
        public String getLineText() {
            return lineText;
        }

        @Override
        public String toString() {
            return "[(" + sourceCode.astRoot.getLineNumber(getStartPosition()) + ","
                    + sourceCode.astRoot.getColumnNumber(getStartPosition()) + ")"
                    + " => (" + sourceCode.astRoot.getLineNumber(getEndPosition())
                    + "," + sourceCode.astRoot.getColumnNumber(getEndPosition()) + ")]";
        }

    }

    private final CompilationUnit astRoot;
    private final String text;
    private final ICompilationUnit compilationUnit;
    private final List<Line> lines = new ArrayList<Line>();

    /**
     * Builds an instance of this class.
     *
     * @param text the text of the source file.
     * @param astRoot the AST root of the source file
     * @param compilationUnit the compilation unit of the source file
     */
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
            throw new UnhandledException(astRoot, e);
        }
    }

}
