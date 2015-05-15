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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.SourceLocation.*;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.SourceLocation;
import org.autorefactor.util.IllegalStateException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/**
 * TODO JNR.
 */
public class RemoveEmptyLinesRefactoring extends AbstractRefactoringRule {

    private String newlineChars;

    /** {@inheritDoc} */
    @Override
    public boolean visit(CompilationUnit node) {
        final String source = this.ctx.getSource(node);
        newlineChars = getNewlineChars(source);
        final Refactorings r = this.ctx.getRefactorings();

        int index = getIndexOfFirstNonWhitespaceChar(source, 0);
        if (index != -1) {
            r.remove(SourceLocation.fromPositions(0, index));
            return DO_NOT_VISIT_SUBTREE;
        }

        if (node.getPackage() != null) {
            int lastIndex = node.getPackage().getStartPosition();
            int lastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, lastIndex - 1);
            int endOfLineIndex = source.indexOf(newlineChars, lastNonWsIndex);
            if (maybeRemoveEmptyLines(source, endOfLineIndex, lastIndex)) {
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        boolean result = VISIT_SUBTREE;
        final String newline = "(?:" + newlineChars + ")";
        Matcher m = Pattern.compile("(" + newline + "\\s*?" + newline + "\\s*?" + ")" + "(?:" + newline + "\\s*?)+")
                .matcher(source);
        while (m.find()) {
            r.replace(toSourceLocation(m, 0), substring(source, m, 1));
            result = DO_NOT_VISIT_SUBTREE;
        }
        return result;
    }

    private int getIndexOfFirstNonWhitespaceChar(String s, int offset) {
        if (Character.isWhitespace(s.charAt(offset))) {
            for (int i = offset; i < s.length(); i++) {
                if (!Character.isWhitespace(s.charAt(i))) {
                    return i;
                }
            }
        }
        return -1;
    }

    private int getLastIndexOfNonWhitespaceChar(String s, int fromIndex) {
        for (int i = fromIndex; 0 <= i; i--) {
            if (!Character.isWhitespace(s.charAt(i))) {
                return i;
            }
        }
        return fromIndex;
    }

    private String getNewlineChars(String source) {
        if (source.contains("\r\n")) {
            return "\r\n";
        } else if (source.contains("\n")) {
            return "\n";
        } else if (source.contains("\r")) {
            return "\r";
        }
        throw new IllegalStateException(null,
                "Cannot determine end of line encoding for source " + this.ctx.getCompilationUnit());
    }

    private SourceLocation toSourceLocation(Matcher m, int groupNumber) {
        return SourceLocation.fromPositions(m.start(groupNumber), m.end(groupNumber));
    }

    private String substring(String s, Matcher m, int groupNumber) {
        return s.substring(m.start(groupNumber), m.end(groupNumber));
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(AnnotationTypeDeclaration node) {
        return visit((AbstractTypeDeclaration) node);
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnumDeclaration node) {
        return visit((AbstractTypeDeclaration) node);
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(TypeDeclaration node) {
        return visit((AbstractTypeDeclaration) node);
    }

    private boolean visit(AbstractTypeDeclaration node) {
        final String source = this.ctx.getSource(node);
        int openingCurlyIndex = findOpeningCurlyForTypeBody(node, source);
        int newLineBeforeOpeningCurly = source.lastIndexOf(newlineChars, openingCurlyIndex) + newlineChars.length();
        int lastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, openingCurlyIndex - 1);
        int endOfLineIndex = source.indexOf(newlineChars, lastNonWsIndex);
        if (maybeRemoveEmptyLines(source, endOfLineIndex, newLineBeforeOpeningCurly)) {
            return DO_NOT_VISIT_SUBTREE;
        }

        int newLineAfterOpeningCurly = source.indexOf(newlineChars, openingCurlyIndex) + newlineChars.length();
        int lastNonWsIndex2 = getIndexOfFirstNonWhitespaceChar(source, newLineAfterOpeningCurly);
        int endOfLineIndex2 = source.lastIndexOf(newlineChars, lastNonWsIndex2) + newlineChars.length();
        if (maybeRemoveEmptyLines(source, openingCurlyIndex + 1, endOfLineIndex2)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return visitNodeWithClosingCurly(node);
    }

    private int findOpeningCurlyForTypeBody(AbstractTypeDeclaration node, String source) {
        int pos = node.getStartPosition();
        while (true) {
            int firstCurly = source.indexOf("{", pos);
            if (!this.ctx.isInComment(firstCurly)) {
                return firstCurly;
            }
            pos = firstCurly + 1;
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodDeclaration node) {
        final String source = this.ctx.getSource(node);
        int openingCurlyIndex = node.getBody().getStartPosition();
        int newLineBeforeOpeningCurly = source.lastIndexOf(newlineChars, openingCurlyIndex) + newlineChars.length();
        int lastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, openingCurlyIndex - 1);
        int endOfLineIndex = source.indexOf(newlineChars, lastNonWsIndex);
        if (maybeRemoveEmptyLines(source, endOfLineIndex, newLineBeforeOpeningCurly)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return visit(node.getBody());
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(Block node) {
        final String source = this.ctx.getSource(node);
        int openingCurlyIndex = node.getStartPosition();
        int newLineAfterOpeningCurly = source.indexOf(newlineChars, openingCurlyIndex) + newlineChars.length();
        int lastNonWsIndex = getIndexOfFirstNonWhitespaceChar(source, newLineAfterOpeningCurly);
        int endOfLineIndex = source.lastIndexOf(newlineChars, lastNonWsIndex) + newlineChars.length();
        if (maybeRemoveEmptyLines(source, openingCurlyIndex + 1, endOfLineIndex)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return visitNodeWithClosingCurly(node);
    }

    private boolean visitNodeWithClosingCurly(ASTNode node) {
        final String source = this.ctx.getSource(node);

        int closingCurlyIndex = source.lastIndexOf('}', getEndPosition(node));
        int newLineBeforeClosingCurly = source.lastIndexOf(newlineChars, closingCurlyIndex) + newlineChars.length();
        int lastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, closingCurlyIndex - 1);
        int endOfLineIndex = source.indexOf(newlineChars, lastNonWsIndex);
        if (maybeRemoveEmptyLines(source, endOfLineIndex, newLineBeforeClosingCurly)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean maybeRemoveEmptyLines(String source, int endOfLineIndex, int newLineIndex) {
        if (endOfLineIndex < newLineIndex
                && !equals(source, endOfLineIndex, newLineIndex, newlineChars)) {
            this.ctx.getRefactorings().replace(
                    SourceLocation.fromPositions(endOfLineIndex, newLineIndex),
                    newlineChars);
            return true;
        }
        return false;
    }

    /**
     * Equivalent to (but without allocations)
     *
     * <pre>
     * s1.substring(beginIndex, endIndex).equals(s2)
     * </pre>
     */
    private boolean equals(String s1, int beginIndex, int endIndex, String s2) {
        int length = endIndex - beginIndex;
        if (s2.length() != length) {
            return false;
        }
        for (int i = 0; i < length; i++) {
            if (s1.charAt(beginIndex + i) != s2.charAt(i)) {
                return false;
            }
        }
        return true;
    }
}
