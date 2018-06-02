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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.SourceLocation.getEndPosition;

import java.util.SortedSet;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.SourceLocation;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnnotationTypeDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.TypeDeclaration;

/** See {@link #getDescription()} method. */
public class RemoveEmptyLinesRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove empty lines";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Removes unnecessary empty lines from source code:\n"
            + "- empty lines after opening braces,\n"
            + "- empty lines before closing braces,\n"
            + "- two consecutive empty lines are converted to a single empty line.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    private static final Pattern NEWLINE_PATTERN = Pattern.compile("\\r\\n|\\n|\\r");
    private final TreeSet<Integer> lineEnds = new TreeSet<Integer>();

    @Override
    public boolean visit(CompilationUnit node) {
        lineEnds.clear();

        final String source = this.ctx.getSource(node);
        if (source.length() == 0) {
            // empty file, bail out
            return VISIT_SUBTREE;
        }

        computeLineEnds(node);

        final Refactorings r = this.ctx.getRefactorings();

        int index = getIndexOfFirstNonWhitespaceChar(source, 0);
        if (index != -1) {
            r.remove(SourceLocation.fromPositions(0, index));
            return DO_NOT_VISIT_SUBTREE;
        }

        if (node.getPackage() != null) {
            int lastIndex = node.getPackage().getStartPosition();
            int lastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, lastIndex - 1);
            if (lastNonWsIndex != -1) {
                int endOfLineIndex = beforeNewlineChars(source, lastNonWsIndex);
                if (maybeRemoveEmptyLines(source, endOfLineIndex, lastIndex)) {
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }

        boolean result = VISIT_SUBTREE;
        final String newline = "(?:" + NEWLINE_PATTERN + ")";
        Matcher m = Pattern.compile("(" + newline + "\\s*?" + newline + "\\s*?" + ")" + "(?:" + newline + "\\s*?)+")
                .matcher(source);
        while (m.find()) {
            final String matchedString = m.group(0);
            if (!"\r\n\r\n".equals(matchedString)
                    && !"\n\n".equals(matchedString)
                    && !"\r\r".equals(matchedString)) {
                r.remove(SourceLocation.fromPositions(m.end(1), m.end(0)));
                result = DO_NOT_VISIT_SUBTREE;
            }
        }
        if (result == DO_NOT_VISIT_SUBTREE) {
            return DO_NOT_VISIT_SUBTREE;
        }

        int afterLastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, source.length() - 1) + 1;
        if (substringMatchesAny(source, afterLastNonWsIndex, "\r\n", "\r", "\n")) {
            return VISIT_SUBTREE;
        }

        Matcher endOfFileMatcher = Pattern.compile(newline + "(" + "\\s*?" + "(" + newline + "\\s*?)+)")
                .matcher(source).region(afterLastNonWsIndex, source.length());
        if (endOfFileMatcher.find()) {
            r.remove(SourceLocation.fromPositions(endOfFileMatcher.start(2), endOfFileMatcher.end(2)));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean substringMatchesAny(String s, int offset, String... stringToMatch) {
        for (String toMatch : stringToMatch) {
            if (substringMatches(s, offset, toMatch)) {
                return true;
            }
        }
        return false;
    }

    private boolean substringMatches(final String s, int offset, String toMatch) {
        return s.regionMatches(offset, toMatch, 0, s.length() - offset);
    }

    private void computeLineEnds(CompilationUnit node) {
        final Matcher matcher = NEWLINE_PATTERN.matcher(ctx.getSource(node));
        while (matcher.find()) {
            lineEnds.add(matcher.end());
        }
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

    @Override
    public boolean visit(AnnotationTypeDeclaration node) {
        return visit((AbstractTypeDeclaration) node);
    }

    @Override
    public boolean visit(EnumDeclaration node) {
        return visit((AbstractTypeDeclaration) node);
    }

    @Override
    public boolean visit(TypeDeclaration node) {
        return visit((AbstractTypeDeclaration) node);
    }

    private boolean visit(AbstractTypeDeclaration node) {
        final String source = this.ctx.getSource(node);
        int openingCurlyIndex = findOpeningCurlyForTypeBody(node, source);
        if (openingCurlyOnSameLineAsEndOfNode(node, openingCurlyIndex)) {
            return VISIT_SUBTREE;
        }
        if (maybeRemoveEmptyLinesAfterCurly(node, openingCurlyIndex)) {
            return DO_NOT_VISIT_SUBTREE;
        }

        int lastNonWsIndex2 = getIndexOfFirstNonWhitespaceChar(source, openingCurlyIndex + 1);
        int endOfLineIndex2 = previousLineEnd(lastNonWsIndex2);
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

    @Override
    public boolean visit(MethodDeclaration node) {
        final Block body = node.getBody();
        if (body == null) {
            return VISIT_SUBTREE;
        }
        int openingCurlyIndex = body.getStartPosition();
        if (openingCurlyOnSameLineAsEndOfNode(node, openingCurlyIndex)) {
            return VISIT_SUBTREE;
        }
        if (maybeRemoveEmptyLinesAfterCurly(node, openingCurlyIndex)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return visit(body);
    }

    @Override
    public boolean visit(Block node) {
        final String source = this.ctx.getSource(node);
        int openingCurlyIndex = node.getStartPosition();
        if (openingCurlyOnSameLineAsEndOfNode(node, openingCurlyIndex)) {
            return VISIT_SUBTREE;
        }
        int lastNonWsIndex = getIndexOfFirstNonWhitespaceChar(source, openingCurlyIndex + 1);
        int endOfLineIndex = previousLineEnd(lastNonWsIndex);
        if (maybeRemoveEmptyLines(source, openingCurlyIndex + 1, endOfLineIndex)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return visitNodeWithClosingCurly(node);
    }

    private boolean visitNodeWithClosingCurly(ASTNode node) {
        final String source = this.ctx.getSource(node);
        int closingCurlyIndex = source.lastIndexOf('}', getEndPosition(node));
        if (maybeRemoveEmptyLinesAfterCurly(node, closingCurlyIndex)) {
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean openingCurlyOnSameLineAsEndOfNode(final ASTNode node, int openingCurlyIndex) {
        int lineEndAfterCurly = nextLineEnd(openingCurlyIndex);
        int lineEndAfterNode = nextLineEnd(getEndPosition(node));
        return lineEndAfterCurly == lineEndAfterNode;
    }

    private boolean maybeRemoveEmptyLinesAfterCurly(final ASTNode node, int curlyIndex) {
        final String source = ctx.getSource(node);
        int newLineBeforeCurly = previousLineEnd(curlyIndex);
        int lastNonWsIndex = getLastIndexOfNonWhitespaceChar(source, curlyIndex - 1);
        int endOfLineIndex = beforeNewlineChars(source, lastNonWsIndex);
        return maybeRemoveEmptyLines(source, endOfLineIndex, newLineBeforeCurly);
    }

    private boolean maybeRemoveEmptyLines(String source, int endOfLineIndex, int newLineIndex) {
        if (endOfLineIndex < newLineIndex) {
            Matcher matcher = NEWLINE_PATTERN.matcher(source).region(endOfLineIndex, newLineIndex);
            boolean isEqualToNewline = matcher.matches();
            if (!isEqualToNewline
                    && matcher.find()
                    && matcher.end() < newLineIndex) {
                final SourceLocation toRemove = SourceLocation.fromPositions(matcher.end(), newLineIndex);
                this.ctx.getRefactorings().remove(toRemove);
                return true;
            }
        }
        return false;
    }

    private int nextLineEnd(int fromIndex) {
        SortedSet<Integer> higher = lineEnds.tailSet(fromIndex + 1);
        return higher.isEmpty() ? -1 : higher.first();
    }

    private int previousLineEnd(int fromIndex) {
        SortedSet<Integer> lower = lineEnds.headSet(fromIndex + 1);
        return lower.isEmpty() ? -1 : lower.last();
    }

    private int beforeNewlineChars(final String source, int fromIndex) {
        Matcher matcher = NEWLINE_PATTERN.matcher(source);
        if (matcher.find(fromIndex)) {
            return matcher.start();
        }
        return -1;
    }
}
