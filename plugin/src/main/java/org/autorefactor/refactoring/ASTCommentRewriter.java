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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.    If not, see
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
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;

/**
 * This class rewrites AST comments.
 */
public class ASTCommentRewriter {

    /**
     * Using a Set to avoid duplicates because Javadocs are visited twice via
     * CompilationUnit.getCommentList() and visit(Javadoc).
     */
    private Set<Comment> removals = new LinkedHashSet<Comment>();
    private Set<Pair<Comment, String>> replacements = new LinkedHashSet<Pair<Comment, String>>();
    private List<BlockComment> blockCommentToJavadoc = new ArrayList<BlockComment>();
    private List<List<LineComment>> lineCommentsToJavadoc = new ArrayList<List<LineComment>>();

    /** Default constructor. */
    public ASTCommentRewriter() {
        super();
    }

    /**
     * Removes the provided comment.
     *
     * @param comment the comment to remove
     */
    public void remove(Comment comment) {
        this.removals.add(comment);
    }

    /**
     * Replaces the provided comment with the provided replacement text.
     *
     * @param comment the comment to replace
     * @param replacement the replacement text
     */
    public void replace(Comment comment, String replacement) {
        this.replacements.add(Pair.of(comment, replacement));
    }

    /**
     * Converts the provided block comment into a javadoc.
     *
     * @param comment the block comment to convert into a javadoc
     */
    public void toJavadoc(BlockComment comment) {
        this.blockCommentToJavadoc.add(comment);
    }

    /**
     * Converts the provided list of line comments into a javadoc.
     *
     * @param comments the list of line comments to convert into a javadoc
     */
    public void toJavadoc(List<LineComment> comments) {
        this.lineCommentsToJavadoc.add(comments);
    }

    /**
     * Adds the edits contained in the current instance to the provided edits for the provided document.
     *
     * @param document the provided document to edit
     * @param edits where to add edits
     */
    public void addEdits(IDocument document, TextEdit edits) {
        addRemovalEdits(edits, document.get());
        addReplacementEdits(edits);
        addToJavadocEdits(edits);
    }

    private void addRemovalEdits(TextEdit edits, String text) {
        if (this.removals.isEmpty()) {
            return;
        }
        for (Comment node : this.removals) {
            final int start = node.getStartPosition();
            final int length = node.getLength();

            // chomp from the end before the start variable gets modified
            final int startToRemove = chompWhitespacesBefore(text, start);
            final int endToRemove = chompWhitespacesAfter(text, start + length);
            final int lengthToRemove = endToRemove - startToRemove;

            edits.addChild(new DeleteEdit(startToRemove, lengthToRemove));
        }
    }

    private void addReplacementEdits(TextEdit edits) {
        if (this.replacements.isEmpty()) {
            return;
        }
        for (Pair<Comment, String> pair : this.replacements) {
            final Comment node = pair.getFirst();
            final int start = node.getStartPosition();
            final int length = node.getLength();

            edits.addChild(new ReplaceEdit(start, length, pair.getSecond()));
        }
    }

    private void addToJavadocEdits(TextEdit edits) {
        for (BlockComment blockComment : this.blockCommentToJavadoc) {
            final int offset = blockComment.getStartPosition() + "/*".length();
            edits.addChild(new InsertEdit(offset, "*"));
        }
        for (List<LineComment> lineComments : this.lineCommentsToJavadoc) {
            // TODO Collect all words from the line comments,
            // then get access to indent settings, line length and newline chars
            // then spread them across several lines if needed or folded on one line only
            if (lineComments.size() == 1) {
                final LineComment lineComment = lineComments.get(0);
                final int start = lineComment.getStartPosition();
                // TODO JNR how to obey configured indentation?
                // TODO JNR how to obey configured line length?
                edits.addChild(new ReplaceEdit(start, "//".length(), "/**"));
                edits.addChild(new InsertEdit(start + lineComment.getLength(), " */"));
                continue;
            }

            boolean isFirst = true;
            for (Iterator<LineComment> iter = lineComments.iterator(); iter.hasNext();) {
                LineComment lineComment = iter.next();
                if (isFirst) {
                    edits.addChild(new ReplaceEdit(lineComment.getStartPosition(), "//".length(), "/**"));
                    // TODO JNR how to obey configured indentation?
                    // TODO JNR how to obey configured line length?
                    isFirst = false;
                } else {
                    edits.addChild(new ReplaceEdit(lineComment.getStartPosition(), "//".length(), " *"));
                }
                if (!iter.hasNext()) {
                    // this was the last line comment to transform
                    // TODO JNR how to get access to configured newline? @see #getNewline();
                    // TODO JNR how to obey configured indentation?
                    edits.addChild(new InsertEdit(lineComment.getStartPosition() + lineComment.getLength(), "\n*/"));
                }
            }
        }
    }

    private void getNewline() {
        // TODO how to get access to configured newline
        // Answer: use ICompilationUnit.findRecommendedLineSeparator()
    }

    private int chompWhitespacesBefore(final String text, int start) {
        int i = start - 1;
        while (i >= 0) {
            final char c = text.charAt(i);
            // TODO JNR how to get project specific newline separator?? @see #getNewline();
            if (!Character.isWhitespace(c) || c == '\n') {
                break;
            }
            start = i;
            i--;
        }
        return start;
    }

    private int chompWhitespacesAfter(final String text, int end) {
        int i = end;
        while (i < text.length()) {
            final char c = text.charAt(i);
            if (!Character.isWhitespace(c)) {
                break;
            }
            i++;
            end = i;
            // TODO JNR how to get project specific newline separator?? @see #getNewline();
            if (c == '\n') {
                // we chomped the newline character, do not chomp on the next line
                break;
            }
        }
        return end;
    }

}
