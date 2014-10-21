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
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.autorefactor.util.NotImplementedException;
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.RangeMarker;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditVisitor;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * This class rewrites AST comments.
 */
public class ASTCommentRewriter {

    /**
     * Using a Set to avoid duplicates because Javadocs are visited twice via
     * CompilationUnit.getCommentList() and visit(Javadoc).
     */
    private final Set<Comment> removals = new LinkedHashSet<Comment>();
    private final Set<Pair<Comment, String>> replacements = new LinkedHashSet<Pair<Comment, String>>();
    private final List<BlockComment> blockCommentToJavadoc = new ArrayList<BlockComment>();
    private final Map<ASTNode, List<LineComment>> lineCommentsToJavadoc = new HashMap<ASTNode, List<LineComment>>();

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
     * Adds the provided line comment to convert to javadoc.
     *
     * @param lineComment the line comment to convert to javadoc
     * @param nextNode the AST node immediately following the line comment
     */
    public void toJavadoc(LineComment lineComment, ASTNode nextNode) {
        List<LineComment> comments = lineCommentsToJavadoc.get(nextNode);
        if (comments == null) {
            comments = new LinkedList<LineComment>();
            lineCommentsToJavadoc.put(nextNode, comments);
        }
        comments.add(lineComment);
    }

    /**
     * Adds the edits contained in the current instance to the provided edits for the provided document.
     *
     * @param document the provided document to edit
     * @param edits where to add edits
     */
    public void addEdits(IDocument document, TextEdit edits) {
        final String source = document.get();
        final List<TextEdit> commentEdits = new LinkedList<TextEdit>();
        addRemovalEdits(commentEdits, source);
        addReplacementEdits(commentEdits);
        addBlockCommentToJavadocEdits(commentEdits);
        addLineCommentsToJavadocEdits(commentEdits, source);
        if (!commentEdits.isEmpty() && !anyCommentEditIsCovered(edits, commentEdits)) {
            edits.addChildren(commentEdits.toArray(new TextEdit[0]));
        }
        // else, code edits take priority. Give up applying current text edits.
        // They will be retried in the next refactoring loop.
    }

    private boolean anyCommentEditIsCovered(TextEdit edits, List<TextEdit> commentEdits) {
        for (TextEdit commentEdit : commentEdits) {
            if (covers(edits, commentEdit.getOffset(), commentEdit.getLength())) {
                return true;
            }
        }
        return false;
    }

    private boolean covers(TextEdit edits, int startPosition, int length) {
        final TextEdit range = new RangeMarker(startPosition, length);
        final AtomicBoolean covers = new AtomicBoolean();
        edits.accept(new TextEditVisitor() {
            @Override
            public boolean visit(MultiTextEdit edit) {
                // move on there is nothing to check here
                return VISIT_SUBTREE;
            }

            @Override
            public boolean visitNode(TextEdit edit) {
                if (!covers.get()) {
                    // is it covered now?
                    covers.set(edit.covers(range));
                }
                return !covers.get();
            }
        });
        return covers.get();
    }

    private void addRemovalEdits(List<TextEdit> commentEdits, String source) {
        if (this.removals.isEmpty()) {
            return;
        }
        for (Comment node : this.removals) {
            final int start = node.getStartPosition();
            final int length = node.getLength();

            // chomp from the end before the start variable gets modified
            final int startToRemove = chompWhitespacesBefore(source, start);
            final int endToRemove = chompWhitespacesAfter(source, start + length);
            final int lengthToRemove = endToRemove - startToRemove;

            commentEdits.add(new DeleteEdit(startToRemove, lengthToRemove));
        }
    }

    private void addReplacementEdits(List<TextEdit> commentEdits) {
        if (this.replacements.isEmpty()) {
            return;
        }
        for (Pair<Comment, String> pair : this.replacements) {
            final Comment node = pair.getFirst();
            final int start = node.getStartPosition();
            final int length = node.getLength();
            commentEdits.add(new ReplaceEdit(start, length, pair.getSecond()));
        }
    }

    private void addBlockCommentToJavadocEdits(List<TextEdit> commentEdits) {
        for (BlockComment blockComment : this.blockCommentToJavadoc) {
            final int offset = blockComment.getStartPosition() + "/*".length();
            commentEdits.add(new InsertEdit(offset, "*"));
        }
    }

    private void addLineCommentsToJavadocEdits(List<TextEdit> commentEdits, String source) {
        if (this.lineCommentsToJavadoc.isEmpty()) {
            return;
        }
        final TreeSet<Integer> lineStarts = getLineStarts(source);
        for (Entry<ASTNode, List<LineComment>> entry : this.lineCommentsToJavadoc.entrySet()) {
            final List<LineComment> lineComments = entry.getValue();
            // TODO Collect all words from the line comments,
            // then get access to indent settings, line length and newline chars
            // then spread them across several lines if needed or folded on one line only
            if (lineComments.size() == 1) {
                addSingleLineCommentToJavadocEdits(commentEdits, entry.getKey(), lineComments, source, lineStarts);
            } else {
                addMultiLineCommentsToJavadocEdits(commentEdits, entry.getKey(), lineComments, source, lineStarts);
            }
        }
    }

    private TreeSet<Integer> getLineStarts(String source) {
        final TreeSet<Integer> lineStarts = new TreeSet<Integer>();
        lineStarts.add(0);

        final Matcher matcher = Pattern.compile("\\r\\n|\\r|\\n").matcher(source);
        while (matcher.find()) {
            lineStarts.add(matcher.end());
        }
        return lineStarts;
    }

    private void addSingleLineCommentToJavadocEdits(List<TextEdit> commentEdits,
            ASTNode nextNode, List<LineComment> lineComments, String source, TreeSet<Integer> lineStarts) {
        final int nodeStart = nextNode.getStartPosition();
        final LineComment lineComment = lineComments.get(0);
        final int commentStart = lineComment.getStartPosition();
        final int commentLength = lineComment.getLength();
        final String comment = source.substring(commentStart, commentStart + commentLength);
        final String commentText = comment.substring(2);
        // add a starting and ending space?
        final String spaceAtStart = !Character.isWhitespace(commentText.charAt(0)) ? " " : "";
        final String spaceAtEnd = !Character.isWhitespace(commentText.charAt(commentText.length() - 1)) ? " " : "";

        // TODO JNR how to obey configured indentation?
        // TODO JNR how to obey configured line length?
        if (commentStart < nodeStart) {
            // assume comment is situated exactly before target node for javadoc
            commentEdits.add(new ReplaceEdit(commentStart, "//".length(), "/**" + spaceAtStart));
            commentEdits.add(new InsertEdit(commentStart + commentLength, spaceAtEnd + "*/"));
        } else {
            // assume comment is situated exactly after target node for javadoc
            final String indent = getIndent(nextNode, source, lineStarts);
            final String newJavadoc = "/**" + spaceAtStart + commentText + spaceAtEnd + "*/\r\n" + indent;
            final int nbWhiteSpaces = nbTrailingSpaces(source, commentStart);
            commentEdits.add(new InsertEdit(nodeStart, newJavadoc));
            commentEdits.add(new DeleteEdit(commentStart - nbWhiteSpaces, nbWhiteSpaces + commentLength));
        }
    }

    private int nbTrailingSpaces(String source, int commentStart) {
        int result = 0;
        while (Character.isWhitespace(source.charAt(commentStart - result - 1))) {
            ++result;
        }
        return result;
    }

    private void addMultiLineCommentsToJavadocEdits(List<TextEdit> commentEdits, ASTNode node,
            List<LineComment> lineComments,
             String source, TreeSet<Integer> lineStarts) {
        final String newline = "\n";
        for (int i = 0; i < lineComments.size(); i++) {
            final LineComment lineComment = lineComments.get(i);
            if (node.getStartPosition() < lineComment.getStartPosition()) {
                throw new NotImplementedException(lineComment,
                        " for comments situated after the target node for the Javadoc");
            }
            final String replacementText;
            final boolean isFirst = i == 0;
            if (isFirst) {
                // TODO JNR how to get access to configured newline? @see #getNewline();
                // TODO JNR how to obey configured indentation?
                replacementText = "/**" + newline + getIndentForJavadoc(lineComment, source, lineStarts) + "*";
            } else {
                replacementText = " *";
            }
            commentEdits.add(new ReplaceEdit(lineComment.getStartPosition(), "//".length(), replacementText));

            final boolean isLast = i == lineComments.size() - 1;
            if (isLast) {
                // TODO JNR how to get access to configured newline? @see #getNewline();
                // TODO JNR how to obey configured indentation?
                final int position = lineComment.getStartPosition() + lineComment.getLength();
                final String indent = getIndentForJavadoc(lineComment, source, lineStarts);
                commentEdits.add(new InsertEdit(position, newline + indent + "*/"));
            }
        }
    }

    private String getIndentForJavadoc(final LineComment lineComment, String source, TreeSet<Integer> lineStarts) {
        final String indent = getIndent(lineComment, source, lineStarts);
        if (indent.matches("\\s+")) {
            return indent + " ";
        }
        return "";
    }

    private String getIndent(ASTNode node, String source, TreeSet<Integer> lineStarts) {
        if (lineStarts.isEmpty()) {
            return "";
        }

        final int commentStart = node.getStartPosition();
        final int previousLineStart = findPreviousLineStart(lineStarts, commentStart);
        return source.substring(previousLineStart, commentStart);
    }

    private int findPreviousLineStart(TreeSet<Integer> lineStarts, final int commentStart) {
        return lineStarts.headSet(commentStart).last();
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
