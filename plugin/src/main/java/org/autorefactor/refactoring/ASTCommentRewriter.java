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

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditGroup;
import org.eclipse.text.edits.TextEditVisitor;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.SourceLocation.*;

/**
 * This class rewrites AST comments.
 */
public class ASTCommentRewriter {

    /**
     * Using a Set to avoid duplicates because Javadocs are visited twice via
     * CompilationUnit.getCommentList() and visit(Javadoc).
     */
    private final Map<Comment, TextEditGroup> removals = new LinkedHashMap<Comment, TextEditGroup>();
    private final Map<Pair<Comment, String>, TextEditGroup> replacements =
            new LinkedHashMap<Pair<Comment, String>, TextEditGroup>();
    private final Map<BlockComment, TextEditGroup> blockCommentToJavadoc = new HashMap<BlockComment, TextEditGroup>();
    private final Map<ASTNode, Pair<List<LineComment>, TextEditGroup>> lineCommentsToJavadoc =
            new HashMap<ASTNode, Pair<List<LineComment>, TextEditGroup>>();

    /** Default constructor. */
    public ASTCommentRewriter() {
        super();
    }

    /**
     * Removes the provided comment.
     *
     * @param comment the comment to remove
     * @param textEditGroup the text edit group
     */
    public void remove(Comment comment, TextEditGroup textEditGroup) {
        this.removals.put(comment, textEditGroup);
    }

    /**
     * Replaces the provided comment with the provided replacement text.
     *
     * @param comment the comment to replace
     * @param replacement the replacement text
     * @param textEditGroup the text edit group
     */
    public void replace(Comment comment, String replacement, TextEditGroup textEditGroup) {
        this.replacements.put(Pair.of(comment, replacement), textEditGroup);
    }

    /**
     * Converts the provided block comment into a javadoc.
     *
     * @param comment the block comment to convert into a javadoc
     * @param textEditGroup the text edit group
     */
    public void toJavadoc(BlockComment comment, TextEditGroup textEditGroup) {
        this.blockCommentToJavadoc.put(comment, textEditGroup);
    }

    /**
     * Adds the provided line comment to convert to javadoc.
     *
     * @param lineComment the line comment to convert to javadoc
     * @param nextNode the AST node immediately following the line comment
     * @param textEditGroup the text edit group
     */
    public void toJavadoc(LineComment lineComment, ASTNode nextNode, TextEditGroup textEditGroup) {
        List<LineComment> comments;
        Pair<List<LineComment>, TextEditGroup> value = lineCommentsToJavadoc.get(nextNode);
        if (value != null) {
            comments = value.getFirst();
        } else {
            comments = new LinkedList<LineComment>();
            value = Pair.of(comments, textEditGroup);
            lineCommentsToJavadoc.put(nextNode, value);
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
            edits.addChildren(commentEdits.toArray(new TextEdit[commentEdits.size()]));
        }
        // else, code edits take priority. Give up applying current text edits.
        // They will be retried in the next refactoring loop.
    }

    private boolean anyCommentEditIsCovered(TextEdit edits, List<TextEdit> commentEdits) {
        for (TextEdit commentEdit : commentEdits) {
            if (covers(edits, commentEdit)) {
                return true;
            }
        }
        return false;
    }

    private boolean covers(TextEdit edits, TextEdit edit2) {
        final SourceLocation range = toSourceLoc(edit2);
        final AtomicBoolean overlaps = new AtomicBoolean();
        edits.accept(new TextEditVisitor() {
            @Override
            public boolean visit(MultiTextEdit edit) {
                // move on there is nothing to check here
                return VISIT_SUBTREE;
            }

            @Override
            public boolean visitNode(TextEdit edit) {
                if (!overlaps.get()) {
                    overlaps.set(overlapsWith(range, edit));
                }
                return !overlaps.get();
            }

            private boolean overlapsWith(SourceLocation loc1, TextEdit edit) {
                return loc1.overlapsWith(toSourceLoc(edit));
            }
        });
        return overlaps.get();
    }

    private SourceLocation toSourceLoc(TextEdit edit) {
        return new SourceLocation(edit.getOffset(), edit.getLength());
    }

    private void addTextEdit(List<TextEdit> edits, TextEditGroup textEditGroup, TextEdit textEdit) {
        edits.add(textEdit);
        textEditGroup.addTextEdit(textEdit);
    }

    private void addRemovalEdits(List<TextEdit> commentEdits, String source) {
        if (this.removals.isEmpty()) {
            return;
        }
        for (Entry<Comment, TextEditGroup> entry : this.removals.entrySet()) {
            final Comment node = entry.getKey();
            final TextEditGroup textEditGroup = entry.getValue();
            final int start = node.getStartPosition();
            final int length = node.getLength();

            // chomp from the end before the start variable gets modified
            final int startToRemove = chompWhitespacesBefore(source, start);
            final int endToRemove = chompWhitespacesAfter(source, start + length);
            final int lengthToRemove = endToRemove - startToRemove;

            DeleteEdit textEdit = new DeleteEdit(startToRemove, lengthToRemove);
            addTextEdit(commentEdits, textEditGroup, textEdit);
        }
    }

    private void addReplacementEdits(List<TextEdit> commentEdits) {
        if (this.replacements.isEmpty()) {
            return;
        }
        for (Entry<Pair<Comment, String>, TextEditGroup> entry : this.replacements.entrySet()) {
            final Pair<Comment, String> pair = entry.getKey();
            final TextEditGroup textEditGroup = entry.getValue();
            final Comment node = pair.getFirst();
            final int start = node.getStartPosition();
            final int length = node.getLength();
            ReplaceEdit textEdit = new ReplaceEdit(start, length, pair.getSecond());
            addTextEdit(commentEdits, textEditGroup, textEdit);
        }
    }

    private void addBlockCommentToJavadocEdits(List<TextEdit> commentEdits) {
        for (Entry<BlockComment, TextEditGroup> entry : this.blockCommentToJavadoc.entrySet()) {
            final BlockComment blockComment = entry.getKey();
            final TextEditGroup textEditGroup = entry.getValue();
            final int offset = blockComment.getStartPosition() + "/*".length();
            final InsertEdit textEdit = new InsertEdit(offset, "*");
            addTextEdit(commentEdits, textEditGroup, textEdit);
        }
    }

    private void addLineCommentsToJavadocEdits(List<TextEdit> commentEdits, String source) {
        if (this.lineCommentsToJavadoc.isEmpty()) {
            return;
        }
        final TreeSet<Integer> lineStarts = getLineStarts(source);
        for (Entry<ASTNode, Pair<List<LineComment>, TextEditGroup>> entry : this.lineCommentsToJavadoc.entrySet()) {
            final ASTNode node = entry.getKey();
            final Pair<List<LineComment>, TextEditGroup> pair = entry.getValue();
            final List<LineComment> lineComments = pair.getFirst();
            final TextEditGroup textEditGroup = pair.getSecond();
            // TODO Collect all words from the line comments,
            // then get access to indent settings, line length and newline chars
            // then spread them across several lines if needed or folded on one line only
            if (lineComments.size() == 1) {
                addSingleLineCommentToJavadocEdits(commentEdits, node, lineComments, source, lineStarts, textEditGroup);
            } else {
                addMultiLineCommentsToJavadocEdits(commentEdits, node, lineComments, source, lineStarts, textEditGroup);
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

    private void addSingleLineCommentToJavadocEdits(List<TextEdit> commentEdits, ASTNode nextNode,
            List<LineComment> lineComments, String source, TreeSet<Integer> lineStarts, TextEditGroup textEditGroup) {
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
            final TextEdit textEdit1 = new ReplaceEdit(commentStart, "//".length(), "/**" + spaceAtStart);
            final TextEdit textEdit2 = new InsertEdit(commentStart + commentLength, spaceAtEnd + "*/");
            addTextEdit(commentEdits, textEditGroup, textEdit1);
            addTextEdit(commentEdits, textEditGroup, textEdit2);
        } else {
            // assume comment is situated exactly after target node for javadoc
            final String indent = getIndent(nextNode, source, lineStarts);
            final String newJavadoc = "/**" + spaceAtStart + commentText + spaceAtEnd + "*/\r\n" + indent;
            final TextEdit textEdit = new InsertEdit(nodeStart, newJavadoc);
            addTextEdit(commentEdits, textEditGroup, textEdit);
            deleteLineCommentAfterNode(commentEdits, source, lineComment, textEditGroup);
        }
    }

    private void deleteLineCommentAfterNode(List<TextEdit> commentEdits,
            String source, LineComment lineComment, TextEditGroup textEditGroup) {
        final int commentStart = lineComment.getStartPosition();
        final int commentLength = lineComment.getLength();
        final int nbWhiteSpaces = nbTrailingSpaces(source, commentStart);
        final TextEdit textEdit = new DeleteEdit(commentStart - nbWhiteSpaces, nbWhiteSpaces + commentLength);
        addTextEdit(commentEdits, textEditGroup, textEdit);
    }

    private int nbTrailingSpaces(String source, int commentStart) {
        int result = 0;
        while (Character.isWhitespace(source.charAt(commentStart - result - 1))) {
            ++result;
        }
        return result;
    }

    private void addMultiLineCommentsToJavadocEdits(List<TextEdit> commentEdits, ASTNode node,
            List<LineComment> lineComments, String source, TreeSet<Integer> lineStarts, TextEditGroup textEditGroup) {
        final String newline = "\n";
        for (int i = 0; i < lineComments.size(); i++) {
            final LineComment lineComment = lineComments.get(i);
            if (lineComment.getStartPosition() <= node.getStartPosition()) {
                replaceLineCommentBeforeJavaElement(
                        commentEdits, lineComment, lineComments, i, source, lineStarts, newline, textEditGroup);
            } else {
                replaceLineCommentAfterJavaElement(
                        commentEdits, lineComment, lineComments, i, source, lineStarts, newline, textEditGroup);
            }
        }
    }

    private void replaceLineCommentBeforeJavaElement(List<TextEdit> commentEdits,
            LineComment lineComment, List<LineComment> lineComments, int i,
            String source, TreeSet<Integer> lineStarts, String newline, TextEditGroup textEditGroup) {
        final boolean isFirst = i == 0;
        final String replacementText;
        if (isFirst) {
            // TODO JNR how to get access to configured newline? @see #getNewline();
            // TODO JNR how to obey configured indentation?
            replacementText = "/**" + newline + getIndentForJavadoc(lineComment, source, lineStarts) + "*";
        } else {
            replacementText = " *";
        }
        final TextEdit textEdit1 = new ReplaceEdit(lineComment.getStartPosition(), "//".length(), replacementText);
        addTextEdit(commentEdits, textEditGroup, textEdit1);

        final boolean isLast = i == lineComments.size() - 1;
        if (isLast) {
            // TODO JNR how to get access to configured newline? @see #getNewline();
            // TODO JNR how to obey configured indentation?
            final int position = getEndPosition(lineComment);
            final String indent = getIndentForJavadoc(lineComment, source, lineStarts);
            final TextEdit textEdit2 = new InsertEdit(position, newline + indent + "*/");
            addTextEdit(commentEdits, textEditGroup, textEdit2);
        }
    }

    private void replaceLineCommentAfterJavaElement(List<TextEdit> commentEdits,
            LineComment lineComment, List<LineComment> lineComments, int i,
            String source, TreeSet<Integer> lineStarts, String newline, TextEditGroup textEditGroup) {
        if (i - 1 < 0) {
            throw new NotImplementedException(lineComment,
                    "for a line comment situated after the java elements that it documents,"
                            + " and this line comment is not the last line comment to add to the javadoc.");
        }

        final String commentText = source.substring(
                lineComment.getStartPosition() + "//".length(), getEndPosition(lineComment));
        final LineComment previousLineComment = lineComments.get(i - 1);
        final int position = getEndPosition(previousLineComment);
        final String indent = getIndentForJavadoc(previousLineComment, source, lineStarts);
        final TextEdit textEdit = new InsertEdit(position,
                newline + indent + " *" + commentText
                + newline + indent + " */");
        addTextEdit(commentEdits, textEditGroup, textEdit);
        deleteLineCommentAfterNode(commentEdits, source, lineComment, textEditGroup);
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
