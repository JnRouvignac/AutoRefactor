/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

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
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditVisitor;

/** This class rewrites AST comments. */
public class ASTCommentRewriter {
	private static final Pattern INDENT= Pattern.compile("\\s+"); //$NON-NLS-1$
	/**
	 * Using a Set to avoid duplicates because Javadocs are visited twice via
	 * CompilationUnit.getCommentList() and visit(Javadoc).
	 */
	private final Set<Comment> removals= new LinkedHashSet<>();
	private final Set<Pair<Comment, String>> replacements= new LinkedHashSet<>();
	private final List<BlockComment> blockCommentToJavadoc= new ArrayList<>();
	private final Map<ASTNode, List<LineComment>> lineCommentsToJavadoc= new HashMap<>();
	private final String lineSeparator;

	/**
	 * Default constructor.
	 *
	 * @param astRoot the compilation unit, root of the AST
	 */
	public ASTCommentRewriter(final CompilationUnit astRoot) {
		this.lineSeparator= getLineSeparator(astRoot);
	}

	private String getLineSeparator(final CompilationUnit astRoot) {
		String result= findRecommendedLineSeparator(astRoot);
		return result != null ? result : System.getProperty("line.separator"); //$NON-NLS-1$
	}

	private String findRecommendedLineSeparator(final CompilationUnit astRoot) {
		try {
			return astRoot.getTypeRoot().findRecommendedLineSeparator();
		} catch (JavaModelException e) {
			throw new UnhandledException(astRoot, e);
		}
	}

	/**
	 * Removes the provided comment.
	 *
	 * @param comment the comment to remove
	 */
	public void remove(final Comment comment) {
		this.removals.add(comment);
	}

	/**
	 * Replaces the provided comment with the provided replacement text.
	 *
	 * @param comment     the comment to replace
	 * @param replacement the replacement text
	 */
	public void replace(final Comment comment, final String replacement) {
		this.replacements.add(Pair.of(comment, replacement));
	}

	/**
	 * Converts the provided block comment into a javadoc.
	 *
	 * @param comment the block comment to convert into a javadoc
	 */
	public void toJavadoc(final BlockComment comment) {
		this.blockCommentToJavadoc.add(comment);
	}

	/**
	 * Adds the provided line comment to convert to javadoc.
	 *
	 * @param lineComment the line comment to convert to javadoc
	 * @param nextNode    the AST node immediately following the line comment
	 */
	public void toJavadoc(final LineComment lineComment, final ASTNode nextNode) {
		List<LineComment> comments= lineCommentsToJavadoc.get(nextNode);
		if (comments == null) {
			comments= new LinkedList<>();
			lineCommentsToJavadoc.put(nextNode, comments);
		}
		comments.add(lineComment);
	}

	/**
	 * Adds the edits contained in the current instance to the provided edits for
	 * the provided document.
	 *
	 * @param document the provided document to edit
	 * @param edits    where to add edits
	 */
	public void addEdits(final IDocument document, final TextEdit edits) {
		String source= document.get();
		List<TextEdit> commentEdits= new ArrayList<>(nbEdits());
		addRemovalEdits(commentEdits, source);
		addReplacementEdits(commentEdits);
		addBlockCommentToJavadocEdits(commentEdits);
		addLineCommentsToJavadocEdits(commentEdits, source);
		if (!commentEdits.isEmpty() && !anyOverlaps(edits, commentEdits)) {
			edits.addChildren(commentEdits.toArray(new TextEdit[commentEdits.size()]));
		}
		// Else, code edits take priority. Give up applying current text edits.
		// They will be retried in the next cleanup loop.
	}

	private int nbEdits() {
		return removals.size() + replacements.size() + blockCommentToJavadoc.size() + lineCommentsToJavadoc.size();
	}

	private boolean anyOverlaps(final TextEdit edits, final List<TextEdit> commentEdits) {
		for (TextEdit commentEdit : commentEdits) {
			if (overlaps(edits, commentEdit)) {
				return true;
			}
		}

		return false;
	}

	private boolean overlaps(final TextEdit edits, final TextEdit edit2) {
		final SourceLocation range= toSourceLoc(edit2);
		final AtomicBoolean overlaps= new AtomicBoolean();
		edits.accept(new TextEditVisitor() {
			@Override
			public boolean visit(final MultiTextEdit edit) {
				// Move on there is nothing to check here
				return true;
			}

			@Override
			public boolean visitNode(final TextEdit edit) {
				if (!overlaps.get()) {
					overlaps.lazySet(range.overlapsWith(toSourceLoc(edit)));
				}

				return !overlaps.get();
			}
		});
		return overlaps.get();
	}

	private SourceLocation toSourceLoc(final TextEdit edit) {
		return new SourceLocation(edit.getOffset(), edit.getLength());
	}

	private void addRemovalEdits(final List<TextEdit> commentEdits, final String source) {
		if (this.removals.isEmpty()) {
			return;
		}
		for (Comment node : this.removals) {
			int start= node.getStartPosition();
			int length= node.getLength();

			// Chomp from the end before the start variable gets modified
			int startToRemove= chompWhitespacesBefore(source, start);
			int endToRemove= chompWhitespacesAfter(source, start + length);
			int lengthToRemove= endToRemove - startToRemove;

			commentEdits.add(new DeleteEdit(startToRemove, lengthToRemove));
		}
	}

	private void addReplacementEdits(final List<TextEdit> commentEdits) {
		if (this.replacements.isEmpty()) {
			return;
		}
		for (Pair<Comment, String> pair : this.replacements) {
			Comment node= pair.getFirst();
			int start= node.getStartPosition();
			int length= node.getLength();
			commentEdits.add(new ReplaceEdit(start, length, pair.getSecond()));
		}
	}

	private void addBlockCommentToJavadocEdits(final List<TextEdit> commentEdits) {
		for (BlockComment blockComment : this.blockCommentToJavadoc) {
			int offset= blockComment.getStartPosition() + "/*".length(); //$NON-NLS-1$
			commentEdits.add(new InsertEdit(offset, "*")); //$NON-NLS-1$
		}
	}

	private void addLineCommentsToJavadocEdits(final List<TextEdit> commentEdits, final String source) {
		if (this.lineCommentsToJavadoc.isEmpty()) {
			return;
		}
		TreeSet<Integer> lineStarts= getLineStarts(source);
		for (Entry<ASTNode, List<LineComment>> entry : this.lineCommentsToJavadoc.entrySet()) {
			List<LineComment> lineComments= entry.getValue();
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

	private TreeSet<Integer> getLineStarts(final String source) {
		TreeSet<Integer> lineStarts= new TreeSet<>();
		lineStarts.add(0);

		Matcher matcher= Pattern.compile("\\rewrite\\n|\\rewrite|\\n").matcher(source); //$NON-NLS-1$
		while (matcher.find()) {
			lineStarts.add(matcher.end());
		}

		return lineStarts;
	}

	private void addSingleLineCommentToJavadocEdits(final List<TextEdit> commentEdits, final ASTNode nextNode,
			final List<LineComment> lineComments, final String source, final TreeSet<Integer> lineStarts) {
		int nodeStart= nextNode.getStartPosition();
		LineComment lineComment= lineComments.get(0);

		// TODO JNR how to obey configured indentation?
		// TODO JNR how to obey configured line length?
		int commentStart= lineComment.getStartPosition();
		if (commentStart < nodeStart) {
			// Assume comment is situated exactly before target node for javadoc
			String spaceAtStart= getSpaceAtStart(source, lineComment);
			commentEdits.add(new ReplaceEdit(commentStart, "//".length(), "/**" + spaceAtStart)); //$NON-NLS-1$ //$NON-NLS-2$
			commentEdits.add(new InsertEdit(SourceLocation.getEndPosition(lineComment), getSpaceAtEnd(source, lineComment) + "*/")); //$NON-NLS-1$
			replaceEndsOfBlockCommentFromCommentText(commentEdits, lineComment, source);
		} else {
			// Assume comment is situated exactly after target node for javadoc
			StringBuilder newJavadoc= new StringBuilder("/**").append(getSpaceAtStart(source, lineComment)); //$NON-NLS-1$

			appendCommentTextReplaceEndsOfBlockComment(newJavadoc, lineComment, source);

			SourceLocation indent= getIndent(nextNode, lineStarts);
			newJavadoc.append(getSpaceAtEnd(source, lineComment)).append("*/").append(lineSeparator).append(source, //$NON-NLS-1$
					indent.getStartPosition(), indent.getEndPosition());
			commentEdits.add(new InsertEdit(nodeStart, newJavadoc.toString()));
			deleteLineCommentAfterNode(commentEdits, source, lineComment);
		}
	}

	private void appendCommentTextReplaceEndsOfBlockComment(final StringBuilder sb, final LineComment lineComment, final String source) {
		int commentStart= lineComment.getStartPosition();
		int nextStart= commentStart + "//".length(); //$NON-NLS-1$
		Matcher matcher= endsOfBlockCommentMatcher(lineComment, source, nextStart);
		while (matcher.find()) {
			sb.append(source, nextStart, matcher.start());
			sb.append("* /"); //$NON-NLS-1$
			nextStart= matcher.end();
		}
		if (source.charAt(nextStart) == '/') {
			sb.append(' ');
		}
		sb.append(source, nextStart, SourceLocation.getEndPosition(lineComment));
	}

	private String getSpaceAtStart(final String source, final LineComment lineComment) {
		char firstChar= source.charAt(lineComment.getStartPosition() + "//".length()); //$NON-NLS-1$
		return !Character.isWhitespace(firstChar) ? " " : ""; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private String getSpaceAtEnd(final String source, final LineComment lineComment) {
		char lastChar= source.charAt(SourceLocation.getEndPosition(lineComment) - 1);
		return !Character.isWhitespace(lastChar) ? " " : ""; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private void deleteLineCommentAfterNode(final List<TextEdit> commentEdits, final String source, final LineComment lineComment) {
		int commentStart= lineComment.getStartPosition();
		int commentLength= lineComment.getLength();
		int nbWhiteSpaces= nbTrailingSpaces(source, commentStart);
		commentEdits.add(new DeleteEdit(commentStart - nbWhiteSpaces, nbWhiteSpaces + commentLength));
	}

	private int nbTrailingSpaces(final String source, final int commentStart) {
		int result= 0;
		while (Character.isWhitespace(source.charAt(commentStart - result - 1))) {
			++result;
		}

		return result;
	}

	private void addMultiLineCommentsToJavadocEdits(final List<TextEdit> commentEdits, final ASTNode node,
			final List<LineComment> lineComments, final String source, final TreeSet<Integer> lineStarts) {
		for (int i= 0; i < lineComments.size(); i++) {
			LineComment lineComment= lineComments.get(i);
			if (lineComment.getStartPosition() <= node.getStartPosition()) {
				replaceLineCommentBeforeJavaElement(commentEdits, lineComment, lineComments, i, source, lineStarts);
			} else {
				replaceLineCommentAfterJavaElement(commentEdits, lineComment, lineComments, i, source, lineStarts);
			}
		}
	}

	private void replaceLineCommentBeforeJavaElement(final List<TextEdit> commentEdits, final LineComment lineComment,
			final List<LineComment> lineComments, final int i, final String source, final TreeSet<Integer> lineStarts) {
		int replaceLength= "//".length(); //$NON-NLS-1$
		boolean isFirst= i == 0;
		String replacementText;
		SourceLocation indentLoc= getIndentForJavadoc(lineComment, source, lineStarts);
		if (isFirst) {
			// TODO JNR how to obey configured indentation?
			replacementText= "/**" + lineSeparator + indentLoc.substring(source) + " *"; //$NON-NLS-1$ //$NON-NLS-2$
		} else {
			replacementText= " *"; //$NON-NLS-1$
		}
		boolean commentStartsWithSlash= source.charAt(lineComment.getStartPosition() + replaceLength) == '/';
		if (commentStartsWithSlash) {
			replacementText+= " "; //$NON-NLS-1$
		}
		commentEdits.add(new ReplaceEdit(lineComment.getStartPosition(), replaceLength, replacementText));

		replaceEndsOfBlockCommentFromCommentText(commentEdits, lineComment, source);

		boolean isLast= i == lineComments.size() - 1;
		if (isLast) {
			// TODO JNR how to obey configured indentation?
			int position= SourceLocation.getEndPosition(lineComment);
			commentEdits.add(new InsertEdit(position, lineSeparator + indentLoc.substring(source) + " */")); //$NON-NLS-1$
		}
	}

	private void replaceEndsOfBlockCommentFromCommentText(final List<TextEdit> commentEdits, final LineComment lineComment,
			final String source) {
		Matcher matcher= endsOfBlockCommentMatcher(lineComment, source, lineComment.getStartPosition());
		while (matcher.find()) {
			commentEdits.add(new ReplaceEdit(matcher.start(), matcher.end() - matcher.start(), "* /")); //$NON-NLS-1$
		}
	}

	private Matcher endsOfBlockCommentMatcher(final LineComment lineComment, final String source, final int startPos) {
		return Pattern.compile("\\*/").matcher(source).region(startPos, SourceLocation.getEndPosition(lineComment)); //$NON-NLS-1$
	}

	private void replaceLineCommentAfterJavaElement(final List<TextEdit> commentEdits, final LineComment lineComment,
			final List<LineComment> lineComments, final int i, final String source, final TreeSet<Integer> lineStarts) {
		if (i - 1 < 0) {
			throw new NotImplementedException(lineComment,
					"for a line comment situated after the java elements that it documents," //$NON-NLS-1$
							+ " and this line comment is not the last line comment to add to the javadoc."); //$NON-NLS-1$
		}

		LineComment previousLineComment= lineComments.get(i - 1);
		int position= SourceLocation.getEndPosition(previousLineComment);
		String indent= getIndentForJavadoc(previousLineComment, source, lineStarts).substring(source);
		StringBuilder newJavadoc= new StringBuilder(lineSeparator).append(indent).append(" *"); //$NON-NLS-1$

		appendCommentTextReplaceEndsOfBlockComment(newJavadoc, lineComment, source);

		newJavadoc.append(lineSeparator).append(indent).append(" */"); //$NON-NLS-1$
		commentEdits.add(new InsertEdit(position, newJavadoc.toString()));
		deleteLineCommentAfterNode(commentEdits, source, lineComment);
	}

	private SourceLocation getIndentForJavadoc(final LineComment lineComment, final String source, final TreeSet<Integer> lineStarts) {
		SourceLocation indentLoc= getIndent(lineComment, lineStarts);
		Matcher matcher= INDENT.matcher(source).region(indentLoc.getStartPosition(), indentLoc.getEndPosition());
		if (matcher.matches()) {
			return indentLoc;
		}

		return SourceLocation.fromPositions(0, 0);
	}

	private SourceLocation getIndent(final ASTNode node, final TreeSet<Integer> lineStarts) {
		if (lineStarts.isEmpty()) {
			// No match, return empty range
			return SourceLocation.fromPositions(0, 0);
		}

		int commentStart= node.getStartPosition();
		int previousLineStart= findPreviousLineStart(lineStarts, commentStart);
		return SourceLocation.fromPositions(previousLineStart, commentStart);
	}

	private int findPreviousLineStart(final TreeSet<Integer> lineStarts, final int commentStart) {
		return lineStarts.headSet(commentStart + 1).last();
	}

	private int chompWhitespacesBefore(final String text, int start) {
		int i= start - 1;
		while (i >= 0) {
			char c= text.charAt(i);
			// TODO JNR how to get project specific newline separator?? @see #lineSeparator
			if (!Character.isWhitespace(c) || c == '\n') {
				break;
			}
			start= i;
			i--;
		}

		return start;
	}

	private int chompWhitespacesAfter(final String text, int end) {
		int i= end;
		while (i < text.length()) {
			char c= text.charAt(i);
			if (!Character.isWhitespace(c)) {
				break;
			}
			i++;
			end= i;
			// TODO JNR how to get project specific newline separator?? @see #lineSeparator
			if (c == '\n') {
				// We chomped the newline character, do not chomp on the next line
				break;
			}
		}

		return end;
	}
}
