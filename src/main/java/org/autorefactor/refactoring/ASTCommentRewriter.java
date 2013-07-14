package org.autorefactor.refactoring;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;

public class ASTCommentRewriter {

	private List<ASTNode> removals = new ArrayList<ASTNode>();
	private List<BlockComment> blockCommentToJavadoc = new ArrayList<BlockComment>();
	private List<List<LineComment>> lineCommentsToJavadoc = new ArrayList<List<LineComment>>();

	public ASTCommentRewriter() {
	}

	public void remove(ASTNode node) {
		this.removals.add(node);
	}

	public void toJavadoc(BlockComment comment) {
		this.blockCommentToJavadoc.add(comment);
	}

	public void toJavadoc(List<LineComment> comments) {
		this.lineCommentsToJavadoc.add(comments);
	}

	public void addEdits(IDocument document, TextEdit edits) {
		addRemovalEdits(document, edits);
		addToJavadocEdits(document, edits);
	}

	private void addRemovalEdits(IDocument document, TextEdit edits) {
		if (this.removals.isEmpty()) {
			return;
		}
		final String text = document.get();
		for (ASTNode node : this.removals) {
			final int start = node.getStartPosition();
			final int length = node.getLength();

			// chomp from the end before the start variable gets modified
			final int startToRemove = chompWhitespacesBefore(text, start);
			final int endToRemove = chompWhitespacesAfter(text, start + length);
			final int lengthToRemove = endToRemove - startToRemove;

			edits.addChild(new DeleteEdit(startToRemove, lengthToRemove));
		}
	}

	private void addToJavadocEdits(IDocument document, TextEdit edits) {
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
			for (Iterator<LineComment> iter = lineComments.iterator(); iter
					.hasNext();) {
				LineComment lineComment = (LineComment) iter.next();
				if (isFirst) {
					edits.addChild(new ReplaceEdit(lineComment.getStartPosition(), "//".length(), "/**"));
					// TODO JNR how to obey configured indentation?
					// TODO JNR how to obey configured line length?
					isFirst = false;
				} else {
					edits.addChild(new ReplaceEdit(lineComment.getStartPosition(), "//".length(), " *"));
				}
				if (!iter.hasNext())  {
					// this was the last line comment to transform
					// TODO JNR how to get access to configured newline?
					// TODO JNR how to obey configured indentation?
					edits.addChild(new InsertEdit(lineComment.getStartPosition() + lineComment.getLength(), "\n*/"));
				}
			}
		}
	}

	private int chompWhitespacesBefore(final String text, int start) {
		int i = start - 1;
		while (i >= 0) {
			final char c = text.charAt(i);
			// TODO JNR how to get project specific newline separator??
			// Platform.PREF_LINE_SEPARATOR ??
			// See org.eclipse.ui.internal.ide.LineDelimiterEditor
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
			// TODO JNR how to get project specific newline separator??
			if (c == '\n') {
				// we chomped the newline character, do not chomp on the next line
				break;
			}
		}
		return end;
	}

}
