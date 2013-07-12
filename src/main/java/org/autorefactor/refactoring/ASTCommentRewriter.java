package org.autorefactor.refactoring;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ITrackedNodePosition;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.TextEdit;

public class ASTCommentRewriter {

	private static final Pattern whiteSpaces = Pattern.compile("\\s+");
	private List<ASTNode> removals = new ArrayList<ASTNode>();

	public ASTCommentRewriter() {
	}

	public void remove(ASTNode node) {
		this.removals.add(node);
	}

	public void addEdits(IDocument document, TextEdit edits) {
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
