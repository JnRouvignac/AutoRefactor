package org.autorefactor.jdt.internal.corext.dom;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;

/**
 * Methods helping with comments.
 */
public final class ASTComments {
	private ASTComments() {
	}

	/**
	 * Comments completely contained in range ordered by start position.
	 *
	 * @param start start of range
	 * @param end end of range (exclusive)
	 * @param cu compilation unit
	 * @return comments
	 */
	public static List<Comment> filterCommentsInRange(int start, int end, final CompilationUnit cu) {
		return filterCommentsInRange(start, end, cu.getCommentList());
	}

	/**
	 * Comments completely contained in range ordered by start position.
	 *
	 * @param start start of range
	 * @param end end of range (exclusive)
	 * @param commentList raw comment list
	 * @return comments
	 */
	private static List<Comment> filterCommentsInRange(final int start, final int end, final List<Comment> commentList) {
		if (commentList.isEmpty()) {
			return Collections.emptyList();
		}

		List<Comment> comments= new ArrayList<>(commentList);
		Collections.sort(comments, ASTNodes.ORDER_NODES_BY_START_POSITION);

		for (Iterator<Comment> iterator= comments.iterator(); iterator.hasNext();) {
			Comment comment= iterator.next();

			if (comment.getStartPosition() < start || SourceLocation.getEndPosition(comment) > end) {
				iterator.remove();
			}
		}

		return comments;
	}
}
