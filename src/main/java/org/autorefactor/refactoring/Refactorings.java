/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;

/**
 * Class aggregating all the refactorings performed by a refactoring rule until
 * the rule finished traversing the whole AST tree.
 */
public class Refactorings {

	/**
	 * The refactorings replacing existing code from the AST tree.
	 */
	private final List<Pair<ASTNode, ASTNode>> replacements = new LinkedList<Pair<ASTNode, ASTNode>>();
	/**
	 * The refactorings inserting code into the AST tree.
	 */
	private final Map<ChildListPropertyDescriptor, List<Insert>> inserts = new LinkedHashMap<ChildListPropertyDescriptor, List<Insert>>();
	/**
	 * The refactorings removing code from the AST tree.
	 */
	private final List<ASTNode> removals = new LinkedList<ASTNode>();
	private final List<Comment> commentRemovals = new LinkedList<Comment>();
	private final Map<ASTNode, List<LineComment>> lineCommentsToJavadoc = new HashMap<ASTNode, List<LineComment>>();
	private final List<BlockComment> blockCommentToJavadoc = new LinkedList<BlockComment>();

	/**
	 * Describes where to insert new code: before or after an existing
	 * {@link ASTNode}.
	 */
	public static enum InsertType {
		AT_INDEX, BEFORE, AFTER
	}

	/**
	 * Describes how to insert new code: which code, before or after an existing
	 * {@link ASTNode} and which ASTNode.
	 */
	public static class Insert {

		public Insert(ASTNode nodeToInsert, ASTNode element, InsertType insertType) {
			this.nodeToInsert = nodeToInsert;
			this.insertType = insertType;
			this.element = element;
			this.listHolder = element.getParent();
			this.index = 0;
		}

		public Insert(ASTNode nodeToInsert, ASTNode listHolder, int index) {
			this.nodeToInsert = nodeToInsert;
			this.insertType = InsertType.AT_INDEX;
			this.element = null;
			this.listHolder = listHolder;
			this.index = index;
		}

		private final ASTNode nodeToInsert;
		private final InsertType insertType;
		private final ASTNode element;
		private final ASTNode listHolder;
		private final int index;

		public ASTNode getNodeToInsert() {
			return nodeToInsert;
		}

		public ASTNode getElement() {
			return element;
		}

		public InsertType getInsertType() {
			return insertType;
		}

		public ASTNode getListHolder() {
			return listHolder;
		}

		public int getIndex() {
			return index;
		}
	}

	public List<Pair<ASTNode, ASTNode>> getReplacements() {
		return this.replacements;
	}

	public List<ASTNode> getRemovals() {
		return removals;
	}

	public Map<ChildListPropertyDescriptor, List<Insert>> getInserts() {
		return this.inserts;
	}

	public List<Comment> getCommentRemovals() {
		return this.commentRemovals;
	}

	public List<BlockComment> getBlockCommentToJavadoc() {
		return blockCommentToJavadoc;
	}

	public Collection<List<LineComment>> getLineCommentsToJavadoc() {
		return lineCommentsToJavadoc.values();
	}

	public void replace(ASTNode node, ASTNode replacement) {
		this.replacements.add(Pair.of(node, replacement));
	}

	public void remove(ASTNode node) {
		if (node instanceof Comment) {
			this.commentRemovals.add((Comment) node);
		} else {
			this.removals.add(node);
		}
	}

	public void remove(ASTNode... nodes) {
		remove(Arrays.asList(nodes));
	}

	public void remove(Collection<ASTNode> nodes) {
		for (ASTNode node : nodes) {
			remove(node);
		}
	}

	public boolean hasRefactorings() {
		return !this.replacements.isEmpty() || !this.removals.isEmpty()
				|| !this.inserts.isEmpty() || !this.commentRemovals.isEmpty()
				|| !this.lineCommentsToJavadoc.isEmpty()
				|| !this.blockCommentToJavadoc.isEmpty();
	}

	public void insertAt(ASTNode nodeToInsert, int index, StructuralPropertyDescriptor locationInParent, ASTNode listHolder) {
		insert(locationInParent, new Insert(nodeToInsert, listHolder, index));
	}

	public void insertBefore(ASTNode nodeToInsert, ASTNode element) {
		insert(element.getLocationInParent(), new Insert(nodeToInsert, element, InsertType.BEFORE));
	}

	public void insertAfter(ASTNode nodeToInsert, ASTNode element) {
		insert(element.getLocationInParent(), new Insert(nodeToInsert, element, InsertType.AFTER));
	}

	private void insert(StructuralPropertyDescriptor locationInParent, Insert insert) {
		// FIXME JNR else if case
		final ChildListPropertyDescriptor clpd = (ChildListPropertyDescriptor) locationInParent;
		List<Insert> inserts = this.inserts.get(clpd);
		if (inserts == null) {
			inserts = new LinkedList<Insert>();
			this.inserts.put(clpd, inserts);
		}
		inserts.add(insert);
	}

	public void toJavadoc(LineComment lineComment, ASTNode nextNode) {
		List<LineComment> comments = this.lineCommentsToJavadoc.get(nextNode);
		if (comments == null) {
			comments = new LinkedList<LineComment>();
			this.lineCommentsToJavadoc.put(nextNode, comments);
		}
		comments.add(lineComment);
	}

	public void toJavadoc(BlockComment blockComment) {
		this.blockCommentToJavadoc.add(blockComment);
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		append(sb, replacements.size(), "replacements");
		append(sb, inserts.size(), "inserts");
		append(sb, removals.size(), "removals");
		append(sb, commentRemovals.size(), "commentRemovals");
		append(sb, lineCommentsToJavadoc.size(), "lineCommentsToJavadoc");
		append(sb, blockCommentToJavadoc.size(), "blockCommentToJavadoc");
		return sb.toString();
	}

	private void append(StringBuilder sb, int size, String s) {
		if (size > 0) {
			if (sb.length() > 0) {
				sb.append(", ");
			}
			sb.append(size).append(" ").append(s);
		}
	}

}
