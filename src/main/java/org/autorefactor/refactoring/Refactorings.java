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
		BEFORE, AFTER
	}

	/**
	 * Describes how to insert new code: which code, before or after an existing
	 * {@link ASTNode} and which ASTNode.
	 */
	public static class Insert {

		public Insert(ASTNode nodeToInsert, ASTNode element,
				InsertType insertType) {
			this.nodeToInsert = nodeToInsert;
			this.element = element;
			this.insertType = insertType;
		}

		private final ASTNode nodeToInsert;
		private final ASTNode element;
		private final InsertType insertType;

		public ASTNode getNodeToInsert() {
			return nodeToInsert;
		}

		public ASTNode getElement() {
			return element;
		}

		public InsertType getInsertType() {
			return insertType;
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

	public void insertBefore(ASTNode node, ASTNode element) {
		insert(element, new Insert(node, element, InsertType.BEFORE));
	}

	public void insertAfter(ASTNode node, ASTNode element) {
		insert(element, new Insert(node, element, InsertType.AFTER));
	}

	private void insert(ASTNode element, Insert insert) {
		// FIXME JNR else if case
		final ChildListPropertyDescriptor clpd = (ChildListPropertyDescriptor) element
				.getLocationInParent();
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

}
