/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2017 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.core.dom;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;

import org.autorefactor.environment.EventLoop;
import org.autorefactor.jdt.internal.corext.dom.ASTCommentRewriter;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.SourceLocation;
import org.autorefactor.jdt.internal.corext.dom.SourceRewriter;
import org.autorefactor.util.Pair;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.rewrite.ImportRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jdt.core.dom.rewrite.TargetSourceRangeComputer;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Class aggregating all the refactorings performed by a cleanup rule until
 * the rule finished traversing the whole AST tree.
 */
public class ASTRewrite {
	private static final String UNTOUCH_COMMENT= "untouchComment"; //$NON-NLS-1$

	private final EventLoop eventLoop;
	private final SubMonitor monitor;
	private final org.eclipse.jdt.core.dom.rewrite.ASTRewrite rewrite;
	private final ImportRewrite importRewrite;
	private TextEdit edits;
	private final Map<Pair<ASTNode, ChildListPropertyDescriptor>, ListRewrite> listRewriteCache= new HashMap<>();
	private final ASTCommentRewriter commentRewriter;
	private final SourceRewriter sourceRewriter= new SourceRewriter();
	/** Nodes that cannot be visited. */
	private final Set<ASTNode> refactoredNodes= new HashSet<>();

	private boolean hasRefactorings;

	/**
	 * Builds an instance of this class.
	 *
	 * @param astRoot   the compilation unit, root of the AST
	 * @param eventLoop the event loop
	 * @param monitor   The monitor
	 */
	public ASTRewrite(final CompilationUnit astRoot, final EventLoop eventLoop, final SubMonitor monitor) {
		this.eventLoop= eventLoop;
		this.monitor= monitor;
		this.rewrite= org.eclipse.jdt.core.dom.rewrite.ASTRewrite.create(astRoot.getAST());
		this.rewrite.setTargetSourceRangeComputer(new TargetSourceRangeComputer() {
			@Override
			public SourceRange computeSourceRange(final ASTNode node) {
				if (Boolean.TRUE.equals(node.getProperty(UNTOUCH_COMMENT))) {
					return new SourceRange(node.getStartPosition(), node.getLength());
				}

				return super.computeSourceRange(node);
			}
		});

		this.importRewrite= ImportRewrite.create(astRoot, true);

		this.commentRewriter= new ASTCommentRewriter(astRoot);
	}

	/**
	 * Returns the AST.
	 *
	 * @return the AST
	 */
	public AST getAST() {
		return rewrite.getAST();
	}

	/**
	 * Returns whether the provided node has been the target of a cleanup.
	 *
	 * @param node the node for which to make the determination
	 * @return true if the provided node has been refactored, false otherwise
	 */
	public boolean hasBeenRefactored(final ASTNode node) {
		return refactoredNodes.contains(node);
	}

	private void addRefactoredNodes(final ASTNode node) {
		hasRefactorings= true;
		refactoredNodes.add(node);
		if (node.getParent() != null) {
			addRefactoredNodes(node.getParent());
		}
	}

	/**
	 * Creates and returns a placeholder node for a copy of the source code of the
	 * provided node.<br>
	 * The placeholder node can be used like any new node created via the AST
	 * class.<br>
	 * When the document is rewritten, a copy of the source code for the provided
	 * node is inserted into the output document at the position corresponding to
	 * the placeholder (indentation is adjusted).
	 *
	 * @param <T>  the type of the provided node
	 * @param node the node for which to create a copy placeholder
	 * @return the new placeholder node
	 * @see ASTRewrite#createCopyTarget(ASTNode)
	 */
	@SuppressWarnings("unchecked")
	public <T extends ASTNode> T createCopyTarget(final T node) {
		return (T) rewrite.createCopyTarget(node);
	}

	/**
	 * Creates and returns a placeholder node for a copy of the source code of the
	 * provided range of nodes.<br>
	 * The placeholder node can be used like any new node created via the AST
	 * class.<br>
	 * When the document is rewritten, a copy of the source code for the provided
	 * range of nodes is inserted into the output document at the position
	 * corresponding to the placeholder (indentation is adjusted).
	 *
	 * @param <T>   the type of the provided nodes
	 * @param first the first node of the range
	 * @param last  the first node of the range
	 * @return the new placeholder node
	 * @see ListRewrite#createCopyTarget(ASTNode, ASTNode)
	 */
	@SuppressWarnings("unchecked")
	public <T extends ASTNode> T createCopyTarget(final T first, final T last) {
		return (T) getListRewrite(first).createCopyTarget(first, last);
	}

	private ListRewrite getListRewrite(final ASTNode child) {
		return getListRewrite(child.getParent(), (ChildListPropertyDescriptor) child.getLocationInParent());
	}

	private ListRewrite getListRewrite(final ASTNode listHolder, final StructuralPropertyDescriptor locationInParent) {
		return getListRewrite(listHolder, (ChildListPropertyDescriptor) locationInParent);
	}

	private ListRewrite getListRewrite(final ASTNode node, final ChildListPropertyDescriptor listProperty) {
		Pair<ASTNode, ChildListPropertyDescriptor> key= Pair.of(node, listProperty);
		ListRewrite listRewrite= listRewriteCache.get(key);
		if (listRewrite == null) {
			listRewrite= rewrite.getListRewrite(node, listProperty);
			listRewriteCache.put(key, listRewrite);
		}

		return listRewrite;
	}

	/**
	 * Creates and returns a placeholder node where to move the source code of the
	 * provided node.<br>
	 * The placeholder node can be used like any new node created via the AST
	 * class.<br>
	 * When the document is rewritten, the source code for the provided node is
	 * inserted into the output document at the position corresponding to the
	 * placeholder (indentation is adjusted) and it is removed from the old
	 * location.
	 *
	 * @param <T>  the type of the provided node
	 * @param node the node for which to create a move placeholder
	 * @return the new placeholder node
	 * @see ASTRewrite#createMoveTarget(ASTNode)
	 */
	@SuppressWarnings("unchecked")
	public <T extends ASTNode> T createMoveTarget(final T node) {
		return (T) rewrite.createMoveTarget(node);
	}

	/**
	 * Moves all the provided {@link ASTNode}s in place.
	 *
	 * @param <T>   the actual nodes type
	 * @param nodes the nodes to move
	 * @return the provided list with all nodes moved
	 */
	public <T extends ASTNode> List<T> createMoveTarget(final Collection<T> nodes) {
		List<T> movedNodes= new ArrayList<>(nodes.size());

		for (T astNode : nodes) {
			movedNodes.add(ASTNodes.createMoveTarget(this, astNode));
		}

		return movedNodes;
	}

	/**
	 * Creates and returns a placeholder node for a move of a range of nodes of the
	 * current list.<br>
	 * The placeholder node can either be inserted as new or used to replace an
	 * existing node.<br>
	 * When the document is rewritten, a copy of the source code for the given node
	 * range is inserted into the output document at the position corresponding to
	 * the placeholder (indentation is adjusted).
	 *
	 * @param <T>   the type of the provided node
	 * @param first the node that starts the range
	 * @param last  the node that ends the range
	 * @return the new placeholder node
	 * @throws IllegalArgumentException An exception is thrown if the first or last
	 *                                  node are null, if a node is not a child of
	 *                                  the current list or if the first node is not
	 *                                  before the last node. An
	 *                                  IllegalArgumentException is also thrown if
	 *                                  the moved range is overlapping with an other
	 *                                  moved or copied range.
	 * @see ListRewrite#createMoveTarget(ASTNode, ASTNode)
	 */
	@SuppressWarnings("unchecked")
	public <T extends ASTNode> T createMoveTarget(final T first, final T last) {
		return (T) getListRewrite(first).createMoveTarget(first, last);
	}

	/**
	 * Returns whether the provided nodes are a valid existing range.
	 *
	 * @param nodes the node range to validate
	 * @return true if the provided nodes are a valid existing range, false
	 *         otherwise
	 */
	public boolean isValidRange(final List<? extends ASTNode> nodes) {
		if (nodes.isEmpty()) {
			return true;
		}
		@SuppressWarnings("unchecked") List<ASTNode> originalList= getListRewrite(nodes.get(0)).getOriginalList();
		Iterator<ASTNode> origIter= originalList.iterator();
		while (origIter.hasNext()) {
			ASTNode origNode= origIter.next();
			Iterator<? extends ASTNode> currIter= nodes.iterator();
			while (currIter.hasNext()) {
				ASTNode currNode= currIter.next();
				if (origNode.equals(currNode)) {
					// All current nodes must be found in the original list now
					while (origIter.hasNext() && currIter.hasNext()) {
						origNode= origIter.next();
						currNode= currIter.next();
						if (!origNode.equals(currNode)) {
							return false;
						}
					}

					return !currIter.hasNext();
				} // else iterate until finding the correct node
			}
		}

		return true;
	}

	/**
	 * Replaces the provided node from the AST with the provided replacement node.
	 *
	 * @param node        The node to remove
	 * @param replacement The replacement node
	 * @param editGroup   The edit group
	 * @see ASTRewrite#replaceButKeepComment(ASTNode, ASTNode,
	 *      org.eclipse.text.edits.TextEditGroup)
	 */
	public void replace(final ASTNode node, final ASTNode replacement, final TextEditGroup editGroup) {
		node.setProperty(UNTOUCH_COMMENT, Boolean.TRUE);
		rewrite.replace(node, replacement, editGroup);
		addRefactoredNodes(node);
	}

	/**
	 * Replaces the provided comment with the provided text.
	 *
	 * @param comment     the comment to replace
	 * @param replacement the replacement text
	 */
	public void replace(final Comment comment, final String replacement) {
		hasRefactorings= true;
		commentRewriter.replace(comment, replacement);
	}

	/**
	 * Replaces the provided source location with the replacement string in the
	 * source.
	 *
	 * @param toReplace   the source location to replace
	 * @param replacement the replacement string
	 */
	public void replace(final SourceLocation toReplace, final String replacement) {
		hasRefactorings= true;
		this.sourceRewriter.replace(toReplace, replacement);
	}

	/**
	 * Removes the provided node from the AST.
	 *
	 * @param node the node to remove
	 * @param editGroup   The edit group
	 * @see ASTRewrite#remove(ASTNode, org.eclipse.text.edits.TextEditGroup)
	 */
	public void remove(final ASTNode node, final TextEditGroup editGroup) {
		if (node instanceof Comment) {
			commentRewriter.remove((Comment) node);
		} else {
			rewrite.remove(node, editGroup);
		}
		addRefactoredNodes(node);
	}

	/**
	 * Removes the provided node from the AST leaving the leading comment.
	 *
	 * @param node the node to remove
	 * @param editGroup   The edit group
	 * @see ASTRewrite#remove(ASTNode, org.eclipse.text.edits.TextEditGroup)
	 */
	public void removeButKeepComment(final ASTNode node, final TextEditGroup editGroup) {
		node.setProperty(UNTOUCH_COMMENT, Boolean.TRUE);
		remove(node, editGroup);
	}

	/**
	 * Removes the provided source location from the source.
	 *
	 * @param toRemove the source location to remove
	 */
	public void remove(final SourceLocation toRemove) {
		if (toRemove.getLength() == 0) {
			throw new IllegalArgumentException("Cannot remove an empty source range: " + toRemove); //$NON-NLS-1$
		}
		hasRefactorings= true;
		sourceRewriter.remove(toRemove);
	}

	/**
	 * Removes the provided nodes from the AST.
	 * @param editGroup   The edit group
	 * @param nodes the nodes to remove
	 *
	 * @see #remove(ASTNode, TextEditGroup)
	 */
	public void remove(final TextEditGroup editGroup, final ASTNode... nodes) {
		remove(Arrays.asList(nodes), editGroup);
	}

	/**
	 * Removes the provided nodes from the AST.
	 *
	 * @param nodes the nodes to remove
	 * @param editGroup   The edit group
	 * @see #remove(ASTNode, TextEditGroup)
	 */
	public void remove(final Collection<? extends ASTNode> nodes, final TextEditGroup editGroup) {
		for (ASTNode node : nodes) {
			remove(node, editGroup);
		}
	}

	/**
	 * Returns whether this instance has any cleanups.
	 *
	 * @return true if this instance has any cleanups, false otherwise.
	 */
	public boolean hasRefactorings() {
		return hasRefactorings;
	}

	/**
	 * Inserts the provided node at the specified index of the list in a node.
	 *
	 * @param listHolder       the node holding the list where to insert
	 * @param locationInParent the insert location description
	 * @param nodeToInsert     the node to insert
	 * @param index            the index where to insert the node in the list
	 * @param editGroup        The edit group
	 * @see ListRewrite#insertAt(ASTNode, int, org.eclipse.text.edits.TextEditGroup)
	 */
	public void insertAt(final ASTNode listHolder, final StructuralPropertyDescriptor locationInParent, final ASTNode nodeToInsert,
			final int index, final TextEditGroup editGroup) {
		getListRewrite(listHolder, locationInParent).insertAt(nodeToInsert, index, editGroup);
		addRefactoredNodes(listHolder);
	}

	/**
	 * Inserts the provided node before the provided element.
	 *
	 * @param nodeToInsert the node to insert
	 * @param element      the node serving as a reference location
	 * @param editGroup    The edit group
	 * @see ListRewrite#insertBefore(ASTNode, ASTNode,
	 *      org.eclipse.text.edits.TextEditGroup)
	 */
	public void insertBefore(final ASTNode nodeToInsert, final ASTNode element, final TextEditGroup editGroup) {
		getListRewrite(element).insertBefore(nodeToInsert, element, editGroup);
		addRefactoredNodes(element.getParent());
	}

	/**
	 * Inserts the provided node after the provided element.
	 *
	 * @param nodeToInsert the node to insert
	 * @param element      the node serving as a reference location
	 * @param editGroup    The edit group
	 * @see ListRewrite#insertAfter(ASTNode, ASTNode,
	 *      org.eclipse.text.edits.TextEditGroup)
	 */
	public void insertAfter(final ASTNode nodeToInsert, final ASTNode element, final TextEditGroup editGroup) {
		getListRewrite(element).insertAfter(nodeToInsert, element, editGroup);
		addRefactoredNodes(element.getParent());
	}

	/**
	 * Inserts the provided node as the first element of the list in a node.
	 *
	 * @param listHolder       the node holding the list where to insert
	 * @param locationInParent the insert location description
	 * @param nodeToInsert     the node to insert
	 * @param editGroup        The edit group
	 * @see ListRewrite#insertFirst(ASTNode, org.eclipse.text.edits.TextEditGroup)
	 */
	public void insertFirst(final ASTNode listHolder, final StructuralPropertyDescriptor locationInParent, final ASTNode nodeToInsert, final TextEditGroup editGroup) {
		getListRewrite(listHolder, locationInParent).insertFirst(nodeToInsert, editGroup);
		addRefactoredNodes(listHolder);
	}

	/**
	 * Inserts the provided node as the last element of the list in a node.
	 *
	 * @param listHolder       the node holding the list where to insert
	 * @param locationInParent the insert location description
	 * @param nodeToInsert     the node to insert
	 * @param editGroup        The edit group
	 * @see ListRewrite#insertLast(ASTNode, org.eclipse.text.edits.TextEditGroup)
	 */
	public void insertLast(final ASTNode listHolder, final StructuralPropertyDescriptor locationInParent, final ASTNode nodeToInsert, final TextEditGroup editGroup) {
		getListRewrite(listHolder, locationInParent).insertLast(nodeToInsert, editGroup);
		addRefactoredNodes(listHolder);
	}

	/**
	 * Moves the old node to the new index in the parents' list.
	 *
	 * @param oldNode   the old node containing the old location
	 * @param newIndex  the new index for the node in the parent's list
	 * @param movedNode the old node which has been moved using
	 *                  {@link org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory#createMoveTarget(ASTNode)}
	 * @param editGroup The edit group
	 */
	public void moveToIndex(final ASTNode oldNode, final int newIndex, final ASTNode movedNode, final TextEditGroup editGroup) {
		insertAt(oldNode.getParent(), oldNode.getLocationInParent(), movedNode, newIndex, editGroup);
	}

	/**
	 * Adds the provided line comment to convert to javadoc.
	 *
	 * @param lineComment the line comment to convert to javadoc
	 * @param nextNode    the AST node immediately following the line comment
	 */
	public void toJavadoc(final LineComment lineComment, final ASTNode nextNode) {
		hasRefactorings= true;
		commentRewriter.toJavadoc(lineComment, nextNode);
	}

	/**
	 * Adds the provided block comment to convert to javadoc.
	 *
	 * @param blockComment the block comment to convert to javadoc
	 */
	public void toJavadoc(final BlockComment blockComment) {
		hasRefactorings= true;
		commentRewriter.toJavadoc(blockComment);
	}

	/**
	 * Sets the node's property to the provided value.
	 *
	 * @param node     the node where to set the property
	 * @param property the property to be set
	 * @param value    the value to set
	 * @param editGroup The edit group
	 * @see ASTRewrite#set(ASTNode, StructuralPropertyDescriptor, Object,
	 *      org.eclipse.text.edits.TextEditGroup)
	 */
	public void set(final ASTNode node, final StructuralPropertyDescriptor property, final Object value, final TextEditGroup editGroup) {
		rewrite.set(node, property, value, editGroup);
		addRefactoredNodes(node);
	}

	/**
	 * Applies the accumulated cleanups to the provided document.
	 *
	 * @param document  the document to refactor
	 * @param hasToSave true if the saving should be handled here
	 * @throws BadLocationException if trying to access a non existing position
	 * @throws CoreException        CoreException
	 */
	public void applyTo(final IDocument document, final boolean hasToSave) throws BadLocationException, CoreException {
		edits= rewrite.rewriteAST(document, null);
		TextEdit importEdits= importRewrite.rewriteImports(monitor);
		commentRewriter.addEdits(document, edits);
		sourceRewriter.addEdits(document, edits);

		if (hasToSave) {
			applyEditsToDocument(edits, importEdits, document);
		}
	}

	private void applyEditsToDocument(final TextEdit edits, final TextEdit importEdits, final IDocument document)
			throws BadLocationException {
		// Call this operation on the SWT Display Thread with syncExec(),
		// because it changes or adds something to the GUI.
		// Otherwise it would throw an Invalid thread access Exception.
		eventLoop.syncExec(new Callable<BadLocationException>() {
			@Override
			public BadLocationException call() throws Exception {
				try {
					edits.apply(document, TextEdit.UPDATE_REGIONS);
					importEdits.apply(document);
					return null;
				} catch (BadLocationException e) {
					return e;
				}
			}
		});
	}

	/**
	 * Gets the ImportRewrite rewrite object.
	 *
	 * @return the ImportRewrite rewrite
	 */
	public ImportRewrite getImportRewrite() {
		return importRewrite;
	}

	/**
	 * Gets the edits.
	 *
	 * @return the edits
	 */
	public TextEdit getEdits() {
		return edits;
	}

	/**
	 * Gets the ASTRewrite rewrite object.
	 *
	 * @return the ASTRewrite rewrite
	 */
	public org.eclipse.jdt.core.dom.rewrite.ASTRewrite getRewrite() {
		return rewrite;
	}
}
