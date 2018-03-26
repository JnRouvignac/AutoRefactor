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
package org.autorefactor.refactoring;

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
import org.autorefactor.util.Pair;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jdt.core.dom.rewrite.TargetSourceRangeComputer;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.TextEdit;

/**
 * Class aggregating all the refactorings performed by a refactoring rule until
 * the rule finished traversing the whole AST tree.
 */
public class Refactorings {

    private static final String UNTOUCH_COMMENT = "untouchComment";

    private final EventLoop eventLoop;
    private boolean hasRefactorings;
    private final ASTRewrite rewrite;
    private final Map<Pair<ASTNode, ChildListPropertyDescriptor>, ListRewrite> listRewriteCache =
            new HashMap<Pair<ASTNode, ChildListPropertyDescriptor>, ListRewrite>();
    private final ASTCommentRewriter commentRewriter;
    private final SourceRewriter sourceRewriter = new SourceRewriter();
    /** Nodes that cannot be visited. */
    private final Set<ASTNode> refactoredNodes = new HashSet<ASTNode>();

    /**
     * Builds an instance of this class.
     *
     * @param astRoot the compilation unit, root of the AST
     * @param eventLoop the event loop
     */
    public Refactorings(CompilationUnit astRoot, EventLoop eventLoop) {
        this.eventLoop = eventLoop;
        this.rewrite = ASTRewrite.create(astRoot.getAST());
        this.rewrite.setTargetSourceRangeComputer(new TargetSourceRangeComputer() {
            @Override
            public SourceRange computeSourceRange(ASTNode node) {
                if (Boolean.TRUE.equals(node.getProperty(UNTOUCH_COMMENT))) {
                    return new SourceRange(node.getStartPosition(), node.getLength());
                }
                return super.computeSourceRange(node);
            }
        });
        this.commentRewriter = new ASTCommentRewriter(astRoot);
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
     * Returns whether the provided node has been the target of a refactoring.
     *
     * @param node the node for which to make the determination
     * @return true if the provided node has been refactored, false otherwise
     */
    public boolean hasBeenRefactored(ASTNode node) {
        return refactoredNodes.contains(node);
    }

    private void addRefactoredNodes(ASTNode node) {
        hasRefactorings = true;
        refactoredNodes.add(node);
        if (node.getParent() != null) {
            addRefactoredNodes(node.getParent());
        }
    }

    /**
     * Creates and returns a placeholder node for a copy of the source code of the provided node.<br>
     * The placeholder node can be used like any new node created via the AST class.<br>
     * When the document is rewritten, a copy of the source code for the provided node is inserted
     * into the output document at the position corresponding to the placeholder (indentation is adjusted).
     *
     * @param <T> the type of the provided node
     * @param node the node for which to create a copy placeholder
     * @return the new placeholder node
     * @see ASTRewrite#createCopyTarget(ASTNode)
     */
    @SuppressWarnings("unchecked")
    public <T extends ASTNode> T createCopyTarget(T node) {
        return (T) rewrite.createCopyTarget(node);
    }

    /**
     * Creates and returns a placeholder node for a copy of the source code of the provided range of nodes.<br>
     * The placeholder node can be used like any new node created via the AST class.<br>
     * When the document is rewritten, a copy of the source code for the provided range of nodes is inserted
     * into the output document at the position corresponding to the placeholder (indentation is adjusted).
     *
     * @param <T> the type of the provided nodes
     * @param first the first node of the range
     * @param last the first node of the range
     * @return the new placeholder node
     * @see ListRewrite#createCopyTarget(ASTNode, ASTNode)
     */
    @SuppressWarnings("unchecked")
    public <T extends ASTNode> T createCopyTarget(T first, T last) {
        return (T) getListRewrite(first).createCopyTarget(first, last);
    }

    private ListRewrite getListRewrite(ASTNode child) {
        return getListRewrite(child.getParent(), (ChildListPropertyDescriptor) child.getLocationInParent());
    }

    private ListRewrite getListRewrite(ASTNode listHolder, StructuralPropertyDescriptor locationInParent) {
        return getListRewrite(listHolder, (ChildListPropertyDescriptor) locationInParent);
    }

    private ListRewrite getListRewrite(ASTNode node, ChildListPropertyDescriptor listProperty) {
        final Pair<ASTNode, ChildListPropertyDescriptor> key = Pair.of(node, listProperty);
        ListRewrite listRewrite = listRewriteCache.get(key);
        if (listRewrite == null) {
            listRewrite = rewrite.getListRewrite(node, listProperty);
            listRewriteCache.put(key, listRewrite);
        }
        return listRewrite;
    }

    /**
     * Creates and returns a placeholder node where to move the source code of the provided node.<br>
     * The placeholder node can be used like any new node created via the AST class.<br>
     * When the document is rewritten, the source code for the provided node is inserted
     * into the output document at the position corresponding to the placeholder (indentation is adjusted)
     * and it is removed from the old location.
     *
     * @param <T> the type of the provided node
     * @param node the node for which to create a move placeholder
     * @return the new placeholder node
     * @see ASTRewrite#createMoveTarget(ASTNode)
     */
    @SuppressWarnings("unchecked")
    public <T extends ASTNode> T createMoveTarget(T node) {
        return (T) rewrite.createMoveTarget(node);
    }

    /**
     * Creates and returns a placeholder node for a move of a range of nodes of the current list.<br>
     * The placeholder node can either be inserted as new or used to replace an existing node.<br>
     * When the document is rewritten, a copy of the source code for the given node range is inserted
     * into the output document at the position corresponding to the placeholder (indentation is
     * adjusted).
     *
     * @param <T>
     *          the type of the provided node
     * @param first
     *          the node that starts the range
     * @param last
     *          the node that ends the range
     * @return the new placeholder node
     * @throws IllegalArgumentException
     *           An exception is thrown if the first or last node are null, if a node is not a child
     *           of the current list or if the first node is not before the last node. An
     *           IllegalArgumentException is also thrown if the moved range is overlapping with an
     *           other moved or copied range.
     * @see ListRewrite#createMoveTarget(ASTNode, ASTNode)
     */
    @SuppressWarnings("unchecked")
    public <T extends ASTNode> T createMoveTarget(T first, T last) {
        return (T) getListRewrite(first).createMoveTarget(first, last);
    }

    /**
     * Returns whether the provided nodes are a valid existing range.
     *
     * @param nodes the node range to validate
     * @return true if the provided nodes are a valid existing range, false otherwise
     */
    public boolean isValidRange(List<? extends ASTNode> nodes) {
        if (nodes.isEmpty()) {
            return true;
        }
        @SuppressWarnings("unchecked")
        final List<ASTNode> originalList = getListRewrite(nodes.get(0)).getOriginalList();
        final Iterator<ASTNode> origIter = originalList.iterator();
        while (origIter.hasNext()) {
            ASTNode origNode = origIter.next();
            final Iterator<? extends ASTNode> currIter = nodes.iterator();
            while (currIter.hasNext()) {
                ASTNode currNode = currIter.next();
                if (origNode.equals(currNode)) {
                    // all current nodes must be found in the original list now
                    while (origIter.hasNext() && currIter.hasNext()) {
                        origNode = origIter.next();
                        currNode = currIter.next();
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
     * @param node the node to remove
     * @param replacement the replacement node
     * @see ASTRewrite#replace(ASTNode, ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void replace(ASTNode node, ASTNode replacement) {
        node.setProperty(UNTOUCH_COMMENT, Boolean.TRUE);
        rewrite.replace(node, replacement, null);
        addRefactoredNodes(node);
    }

    /**
     * Replaces the provided comment with the provided text.
     *
     * @param comment the comment to replace
     * @param replacement the replacement text
     */
    public void replace(Comment comment, String replacement) {
        hasRefactorings = true;
        commentRewriter.replace(comment, replacement);
    }

    /**
     * Replaces the provided source location with the replacement string in the source.
     *
     * @param toReplace the source location to replace
     * @param replacement the replacement string
     */
    public void replace(SourceLocation toReplace, String replacement) {
        hasRefactorings = true;
        this.sourceRewriter.replace(toReplace, replacement);
    }

    /**
     * Removes the provided node from the AST.
     *
     * @param node the node to remove
     * @see ASTRewrite#remove(ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void remove(ASTNode node) {
        if (node instanceof Comment) {
            commentRewriter.remove((Comment) node);
        } else {
            rewrite.remove(node, null);
        }
        addRefactoredNodes(node);
    }

    /**
     * Removes the provided node from the AST leaving the leading comment.
     *
     * @param node the node to remove
     * @see ASTRewrite#remove(ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void removeButKeepComment(ASTNode node) {
        node.setProperty(UNTOUCH_COMMENT, Boolean.TRUE);
        remove(node);
    }

    /**
     * Removes the provided source location from the source.
     *
     * @param toRemove the source location to remove
     */
    public void remove(SourceLocation toRemove) {
        if (toRemove.getLength() == 0) {
            throw new IllegalArgumentException("Cannot remove an empty source range: " + toRemove);
        }
        hasRefactorings = true;
        sourceRewriter.remove(toRemove);
    }

    /**
     * Removes the provided nodes from the AST.
     *
     * @param nodes the nodes to remove
     * @see #remove(ASTNode)
     */
    public void remove(ASTNode... nodes) {
        remove(Arrays.asList(nodes));
    }

    /**
     * Removes the provided nodes from the AST.
     *
     * @param nodes the nodes to remove
     * @see #remove(ASTNode)
     */
    public void remove(Collection<? extends ASTNode> nodes) {
        for (ASTNode node : nodes) {
            remove(node);
        }
    }

    /**
     * Returns whether this instance has any refactorings.
     *
     * @return true if this instance has any refactorings, false otherwise.
     */
    public boolean hasRefactorings() {
        return hasRefactorings;
    }

    /**
     * Inserts the provided node at the specified index of the list in a node.
     *
     * @param listHolder the node holding the list where to insert
     * @param locationInParent the insert location description
     * @param nodeToInsert the node to insert
     * @param index the index where to insert the node in the list
     * @see ListRewrite#insertAt(ASTNode, int, org.eclipse.text.edits.TextEditGroup)
     */
    public void insertAt(
            ASTNode listHolder, StructuralPropertyDescriptor locationInParent, ASTNode nodeToInsert, int index) {
        getListRewrite(listHolder, locationInParent).insertAt(nodeToInsert, index, null);
        addRefactoredNodes(listHolder);
    }

    /**
     * Inserts the provided node before the provided element.
     *
     * @param nodeToInsert the node to insert
     * @param element the node serving as a reference location
     * @see ListRewrite#insertBefore(ASTNode, ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void insertBefore(ASTNode nodeToInsert, ASTNode element) {
        getListRewrite(element).insertBefore(nodeToInsert, element, null);
        addRefactoredNodes(element.getParent());
    }

    /**
     * Inserts the provided node after the provided element.
     *
     * @param nodeToInsert the node to insert
     * @param element the node serving as a reference location
     * @see ListRewrite#insertAfter(ASTNode, ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void insertAfter(ASTNode nodeToInsert, ASTNode element) {
        getListRewrite(element).insertAfter(nodeToInsert, element, null);
        addRefactoredNodes(element.getParent());
    }

    /**
     * Inserts the provided node as the first element of the list in a node.
     *
     * @param listHolder the node holding the list where to insert
     * @param locationInParent the insert location description
     * @param nodeToInsert the node to insert
     * @see ListRewrite#insertFirst(ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void insertFirst(ASTNode listHolder, StructuralPropertyDescriptor locationInParent, ASTNode nodeToInsert) {
        getListRewrite(listHolder, locationInParent).insertFirst(nodeToInsert, null);
        addRefactoredNodes(listHolder);
    }

    /**
     * Inserts the provided node as the last element of the list in a node.
     *
     * @param listHolder the node holding the list where to insert
     * @param locationInParent the insert location description
     * @param nodeToInsert the node to insert
     * @see ListRewrite#insertLast(ASTNode, org.eclipse.text.edits.TextEditGroup)
     */
    public void insertLast(ASTNode listHolder, StructuralPropertyDescriptor locationInParent, ASTNode nodeToInsert) {
        getListRewrite(listHolder, locationInParent).insertLast(nodeToInsert, null);
        addRefactoredNodes(listHolder);
    }

    /**
     * Moves the old node to the new index in the parents' list.
     *
     * @param oldNode the old node containing the old location
     * @param newIndex the new index for the node in the parent's list
     * @param movedNode the old node which has been moved using
     *            {@link org.autorefactor.refactoring.ASTBuilder#move(ASTNode)}
     */
    public void moveToIndex(ASTNode oldNode, int newIndex, ASTNode movedNode) {
        insertAt(oldNode.getParent(), oldNode.getLocationInParent(), movedNode, newIndex);
    }

    /**
     * Adds the provided line comment to convert to javadoc.
     *
     * @param lineComment the line comment to convert to javadoc
     * @param nextNode the AST node immediately following the line comment
     */
    public void toJavadoc(LineComment lineComment, ASTNode nextNode) {
        hasRefactorings = true;
        commentRewriter.toJavadoc(lineComment, nextNode);
    }

    /**
     * Adds the provided block comment to convert to javadoc.
     *
     * @param blockComment the block comment to convert to javadoc
     */
    public void toJavadoc(BlockComment blockComment) {
        hasRefactorings = true;
        commentRewriter.toJavadoc(blockComment);
    }

    /**
     * Sets the node's property to the provided value.
     *
     * @param node the node where to set the property
     * @param property the property to be set
     * @param value the value to set
     * @see ASTRewrite#set(ASTNode, StructuralPropertyDescriptor, Object, org.eclipse.text.edits.TextEditGroup)
     */
    public void set(ASTNode node, StructuralPropertyDescriptor property, Object value) {
        rewrite.set(node, property, value, null);
        addRefactoredNodes(node);
    }

    /**
     * Applies the accumulated refactorings to the provided document.
     *
     * @param document the document to refactor
     * @throws BadLocationException if trying to access a non existing position
     */
    public void applyTo(final IDocument document) throws BadLocationException {
        final TextEdit edits = rewrite.rewriteAST(document, null);
        commentRewriter.addEdits(document, edits);
        sourceRewriter.addEdits(document, edits);
        applyEditsToDocument(edits, document);
    }

    private void applyEditsToDocument(final TextEdit edits, final IDocument document) throws BadLocationException {
        // Call this operation on the SWT Display Thread with syncExec(),
        // because it changes or adds something to the GUI.
        // Otherwise it would throw an Invalid thread access Exception.
        eventLoop.syncExec(new Callable<BadLocationException>() {
            /**
             * Call.
             *
             * @return the bad location exception.
             */
            public BadLocationException call() throws Exception {
                try {
                    edits.apply(document);
                    return null;
                } catch (BadLocationException e) {
                    return e;
                }
            }
        });
    }

    /**
     * Gets the ASTRewrite rewrite object.
     *
     * @return the ASTRewrite rewrite
     */
    public ASTRewrite getRewrite() {
        return rewrite;
    }
}
