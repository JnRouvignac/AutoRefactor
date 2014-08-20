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
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
    private final Map<ChildListPropertyDescriptor, List<Insert>> inserts =
            new LinkedHashMap<ChildListPropertyDescriptor, List<Insert>>();
    /**
     * The refactorings removing code from the AST tree.
     */
    private final List<ASTNode> removals = new LinkedList<ASTNode>();
    private final Set<Comment> commentRemovals = new LinkedHashSet<Comment>();
    private final Set<Pair<Comment, String>> commentReplacements = new LinkedHashSet<Pair<Comment, String>>();
    private final Map<ASTNode, List<LineComment>> lineCommentsToJavadoc = new HashMap<ASTNode, List<LineComment>>();
    private final List<BlockComment> blockCommentToJavadoc = new LinkedList<BlockComment>();

    /**
     * Describes where to insert new code: before or after an existing
     * {@link ASTNode}.
     */
    public static enum InsertType {
        /** Inserts at the provided index in a list. */
        AT_INDEX,
        /** Inserts before the provided list element. */
        BEFORE,
        /** Inserts after the provided list element. */
        AFTER
    }

    /**
     * Describes how to insert new code: which code, before or after an existing
     * {@link ASTNode} and which ASTNode.
     */
    public static class Insert {

        /**
         * Class constructor.
         *
         * @param nodeToInsert the node to insert
         * @param element the node serving as a reference location
         * @param insertType the type of insert to perform
         */
        public Insert(ASTNode nodeToInsert, ASTNode element, InsertType insertType) {
            if (!InsertType.BEFORE.equals(insertType) && !InsertType.AFTER.equals(insertType)) {
                throw new IllegalArgumentException(
                    "InsertType can only be one of BEFORE or AFTER for this constructor. Given: " + insertType);
            }
            this.nodeToInsert = nodeToInsert;
            this.insertType = insertType;
            this.element = element;
            this.listHolder = element.getParent();
            this.index = -1;
        }

        /**
         * Class constructor.
         *
         * @param nodeToInsert the node to insert
         * @param listHolder the node holding the list where to insert
         * @param index the index where to insert the node in the list
         */
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

        /**
         * Returns the node to insert.
         *
         * @return  the node to insert
         */
        public ASTNode getNodeToInsert() {
            return nodeToInsert;
        }

        /**
         * Returns the node to insert.
         *
         * @return  the node to insert
         */
        public ASTNode getElement() {
            return element;
        }

        /**
         * Returns the type of insert to perform.
         *
         * @return the type of insert to perform
         */
        public InsertType getInsertType() {
            return insertType;
        }

        /**
         * Returns the node holding the list where to insert.
         *
         * @return the node holding the list where to insert
         */
        public ASTNode getListHolder() {
            return listHolder;
        }

        /**
         * Returns the index where to insert the node in the list.
         *
         * @return the index where to insert the node in the list
         */
        public int getIndex() {
            return index;
        }
    }

    /**
     * Returns the replacements to perform.
     *
     * @return the replacements to perform
     */
    public List<Pair<ASTNode, ASTNode>> getReplacements() {
        return this.replacements;
    }

    /**
     * Returns the nodes to remove.
     *
     * @return the nodes to remove
     */
    public List<ASTNode> getRemovals() {
        return removals;
    }

    /**
     * Returns the inserts to perform.
     *
     * @return the inserts to perform
     */
    public Map<ChildListPropertyDescriptor, List<Insert>> getInserts() {
        return this.inserts;
    }

    /**
     * Returns the comments to remove.
     *
     * @return the comments to remove
     */
    public Set<Comment> getCommentRemovals() {
        return this.commentRemovals;
    }

    /**
     * Returns the comments to replace with the supplied text.
     *
     * @return the comments to replace with the supplied text
     */
    public Set<Pair<Comment, String>> getCommentReplacements() {
        return this.commentReplacements;
    }

    /**
     * Returns the block comments to convert to javadocs.
     *
     * @return the block comments to convert to javadocs
     */
    public List<BlockComment> getBlockCommentToJavadoc() {
        return blockCommentToJavadoc;
    }

    /**
     * Returns the line comment lists to convert to single javadocs.
     *
     * @return the line comment lists to convert to single javadocs
     */
    public Collection<List<LineComment>> getLineCommentsToJavadoc() {
        return lineCommentsToJavadoc.values();
    }

    /**
     * Replaces the provided node from the AST with the provided replacement node.
     *
     * @param node the node to remove
     * @param replacement the replacement node
     * @see org.eclipse.jdt.core.dom.rewrite.ASTRewrite#replace(ASTNode, ASTNode,
     *      org.eclipse.text.edits.TextEditGroup)
     */
    public void replace(ASTNode node, ASTNode replacement) {
        this.replacements.add(Pair.of(node, replacement));
    }

    /**
     * Replaces the provided comment with the provided text.
     *
     * @param comment the comment to replace
     * @param replacement the replacement text
     */
    public void replace(Comment comment, String replacement) {
        this.commentReplacements.add(Pair.of(comment, replacement));
    }

    /**
     * Removes the provided node from the AST.
     *
     * @param node the node to remove
     * @see org.eclipse.jdt.core.dom.rewrite.ASTRewrite#remove(ASTNode,
     *      org.eclipse.text.edits.TextEditGroup)
     */
    public void remove(ASTNode node) {
        if (node instanceof Comment) {
            this.commentRemovals.add((Comment) node);
        } else {
            this.removals.add(node);
        }
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
    public void remove(Collection<ASTNode> nodes) {
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
        return !this.replacements.isEmpty() || !this.removals.isEmpty()
                || !this.inserts.isEmpty() || !this.commentRemovals.isEmpty()
                || !this.commentReplacements.isEmpty()
                || !this.lineCommentsToJavadoc.isEmpty()
                || !this.blockCommentToJavadoc.isEmpty();
    }

    /**
     * Inserts the provided node at a specified location in a node.
     *
     * @param nodeToInsert the node to insert
     * @param index the index where to insert the node in the list
     * @param locationInParent the insert location description
     * @param listHolder the node holding the list where to insert
     * @see org.eclipse.jdt.core.dom.rewrite.ListRewrite#insertAt(ASTNode, int,
     *      org.eclipse.text.edits.TextEditGroup)
     */
    public void insertAt(ASTNode nodeToInsert, int index, StructuralPropertyDescriptor locationInParent,
            ASTNode listHolder) {
        insert(locationInParent, new Insert(nodeToInsert, listHolder, index));
    }

    /**
     * Inserts the provided node before the provided element.
     *
     * @param nodeToInsert the node to insert
     * @param element the node serving as a reference location
     * @see org.eclipse.jdt.core.dom.rewrite.ListRewrite#insertBefore(ASTNode, ASTNode,
     *      org.eclipse.text.edits.TextEditGroup)
     */
    public void insertBefore(ASTNode nodeToInsert, ASTNode element) {
        insert(element.getLocationInParent(), new Insert(nodeToInsert, element, InsertType.BEFORE));
    }

    /**
     * Inserts the provided node after the provided element.
     *
     * @param nodeToInsert the node to insert
     * @param element the node serving as a reference location
     * @see org.eclipse.jdt.core.dom.rewrite.ListRewrite#insertAfter(ASTNode, ASTNode,
     *      org.eclipse.text.edits.TextEditGroup)
     */
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

    /**
     * Adds the provided line comment to convert to javadoc.
     *
     * @param lineComment the line comment to convert to javadoc
     * @param nextNode the AST node immediately following the line comment
     */
    public void toJavadoc(LineComment lineComment, ASTNode nextNode) {
        List<LineComment> comments = this.lineCommentsToJavadoc.get(nextNode);
        if (comments == null) {
            comments = new LinkedList<LineComment>();
            this.lineCommentsToJavadoc.put(nextNode, comments);
        }
        comments.add(lineComment);
    }

    /**
     * Adds the provided block comment to convert to javadoc.
     *
     * @param blockComment the block comment to convert to javadoc
     */
    public void toJavadoc(BlockComment blockComment) {
        this.blockCommentToJavadoc.add(blockComment);
    }

    /** {@inheritDoc} */
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
