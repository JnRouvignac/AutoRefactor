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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.preferences.Preferences;
import org.autorefactor.util.Pair;
import org.autorefactor.util.UnhandledException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BlockComment;
import org.eclipse.jdt.core.dom.ChildListPropertyDescriptor;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.LineComment;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.swt.widgets.Display;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Class aggregating all the refactorings performed by a refactoring rule until
 * the rule finished traversing the whole AST tree.
 */
public class Refactorings {

    private boolean hasRefactorings;
    private final ASTRewrite rewrite;
    private final SourceLocation selection;
    private final Map<Pair<ASTNode, ChildListPropertyDescriptor>, ListRewrite> listRewriteCache =
            new HashMap<Pair<ASTNode, ChildListPropertyDescriptor>, ListRewrite>();
    private final ASTCommentRewriter commentRewriter = new ASTCommentRewriter();
    private final SourceRewriter sourceRewriter = new SourceRewriter();
    /** Nodes that cannot be visited. */
    private final Set<ASTNode> forbiddenNodes = new HashSet<ASTNode>();
    private final Set<Transaction> transactions = new LinkedHashSet<Transaction>();

    /**
     * Builds an instance of this class.
     *
     * @param ast the AST
     * @param selection the selected text in the source editor where refactorings will be applied
     */
    public Refactorings(AST ast, SourceLocation selection) {
        this.rewrite = ASTRewrite.create(ast);
        this.selection = selection;
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
     * Returns whether the provided node can be visited.
     *
     * @param node the node that might be visited
     * @return true if the provided node can be visited, false otherwise
     */
    public boolean canVisit(ASTNode node) {
        return !forbiddenNodes.contains(node);
    }

    private void doNotVisit(ASTNode node) {
        forbiddenNodes.add(node);
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
        this.forbiddenNodes.equals(node);
        return (T) rewrite.createMoveTarget(node);
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
     * @param transaction the refactoring transaction
     * @see ASTRewrite#replace(ASTNode, ASTNode, TextEditGroup)
     */
    public void replace(ASTNode node, ASTNode replacement, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        rewrite.replace(node, replacement, txn);
        if (txn != transaction) {
            txn.commit();
        }
        doNotVisit(node);
    }

    /**
     * Replaces the provided comment with the provided text.
     *
     * @param comment the comment to replace
     * @param replacement the replacement text
     * @param transaction the refactoring transaction
     */
    public void replace(Comment comment, String replacement, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        commentRewriter.replace(comment, replacement, txn);
        if (txn != transaction) {
            txn.commit();
        }
        doNotVisit(comment);
    }

    /**
     * Replaces the provided source location with the replacement string in the source.
     *
     * @param toReplace the source location to replace
     * @param replacement the replacement string
     * @param transaction the refactoring transaction
     */
    public void replace(SourceLocation toReplace, String replacement, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        sourceRewriter.replace(toReplace, replacement, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Removes the provided node from the AST.
     *
     * @param node the node to remove
     * @param transaction the refactoring transaction
     * @see ASTRewrite#remove(ASTNode, TextEditGroup)
     */
    public void remove(ASTNode node, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        if (node instanceof Comment) {
            commentRewriter.remove((Comment) node, txn);
        } else {
            rewrite.remove(node, txn);
        }
        if (txn != transaction) {
            txn.commit();
        }
        doNotVisit(node);
    }

    /**
     * Removes the provided source location from the source.
     *
     * @param toRemove the source location to remove
     * @param transaction the refactoring transaction
     */
    public void remove(SourceLocation toRemove, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        sourceRewriter.remove(toRemove, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Removes the provided nodes from the AST.
     *
     * @param nodes the nodes to remove
     * @param transaction the refactoring transaction
     * @see #remove(ASTNode, TextEditGroup)
     */
    public void remove(Collection<? extends ASTNode> nodes, Transaction transaction) {
        if (nodes.isEmpty()) {
            throw new IllegalArgumentException("The provided nodes cannot be empty");
        }
        final Transaction txn = getTxn(transaction);
        for (ASTNode node : nodes) {
            remove(node, txn);
        }
        if (txn != transaction) {
            txn.commit();
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
     * Inserts the provided node at a specified location in a node.
     *
     * @param nodeToInsert the node to insert
     * @param index the index where to insert the node in the list
     * @param locationInParent the insert location description
     * @param listHolder the node holding the list where to insert
     * @param transaction the refactoring transaction
     * @see ListRewrite#insertAt(ASTNode, int, TextEditGroup)
     */
    public void insertAt(ASTNode nodeToInsert, int index, StructuralPropertyDescriptor locationInParent,
            ASTNode listHolder, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        final ListRewrite listRewrite = getListRewrite(listHolder, (ChildListPropertyDescriptor) locationInParent);
        listRewrite.insertAt(nodeToInsert, index, txn);
        hackToAddRewriteEventsToTransaction(listRewrite, transaction, listHolder);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * JDT does not track with TextEditGroups the original nodes from a list. So add them ourselves.
     * FIXME Report bug the to JDT. Fix the bug?
     */
    private void hackToAddRewriteEventsToTransaction(
            ListRewrite listRewrite, Transaction transaction, ASTNode listHolder) {
        try {
            Object listRewriteEvent = invoke(listRewrite, "getEvent");
            Object[] rewriteEventArray = (Object[]) invoke(listRewriteEvent, "getChildren");
            Object rewriteStore = invoke(listRewrite, "getRewriteStore");

            for (Object rewriteEvent : rewriteEventArray) {
                invoke(rewriteStore, "setEventEditGroup", rewriteEvent, transaction);
            }
        } catch (Exception e) {
            throw new UnhandledException(listHolder, e);
        }
    }

    private void invoke(Object rewriteStore, String methodName, Object rewriteEvent, Transaction transaction)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, ClassNotFoundException {
        Class<?> forName = Class.forName("org.eclipse.jdt.internal.core.dom.rewrite.RewriteEvent");
        Method m = rewriteStore.getClass().getDeclaredMethod(methodName, forName, TextEditGroup.class);
        m.setAccessible(true);
        m.invoke(rewriteStore, rewriteEvent, transaction);
    }

    private Object invoke(Object listRewrite, String methodName)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Method m = listRewrite.getClass().getDeclaredMethod(methodName);
        m.setAccessible(true);
        return m.invoke(listRewrite);
    }

    /**
     * Inserts the provided node before the provided element.
     *
     * @param nodeToInsert the node to insert
     * @param element the node serving as a reference location
     * @param transaction the refactoring transaction
     * @see ListRewrite#insertBefore(ASTNode, ASTNode, TextEditGroup)
     */
    public void insertBefore(ASTNode nodeToInsert, ASTNode element, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        getListRewrite(element).insertBefore(nodeToInsert, element, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Inserts the provided node after the provided element.
     *
     * @param nodeToInsert the node to insert
     * @param element the node serving as a reference location
     * @param transaction the refactoring transaction
     * @see ListRewrite#insertAfter(ASTNode, ASTNode, TextEditGroup)
     */
    public void insertAfter(ASTNode nodeToInsert, ASTNode element, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        getListRewrite(element).insertAfter(nodeToInsert, element, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Adds the provided line comment to convert to javadoc.
     *
     * @param lineComment the line comment to convert to javadoc
     * @param nextNode the AST node immediately following the line comment
     * @param transaction the refactoring transaction
     */
    public void toJavadoc(LineComment lineComment, ASTNode nextNode, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        commentRewriter.toJavadoc(lineComment, nextNode, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Adds the provided block comment to convert to javadoc.
     *
     * @param blockComment the block comment to convert to javadoc
     * @param transaction the refactoring transaction
     */
    public void toJavadoc(BlockComment blockComment, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        commentRewriter.toJavadoc(blockComment, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Sets the node's property to the provided value.
     *
     * @param node the node where to set the property
     * @param property the property to be set
     * @param value the value to set
     * @param transaction the refactoring transaction
     * @see ASTRewrite#set(ASTNode, StructuralPropertyDescriptor, Object, TextEditGroup)
     */
    public void set(ASTNode node, StructuralPropertyDescriptor property, Object value, Transaction transaction) {
        final Transaction txn = getTxn(transaction);
        hasRefactorings = true;
        rewrite.set(node, property, value, txn);
        if (txn != transaction) {
            txn.commit();
        }
    }

    /**
     * Applies the accumulated refactorings to the provided document.
     *
     * @param document the document to refactor
     * @return true if any refactoring could be applied, false otherwise
     * @throws BadLocationException if trying to access a non existing position
     * @throws InterruptedException if interrupted
     */
    public boolean applyTo(final IDocument document) throws BadLocationException, InterruptedException {
        final TextEdit edits = rewrite.rewriteAST(document, null);
        commentRewriter.addEdits(document, edits);
        sourceRewriter.addEdits(document, edits);

        final Preferences prefs = AutoRefactorPlugin.getPreferenceHelper();
        if (prefs.applyRefactoringsWithTextEditGroup()) {
            final TextEdit[] textRewrites = edits.removeChildren();
            filterOutIncompleteRefactorings(transactions);
            Collection<Transaction> txnsToApply = getRefactoringsApplicableToSelection(transactions, selection);
            if (!txnsToApply.isEmpty()) {
                final TextEdit toApply = toMultiTextEdit(txnsToApply, textRewrites);
                applyEditsToDocument(toApply, document);
                return true;
            }
            return false;
        } else {
            applyEditsToDocument(edits, document);
            return true;
        }
    }

    private TextEdit toMultiTextEdit(Collection<Transaction> txnsToApply, TextEdit[] textRewrites) {
        MultiTextEdit result = new MultiTextEdit();
        for (Transaction txn : txnsToApply) {
            result.addChildren(removeParents(txn.getTextEdits(), textRewrites));
        }
        return result;
    }

    private TextEdit[] removeParents(TextEdit[] txnTextEdits, TextEdit[] textRewrites) {
        final List<TextEdit> results = new ArrayList<TextEdit>();
        List<TextEdit> txnTextEditsCol = Arrays.asList(txnTextEdits);
        for (TextEdit rewrite : textRewrites) {
            if (txnTextEditsCol.contains(rewrite)) {
                results.add(rewrite);
            }
        }
        return results.toArray(new TextEdit[results.size()]);
    }

    private void filterOutIncompleteRefactorings(Collection<Transaction> txns) {
        for (Iterator<Transaction> it = txns.iterator(); it.hasNext();) {
            Transaction txn = it.next();
            if (!txn.isCommitted()) {
                // do not apply incomplete refactorings
                it.remove();
            }
        }
    }

    private Collection<Transaction> getRefactoringsApplicableToSelection(
            Collection<Transaction> txns, SourceLocation selection) {
        if (selection == null) {
            return txns;
        }
        final List<Transaction> results = new ArrayList<Transaction>();
        for (Transaction txn : txns) {
            IRegion region = txn.getRegion();
            if (selection.contains(new SourceLocation(region))) {
                results.add(txn);
            }
        }
        return results;
    }

    private void applyEditsToDocument(final TextEdit edits, final IDocument document)
            throws BadLocationException, InterruptedException {
        // Call this operation on the SWT Display Thread with syncExec(),
        // because it changes or adds something to the GUI.
        // Otherwise it would throw an Invalid thread access Exception.
        final FutureTask<Void> future = new FutureTask<Void>(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                edits.apply(document);
                return null;
            }
        });
        Display.getDefault().syncExec(future);

        try {
            future.get();
        } catch (ExecutionException e) {
            Throwable cause = e.getCause();
            if (cause instanceof BadLocationException) {
                throw (BadLocationException) cause;
            } else  if (cause instanceof MalformedTreeException) {
                throw (MalformedTreeException) cause;
            }
            throw new UnhandledException(null, e);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw e;
        }
    }

    private Transaction getTxn(Transaction txn) {
        final Transaction result = txn != null ? txn : new Transaction();
        transactions.add(result);
        return result;
    }

    /**
     * Returns a new refactoring transaction object.
     *
     * @param refactoringRule the rule for which to create the refactoring transaction object.
     * @return a new refactoring transaction object
     */
    public Transaction newTransaction(RefactoringRule refactoringRule) {
        return new Transaction(refactoringRule.getClass().getSimpleName().toString());
    }
}
