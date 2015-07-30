package org.autorefactor.refactoring;

import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEditGroup;

/**
 * Represents a transaction for a refactoring that groups together text edits.
 *
 * A transaction must be committed by calling {@link #commit()} once it is complete.
 */
public class Transaction extends TextEditGroup {

    private static int count = 1;
    private boolean committed;

    /** Default constructor. */
    public Transaction() {
        this(String.valueOf(count));
        count++;
    }

    /**
     * Constructor that allows to name a refactoring transaction.
     *
     * @param name the transaction name
     */
    public Transaction(String name) {
        super(name);
    }

    /**
     * Commits the current transaction.
     * This validates the current transaction is now complete
     * and will ensure its changes will be applied to the document.
     */
    public void commit() {
        this.committed = true;
    }

    /**
     * Returns whether the current transaction has been committed.
     *
     * @return true if the current transaction has been committed, false otherwise
     */
    public boolean isCommitted() {
        return committed;
    }

    @Override
    public String toString() {
        final MultiTextEdit multiTextEdit = new MultiTextEdit();
        multiTextEdit.addChildren(getTextEdits());
        final String result = multiTextEdit.toString();
        multiTextEdit.removeChildren();
        return result.replaceFirst("\\{MultiTextEdit\\}", "{" + getClass().getSimpleName() + "}");
    }
}
