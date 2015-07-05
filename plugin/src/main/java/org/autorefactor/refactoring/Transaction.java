package org.autorefactor.refactoring;

import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditGroup;

public class Transaction extends TextEditGroup {

    private static int count = 1;
    private boolean committed;

    public Transaction() {
        this(String.valueOf(count));
        count++;
    }

    public Transaction(String name) {
        super(name);
    }

    public void commit() {
        this.committed = true;
    }

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
