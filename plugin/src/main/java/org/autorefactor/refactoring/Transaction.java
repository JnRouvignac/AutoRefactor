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
        final TextEdit[] textEdits = getTextEdits();
        final TextEdit[] parents = removeParents(textEdits);
        final String result = toString(textEdits);
        addParentsBack(textEdits, parents);
        return result;
    }

    private TextEdit[] removeParents(TextEdit[] textEdits) {
        TextEdit[] parents = new TextEdit[textEdits.length];
        for (int i = 0; i < textEdits.length; i++) {
            parents[i] = textEdits[i].getParent();
            parents[i].removeChild(textEdits[i]);
        }
        return parents;
    }

    private void addParentsBack(TextEdit[] textEdits, TextEdit[] parents) {
        for (int i = 0; i < textEdits.length; i++) {
            parents[i].addChild(textEdits[i]);
        }
    }

    private String toString(TextEdit[] textEdits) {
        final MultiTextEdit multiTextEdit = new MultiTextEdit();
        multiTextEdit.addChildren(textEdits);
        final String result = multiTextEdit.toString()
                .replaceFirst("\\{MultiTextEdit\\}", "{" + getClass().getSimpleName() + "}");
        multiTextEdit.removeChildren();
        return result;
    }
}
