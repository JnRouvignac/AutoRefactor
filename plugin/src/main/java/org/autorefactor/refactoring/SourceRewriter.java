/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.autorefactor.util.Pair;
import org.eclipse.jface.text.IDocument;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.TextEditGroup;

/** Directly rewrites source code. */
public class SourceRewriter {

    private final Map<SourceLocation, TextEditGroup> removals = new LinkedHashMap<SourceLocation, TextEditGroup>();
    private final Map<SourceLocation, Pair<String, TextEditGroup>> replacements =
            new LinkedHashMap<SourceLocation, Pair<String, TextEditGroup>>();

    /**
     * Removes the provided source location from the source.
     *
     * @param toRemove the source location to remove
     * @param textEditGroup
     */
    public void remove(SourceLocation toRemove, TextEditGroup textEditGroup) {
        this.removals.put(toRemove, textEditGroup);
    }

    /**
     * Replaces the provided source location with the replacement string in the source.
     *
     * @param toReplace the source location to replace
     * @param replacement the replacement string
     * @param textEditGroup
     */
    public void replace(SourceLocation toReplace, String replacement, TextEditGroup textEditGroup) {
        this.replacements.put(toReplace, Pair.of(replacement, textEditGroup));
    }

    /**
     * Adds the edits contained in the current instance to the provided edits for the provided document.
     *
     * @param document the document to edit
     * @param edits where to add edits
     */
    public void addEdits(IDocument document, TextEdit edits) {
        for (Entry<SourceLocation, TextEditGroup> entry : this.removals.entrySet()) {
            SourceLocation loc = entry.getKey();
            DeleteEdit textEdit = new DeleteEdit(loc.getStartPosition(), loc.getLength());
            addTextEdit(edits, entry.getValue(), textEdit);
        }
        for (Entry<SourceLocation, Pair<String, TextEditGroup>> entry : this.replacements.entrySet()) {
            Pair<String, TextEditGroup> value = entry.getValue();
            SourceLocation loc = entry.getKey();
            String replacement = value.getFirst();
            ReplaceEdit textEdit = new ReplaceEdit(loc.getStartPosition(), loc.getLength(), replacement);
            addTextEdit(edits, value.getSecond(), textEdit);
        }
    }

    private void addTextEdit(TextEdit edits, TextEditGroup textEditGroup, TextEdit textEdit) {
        edits.addChild(textEdit);
        textEditGroup.addTextEdit(textEdit);
    }
}
