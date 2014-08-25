/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.ui;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.io.PrintWriter;
import java.io.StringWriter;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Utility class for shared UI features.
 */
final class UIHelper {

    private UIHelper() {
        // private ctor for utility class
    }

    /**
     * Shows an error dialog for an exception.
     *
     * @param shell the current shell
     * @param e the exception for which to show an error dialog
     */
    static void showErrorDialog(Shell shell, Exception e) {
        final StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw));
        final String fullErrorMsg = sw.toString();

        final String simplifiedErrorMsg = fullErrorMsg.replaceAll(
                "(\\tat (?:javax?\\.|sun\\.|org\\.eclipse\\.).*(?:\\r\\n|\\n|\\r))+",
                "\t[...]\n");
        openErrorDialog(shell, simplifiedErrorMsg, fullErrorMsg);
    }

    private static void openErrorDialog(Shell shell, String msg, String msgForClipboard) {
        final MessageDialog dialog = new MessageDialog(shell,
            "Error while applying refactorings", null,
            "Please copy stacktrace down below and report it to the AutoRefactor project:\n\n\n" + msg,
            MessageDialog.ERROR, new String[] { "Copy", "OK" }, 1);

        final boolean copyToClipboard = dialog.open() == 0;
        if (copyToClipboard) {
            final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
            clipboard.setContents(new StringSelection(msgForClipboard), null);
        }
    }

}
