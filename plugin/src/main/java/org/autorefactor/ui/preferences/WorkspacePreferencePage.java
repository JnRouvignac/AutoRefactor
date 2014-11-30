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
package org.autorefactor.ui.preferences;

import org.autorefactor.AutoRefactorPlugin;
import org.autorefactor.preferences.PreferenceConstants;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import static org.autorefactor.preferences.PreferenceConstants.*;


/**
 * The Eclipse preference page for AutoRefactor.
 */
public class WorkspacePreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

    /** Default constructor. */
    public WorkspacePreferencePage() {
        super(GRID);
        setPreferenceStore(AutoRefactorPlugin.getDefault().getPreferenceStore());
        setDescription("AutoRefactor workbench preferences");
    }

    /** {@inheritDoc} */
    @Override
    protected void createFieldEditors() {
        addBooleanField(REMOVE_THIS_FOR_NON_STATIC_METHOD_ACCESS);
        addBooleanField(ADD_CURLY_BRACKETS_TO_STATEMENT_BODIES);

        addBooleanField(DEBUG_MODE_ON);
    }

    private void addBooleanField(PreferenceConstants pref) {
        addField(new BooleanFieldEditor(pref.getName(), pref.getDescription(), getFieldEditorParent()));
    }

    /** {@inheritDoc} */
    @Override
    public void init(IWorkbench workbench) {
    }

}
