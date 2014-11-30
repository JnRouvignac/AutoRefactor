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

import org.autorefactor.preferences.PreferenceConstants;
import org.autorefactor.preferences.Preferences;
import org.eclipse.jface.preference.IPreferenceStore;

import static org.autorefactor.preferences.PreferenceConstants.*;

/**
 * Helper implementation for Eclipse preferences.
 */
public class EclipsePreferences implements Preferences {

    private final IPreferenceStore preferenceStore;

    /**
     * Builds an instance of this class.
     *
     * @param preferenceStore the preference store
     */
    public EclipsePreferences(IPreferenceStore preferenceStore) {
        this.preferenceStore = preferenceStore;
    }

    private boolean getBoolean(PreferenceConstants pref) {
        return preferenceStore.getBoolean(pref.getName());
    }

    /** {@inheritDoc} */
    @Override
    public boolean debugModeOn() {
        return getBoolean(DEBUG_MODE_ON);
    }

    /** {@inheritDoc} */
    @Override
    public boolean removeThisForNonStaticMethodAccess() {
        return getBoolean(REMOVE_THIS_FOR_NON_STATIC_METHOD_ACCESS);
    }

    /** {@inheritDoc} */
    @Override
    public boolean addCurlyBracketsToStatementBodies() {
        return getBoolean(ADD_CURLY_BRACKETS_TO_STATEMENT_BODIES);
    }
}
