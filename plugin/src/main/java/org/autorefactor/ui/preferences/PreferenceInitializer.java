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
import org.autorefactor.util.NotImplementedException;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import static org.autorefactor.preferences.PreferenceConstants.*;

/**
 * Initializes the Eclipse preferences for AutoRefactor.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    /** {@inheritDoc} */
    @Override
    public void initializeDefaultPreferences() {
        // TODO initialize preferences from the JDT preferences like:
        // code style/cleanup/formatting
        final IPreferenceStore store = AutoRefactorPlugin.getDefault().getPreferenceStore();
        for (PreferenceConstants preference : PreferenceConstants.values()) {
            final String name = preference.getName();
            final Object defaultValue = preference.getDefaultValue();
            if (defaultValue instanceof Boolean) {
                store.setDefault(name, (Boolean) defaultValue);
            } else if (defaultValue instanceof Integer) {
                store.setDefault(name, (Integer) defaultValue);
            } else if (defaultValue instanceof Long) {
                store.setDefault(name, (Long) defaultValue);
            } else if (defaultValue instanceof Double) {
                store.setDefault(name, (Double) defaultValue);
            } else if (defaultValue instanceof Float) {
                store.setDefault(name, (Float) defaultValue);
            } else if (defaultValue instanceof String) {
                store.setDefault(name, (String) defaultValue);
            } else {
                throw new NotImplementedException(null, defaultValue);
            }
        }
        store.setDefault(ADD_CURLY_BRACKETS_TO_STATEMENT_BODIES.getName(), true);
    }

}
