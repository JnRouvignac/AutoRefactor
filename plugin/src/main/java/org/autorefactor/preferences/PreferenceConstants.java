/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.preferences;

/** Interface holding preference constants. */
public enum PreferenceConstants {
    /** Preference that turns debug mode on or off. */
    DEBUG_MODE_ON("debug_mode_on", "Enable debug mode (for developers)", Boolean.FALSE); //$NON-NLS-1$ //$NON-NLS-2$

    /** TODO use this for preferences initialization. */
    private static final String JDT_ALWAYS_USE_THIS_FOR_NON_STATIC_METHOD_ACCESS= "cleanup.always_use_this_for_non_static_method_access"; //$NON-NLS-1$

    /** The preference name (not shown to the user). */
    private final String name;
    /** The preference description (shown to the user). */
    private final String description;
    /** The default value of the preference. */
    private final Object defaultValue;

    /**
     * Builds an instance of this class.
     *
     * @param name        the preference name
     * @param description the preference description
     */
    PreferenceConstants(final String name, final String description, final Object defaultValue) {
        this.name= name;
        this.description= description;
        this.defaultValue= defaultValue;
    }

    /**
     * Returns the preference default value. This default value is used during
     * preference initialization.
     *
     * @return the preference default value
     */
    public Object getDefaultValue() {
        return defaultValue;
    }

    /**
     * Returns the preference description. This description is displayed on the UI.
     *
     * @return the preference description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Returns the preference name. The preference name must uniquely identify the
     * preference in the AutoRefactor project.
     *
     * @return the preference name
     */
    public String getName() {
        return name;
    }
}
