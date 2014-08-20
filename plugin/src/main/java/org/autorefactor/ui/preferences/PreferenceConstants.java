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

/**
 * Interface holding preference constants.
 */
public interface PreferenceConstants {

    /** Class that groups preference related data. */
    class Preference {
        /** The preference name (not shown to the user). */
        final String name;
        /** The preference description (shown to the user). */
        final String description;

        /**
         * Builds an instance of this class.
         *
         * @param name the preference name
         * @param description the preference description
         */
        public Preference(String name, String description) {
            this.name = name;
            this.description = description;
        }
    }

    /** Preference that turns debug mode on or off. */
    Preference DEBUG_MODE_ON = new Preference(
        "debug_mode_on",
        "Enable debug mode (for developers only)");

    /** Preference that configures whether to add brackets to statement bodies. */
    Preference ADD_ANGLE_BRACKETS_TO_STATEMENT_BODIES = new Preference(
        "add_angle_brackets_to_statement_bodies",
        "Add angle brackets '{' and '}' to statement bodies");

    /** Preference that configures whether to remove <code>this</code> for non static method accesses. */
    Preference REMOVE_THIS_FOR_NON_STATIC_METHOD_ACCESS = new Preference(
        "remove_this_for_non_static_method_access",
        "Remove 'this' qualifier for non static method accesses");

    /** TODO use this for preferences initialization. */
    String JDT_ALWAYS_USE_THIS_FOR_NON_STATIC_METHOD_ACCESS =
        "cleanup.always_use_this_for_non_static_method_access";

}
