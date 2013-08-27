/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

public interface PreferenceConstants {

	class Preference {
		final String name;
		final String description;

		public Preference(String name, String description) {
			this.name = name;
			this.description = description;
		}
	}

	Preference ADD_ANGLE_BRACKETS_TO_STATEMENT_BODIES = new Preference(
		"add_angle_brackets_to_statement_bodies",
		"Add angle brackets '{' and '}' to statement bodies");

	Preference REMOVE_THIS_FOR_NON_STATIC_METHOD_ACCESS = new Preference(
		"remove_this_for_non_static_method_access",
		"Remove 'this' qualifier for non static method accesses");

	/** TODO use this for preferences initialization */
	String JDT__ALWAYS_USE_THIS_FOR_NON_STATIC_METHOD_ACCESS =
		"cleanup.always_use_this_for_non_static_method_access";

}
