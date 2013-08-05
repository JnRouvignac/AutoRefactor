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

	String ADD_ANGLE_BRACKETS_TO_STATEMENT_BODIES         = "add_angle_brackets_to_statement_bodies";
	String ADD_ANGLE_BRACKETS_TO_STATEMENT_BODIES_LABEL   = "Add angle brackets '{' and '}' to statement bodies";

	String REMOVE_THIS_FOR_NON_STATIC_METHOD_ACCESS       = "remove_this_for_non_static_method_access";
	String REMOVE_THIS_FOR_NON_STATIC_METHOD_ACCESS_LABEL = "Remove 'this' qualifier for non static method accesses";

	String JDT__ALWAYS_USE_THIS_FOR_NON_STATIC_METHOD_ACCESS       = "cleanup.always_use_this_for_non_static_method_access";
	String JDT__ALWAYS_USE_THIS_FOR_NON_STATIC_METHOD_ACCESS_LABEL = "Use 'this' qualifier for method accesses";

}
