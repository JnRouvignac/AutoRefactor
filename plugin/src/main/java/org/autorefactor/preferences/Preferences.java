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
package org.autorefactor.preferences;

/**
 * Helper interface for preferences.
 */
public interface Preferences {

    /**
     * Returns whether debug mode is on.
     *
     * @return true if debug mode is on, false otherwise.
     */
    boolean debugModeOn();

    /**
     * Returns whether to remove 'this' keyword for accesses to non static methods.
     *
     * @return true if must remove 'this' keyword for accesses to non static methods, false otherwise
     */
    boolean removeThisForNonStaticMethodAccess();

    /**
     * Returns whether debug mode is on.
     *
     * @return true if debug mode is on, false otherwise.
     */
    boolean addCurlyBracketsToStatementBodies();

}
