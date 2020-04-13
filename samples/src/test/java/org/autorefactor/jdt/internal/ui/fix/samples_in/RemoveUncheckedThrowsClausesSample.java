/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import java.sql.SQLException;

public class RemoveUncheckedThrowsClausesSample {

    public void refactorMethod() throws NullPointerException, Exception, Error {
        // Some code
    }

    public void refactorMethod2() throws IllegalArgumentException {
        // Some code
    }

    public void doNotRefactor() throws Exception, SQLException {
        // Some code
    }

    // Remove unchecked descendants from constructor
    public RemoveUncheckedThrowsClausesSample() throws AssertionError,
            NullPointerException, SQLException {
        // Some code
    }

    // Remove unchecked from constructor
    public RemoveUncheckedThrowsClausesSample(int i)
            throws RuntimeException, Error, Exception {
        // Some code
    }

}
