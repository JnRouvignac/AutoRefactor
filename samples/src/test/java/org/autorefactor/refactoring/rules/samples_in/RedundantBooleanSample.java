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
package org.autorefactor.refactoring.rules.samples_in;

import java.io.IOException;
import java.io.Reader;
import java.util.Collection;
import java.util.List;
import java.util.Random;

public class RedundantBooleanSample {
    private boolean addedToMakeCodeFail(boolean b1, boolean b2, Object o) {
        return !b1 && b2 && o != null && addedToMakeCodeFail(b1, b2, o);
    }

    public void removeUselessNullCheck(String s) {
        // Remove redundant constant operands
        boolean b0 = true && s != null;
        boolean b1 = false && s != null;
        boolean b2 = true || s != null;
        boolean b3 = false || s != null;
        boolean b4 = true && s != null && s.startsWith("");
        boolean b5 = false || s == null || s.startsWith("");
        boolean b6 = s != null && true && s.startsWith("");
        boolean b7 = s == null || false || s.startsWith("");
        boolean b8 = s != null && s.startsWith("") && true;
        boolean b9 = s == null || s.startsWith("") || false;
    }

    public boolean doNotRemoveNullCheck(String s) {
        // Right-hand-side left unchanged because left-hand-side can have
        // side effects
        boolean b1 = s != null && false;
        boolean b2 = s != null || true;

        // Right-hand-side left unchanged because left-hand-side can have
        // side effects
        boolean b3 = null != s && false;
        boolean b4 = null != s || true;

        return b1 && b2 && b3;
    }
}
