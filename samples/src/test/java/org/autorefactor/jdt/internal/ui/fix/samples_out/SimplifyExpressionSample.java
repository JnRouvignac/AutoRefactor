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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.io.IOException;
import java.io.Reader;
import java.util.Collection;
import java.util.List;
import java.util.Random;

public class SimplifyExpressionSample {
    private static final String NULL_CONSTANT = null;

    private boolean addedToMakeCodeFail(boolean b1, boolean b2, Object o) {
        return !b1 && b2 && o != null && addedToMakeCodeFail(b1, b2, o);
    }

    public void removeUselessNullCheck(String s) {
        {
            // Remove redundant null checks
            boolean b1 = "".equals(s);
            boolean b2 = "".equalsIgnoreCase(s);
            boolean b3 = s instanceof String;
        }
        {
            // Remove redundant null checks
            boolean b1 = "".equals(s);
            boolean b2 = "".equalsIgnoreCase(s);
            boolean b3 = s instanceof String;
        }
        {
            // Remove redundant constant operands
            boolean b3 = s != null;
            boolean b4 = false;
            boolean b5 = true;
            boolean b6 = s != null;
            boolean b7 = s != null && s.startsWith("");
            boolean b8 = s == null || s.startsWith("");
            boolean b9 = s != null && s.startsWith("");
            boolean b10 = s == null || s.startsWith("");
            boolean b11 = s != null && s.startsWith("");
            boolean b12 = s == null || s.startsWith("");
        }
    }

    public void doNotRemoveNullCheck(String s) {
        {
            // Do not remove non redundant null checks
            boolean b1 = s != null && s.equals(NULL_CONSTANT);
            boolean b2 = s != null && s.equalsIgnoreCase(NULL_CONSTANT);
        }
        {
            // Do not remove non redundant null checks
            boolean b1 = null != s && s.equals(NULL_CONSTANT);
            boolean b2 = null != s && s.equalsIgnoreCase(NULL_CONSTANT);
        }
        {
            // Right-hand-side left unchanged because left-hand-side can have
            // side effects
            boolean b3 = s != null && false;
            boolean b4 = s != null || true;
        }
        {
            // Right-hand-side left unchanged because left-hand-side can have
            // side effects
            boolean b3 = null != s && false;
            boolean b4 = null != s || true;
        }
    }

    public void simplifyPrimitiveBooleanExpression(boolean b) {
        if (b) {
            int i = 0;
        }
        if (b) {
            int i = 0;
        }
        if (!b) {
            int i = 0;
        }
        if (!b) {
            int i = 0;
        }
        if (b) {
            int i = 0;
        }
        if (b) {
            int i = 0;
        }
        if (!b) {
            int i = 0;
        }
        if (!b) {
            int i = 0;
        }
    }

    public void simplifyBooleanWrapperExpression(Boolean b) {
        if (b) {
            int i = 0;
        }
        if (b) {
            int i = 0;
        }
        if (!b) {
            int i = 0;
        }
        if (!b) {
            int i = 0;
        }
    }

    public void doNotSimplifyBooleanWrapperExpression(Boolean b) {
        if (b == Boolean.TRUE) {
            int i = 0;
        }
        if (b != Boolean.FALSE) {
            int i = 0;
        }
        if (b == Boolean.FALSE) {
            int i = 0;
        }
        if (b != Boolean.TRUE) {
            int i = 0;
        }
    }

    public boolean reduceBooleanExpression(boolean b1, boolean b2) {
        boolean b3 = b1 == b2;
        boolean b4 = b1 ^ b2;
        boolean b5 = b1 ^ b2;
        boolean b6 = b1 ^ b2;
        boolean b7 = b1 == b2;
        boolean b8 = b1 == b2;
        boolean b9 = b1 ^ b2;
        boolean b10 = b1 == b2;
        boolean b11 = b1 == b2;
        return b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;
    }
}
