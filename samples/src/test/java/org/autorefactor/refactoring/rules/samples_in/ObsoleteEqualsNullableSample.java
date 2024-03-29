/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2020 Jean-Noël Rouvignac - initial API and implementation
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

public class ObsoleteEqualsNullableSample {
    private static final String NULL_CONSTANT = null;

    public void removeUselessNullCheck(String s) {
        // Remove redundant null checks
        boolean b1 = s != null && "".equals(s);
        boolean b2 = s != null && "".equalsIgnoreCase(s);
        boolean b3 = s != null && s instanceof String;

        // Remove redundant null checks
        boolean b4 = null != s && "".equals(s);
        boolean b5 = null != s && "".equalsIgnoreCase(s);
        boolean b6 = null != s && s instanceof String;
    }

    public boolean doNotRemoveUselessNullCheckOnInstance(Object o) {
        return o != null && equals(o);
    }

    public boolean doNotRemoveUselessNullCheckOnThis(Object o) {
        return o != null && this.equals(o);
    }

    public boolean removeExtendedNullCheck(boolean enabled, String s) {
        // Remove redundant null checks
        boolean b1 = enabled && s != null && "".equals(s);
        boolean b2 = enabled && s != null && "".equalsIgnoreCase(s);
        boolean b3 = enabled && s != null && s instanceof String;

        // Remove redundant null checks
        boolean b4 = enabled && null != s && "".equals(s);
        boolean b5 = enabled && null != s && "".equalsIgnoreCase(s);
        boolean b6 = enabled && null != s && s instanceof String;

        return b1 && b2 && b3 && b4 && b5 && b6;
    }

    public boolean removeExtendedNullCheck(boolean enabled, boolean isValid, String s) {
        // Remove redundant null checks
        boolean b1 = enabled && isValid && s != null && "".equals(s);
        boolean b2 = enabled && isValid && s != null && "".equalsIgnoreCase(s);
        boolean b3 = enabled && isValid && s != null && s instanceof String;

        // Remove redundant null checks
        boolean b4 = enabled && isValid && null != s && "".equals(s);
        boolean b5 = enabled && isValid && null != s && "".equalsIgnoreCase(s);
        boolean b6 = enabled && isValid && null != s && s instanceof String;

        return b1 && b2 && b3 && b4 && b5 && b6;
    }

    public boolean removeNullCheckInTheMiddle(boolean enabled, boolean isValid, String s) {
        // Remove redundant null checks
        boolean b1 = enabled && s != null && "".equals(s) && isValid;
        boolean b2 = enabled && s != null && "".equalsIgnoreCase(s) && isValid;
        boolean b3 = enabled && s != null && s instanceof String && isValid;

        // Remove redundant null checks
        boolean b4 = enabled && null != s && "".equals(s) && isValid;
        boolean b5 = enabled && null != s && "".equalsIgnoreCase(s) && isValid;
        boolean b6 = enabled && null != s && s instanceof String && isValid;

        return b1 && b2 && b3 && b4 && b5 && b6;
    }

    public boolean doNotRemoveNullCheck(String s) {
        // Do not remove non redundant null checks
        boolean b1 = s != null && s.equals(NULL_CONSTANT);
        boolean b2 = s != null && s.equalsIgnoreCase(NULL_CONSTANT);

        // Do not remove non redundant null checks
        boolean b3 = null != s && s.equals(NULL_CONSTANT);
        boolean b4 = null != s && s.equalsIgnoreCase(NULL_CONSTANT);

        return b1 && b2 && b3 && b4;
    }
}
