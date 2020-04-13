/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

public class StringValueOfRatherThanConcatSample {
    public void replaceForcedConcatenationByStringValueOf(
            Object o, boolean b, char c, byte by, short s, int i, long l, float f, double d) {
        // Keep this comment
        String text = String.valueOf(o);

        // Keep this comment too
        text = String.valueOf(b);
        text = String.valueOf(c);
        text = String.valueOf(by);
        text = String.valueOf(s);
        text = String.valueOf(i);
        text = String.valueOf(l);
        text = String.valueOf(f);
        text = String.valueOf(d);

        // Keep this comment also
        text = String.valueOf(o);
        text = String.valueOf(b);
        text = String.valueOf(c);
        text = String.valueOf(by);
        text = String.valueOf(s);
        text = String.valueOf(i);
        text = String.valueOf(l);
        text = String.valueOf(f);
        text = String.valueOf(d);
    }

    public String replaceForcedLongConcatenationByStringValueOf(
            Object o, boolean b, char c, byte by, short s, int i, long l, float f, double d) {
        // Keep this comment
        return String.valueOf(o) + b + c + by + s + i + l + f + d;
    }

    public void doNotReplaceConcatenateWithCharArray(char[] chars) {
        String text = "" + chars;
    }
}