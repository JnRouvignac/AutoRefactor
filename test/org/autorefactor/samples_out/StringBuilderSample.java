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
package org.autorefactor.samples_out;

public class StringBuilderSample {

    public void replaceWithEfficientStringAppend(StringBuffer sbuf, StringBuilder sbui, String s) {
        sbuf.append("foo ").append("bar ").append(0).append(1);
        sbui.append("foo ").append("bar ").append(0).append(1);
        sbuf.append("foo").append(0).append("bar").append(1);
        sbui.append("foo").append(0).append("bar").append(1);
    }

    public void replaceWithStringAppend() {
        String s1 = "foo " + "bar " + "baz";
        String s2 = "foo " + "bar " + "baz";
        String s3 = 0 + 1 + "bar";
        String s4 = 0 + 1 + "bar";
    }

    public void doNotReplaceWithStringAppend(StringBuffer sbuf, StringBuilder sbui) {
        String s5 = sbuf.append("foo ").append("bar").toString();
        String s6 = sbui.append("foo ").append("bar").toString();
    }

    public void removeUselessStringConcatenation() {
        String s1 = Integer.toString(42);
        // FIXME First empty string is not removed.
        // This is a bug with ASTNode.resolveConstantExpressionValue()
        String s2 = "foo " + "" + 0 + "bar " + 1;
        String s3 = 1 + "foo";
    }

    public void doNotRemoveStringConcatenation() {
        String s1 = 1 + "";
    }

}
