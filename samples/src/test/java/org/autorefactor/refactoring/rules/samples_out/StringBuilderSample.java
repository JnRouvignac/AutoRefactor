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
package org.autorefactor.refactoring.rules.samples_out;

import java.io.IOException;

public class StringBuilderSample {

    public void replaceWithEfficientStringAppend(StringBuffer sbuf, StringBuilder sbui, String s) {
        // Keep this comment
        sbuf.append("foo ").append("bar ").append(0).append(1);
        sbui.append("foo ").append("bar ").append(0).append(1);
        sbuf.append("foo").append(0).append("bar").append(1);
        sbui.append("foo").append(0).append("bar").append(1);
    }

    public void replaceWithStringAppend() {
        // Keep this comment
        String s1 = "foo " + "bar " + "baz";
        String s2 = "foo " + "bar " + "baz";
        String s3 = "" + 0 + 1 + "bar";
        String s4 = "" + 0 + 1 + "bar";
        String s5 = "foo";
        String s6 = "foo";
        String s7 = "";
        String s8 = "";
        String s9 = "";
        String s10 = "";
        String s11 = String.valueOf(0);
        String s12 = String.valueOf(0);
        String s13 = "foo " + "bar " + "baz";
        String s14 = "foo " + "bar " + "baz";
    }

    public void doNotReplaceWithStringAppend(StringBuffer sbuf, StringBuilder sbui) {
        String s5 = sbuf.append("foo ").append("bar").toString();
        String s6 = sbui.append("foo ").append("bar").toString();
    }

    public void removeUselessStringConcatenation() {
        // Keep this comment
        String s1 = Integer.toString(42);
        String s2 = "foo " + 0 + "bar " + 1;
        String s3 = 1 + "foo";
    }

    public void doNotRemoveStringConcatenation() {
        String s1 = 1 + "";
    }

    public void removeUselessCallsToValueOfWithStringBuilderAppend(
        Object o, boolean b, char c, int i, long l, float f, double d) {
        StringBuilder sb = new StringBuilder();
        // Keep this comment
        sb.append(o);
        sb.append(b);
        sb.append(b);
        sb.append(c);
        sb.append(c);
        sb.append(i);
        sb.append(i);
        sb.append(l);
        sb.append(l);
        sb.append(f);
        sb.append(f);
        sb.append(d);
        sb.append(d);

        sb.append(o).append(b).append(b).append(c).append(c).append(i)
                .append(i).append(l).append(l).append(f).append(f).append(d)
                .append(d);
    }

    public void removeUselessCallsToValueOfWithStringBufferAppend(
            Object o, boolean b, char c, int i, long l, float f, double d) {
        StringBuffer sb = new StringBuffer();
        // Keep this comment
        sb.append(o);
        sb.append(b);
        sb.append(b);
        sb.append(c);
        sb.append(c);
        sb.append(i);
        sb.append(i);
        sb.append(l);
        sb.append(l);
        sb.append(f);
        sb.append(f);
        sb.append(d);
        sb.append(d);

        sb.append(o).append(b).append(b).append(c).append(c).append(i)
                .append(i).append(l).append(l).append(f).append(f).append(d)
                .append(d);
    }

    public void removeUselessCallsToToStringOfWithStringBuilderAppend(
            Object o, boolean bo, byte by, char c, short s, int i, long l, float f, double d) {
        StringBuilder sb = new StringBuilder();
        // Keep this comment
        sb.append(this);
        sb.append(o);
        sb.append(bo);
        sb.append(by);
        sb.append(c);
        sb.append(s);
        sb.append(i);
        sb.append(l);
        sb.append(f);
        sb.append(d);

        sb.append(this).append(o).append(bo).append(by).append(c).append(s)
                .append(i).append(l).append(f).append(d);
    }

    public void removeUselessCallsToToStringOfWithStringBufferAppend(
            Object o, boolean bo, byte by, char c, short s, int i, long l, float f, double d) {
        StringBuffer sb = new StringBuffer();
        // Keep this comment
        sb.append(this);
        sb.append(o);
        sb.append(bo);
        sb.append(by);
        sb.append(c);
        sb.append(s);
        sb.append(i);
        sb.append(l);
        sb.append(f);
        sb.append(d);

        sb.append(this).append(o).append(bo).append(by).append(c).append(s)
                .append(i).append(l).append(f).append(d);
    }

    public void removeValueOfCallsWithCast(char c, byte b, Character cObject, Byte bObject) {
        StringBuilder sb = new StringBuilder();
        sb.append((int) b);
        sb.append((int) c);
        sb.append((int) bObject);
        sb.append((int) cObject);
    }

    public void removeValueOfCallsWithoutCast(int i, Integer iObject) {
        StringBuilder sb = new StringBuilder();
        sb.append(i);
        sb.append(iObject);
    }

    public void removeSubstringDoubleArgsCallsWithAppend(String s) {
        // Keep this comment
        new StringBuilder().append(s, 0, 1);
        new StringBuffer().append(s, 0, 1);
        new StringBuilder().append(s, 0, 1);
        new StringBuffer().append(s, 0, 1);
    }

    public void doNotRemoveSubstringSingleArgCallsWithAppend(String s) {
        new StringBuilder().append(s.substring(1));
        new StringBuffer().append(s.substring(1));
    }

    public void removeAppendEmptyString(StringBuilder builder, StringBuffer buffer) {
    }

    public final void doNotRefactorForAppendable(Appendable buf, Object o) throws IOException {
        buf.append(o.toString());
    }

    public String formatTime(int hour, int min) {
        // Keep this comment
        return (hour < 10 ? "0" + hour : hour) + ":" + (min < 10 ? "0" + min : min);
    }
}
