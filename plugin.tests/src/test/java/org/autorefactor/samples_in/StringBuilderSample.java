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
package org.autorefactor.samples_in;

public class StringBuilderSample {

    public void replaceWithEfficientStringAppend(StringBuffer sbuf, StringBuilder sbui, String s) {
        sbuf.append("" + "foo ").append("bar " + 0).append("" + 1);
        sbui.append("" + "foo ").append("bar " + 0).append("" + 1);
        sbuf.append("foo" + 0 + "bar" + 1);
        sbui.append("foo" + 0 + "bar" + 1);
    }

    public void replaceWithStringAppend() {
        String s1 = new StringBuffer("foo ").append("bar ").append("baz").toString();
        String s2 = new StringBuilder("foo ").append("bar ").append("baz").toString();
        String s3 = new StringBuffer().append(0).append(1).append("bar").toString();
        String s4 = new StringBuilder().append(0).append(1).append("bar").toString();
        String s5 = new StringBuffer("foo").toString();
        String s6 = new StringBuilder("foo").toString();
        String s7 = new StringBuffer(0).toString();
        String s8 = new StringBuilder(0).toString();
        String s9 = new StringBuffer().toString();
        String s10 = new StringBuilder().toString();
        String s11 = new StringBuffer().append(0).toString();
        String s12 = new StringBuilder().append(0).toString();
    }

    public void doNotReplaceWithStringAppend(StringBuffer sbuf, StringBuilder sbui) {
        String s5 = sbuf.append("foo ").append("bar").toString();
        String s6 = sbui.append("foo ").append("bar").toString();
    }

    public void removeUselessStringConcatenation() {
        String s1 = Integer.toString(42) + "" + "";
        String s2 = "foo " + "" + 0 + "" + "bar " + 1 + "";
        String s3 = 1 + "" + "foo";
    }

    public void doNotRemoveStringConcatenation() {
        String s1 = 1 + "";
    }

    public void removeUselessCallsToValueOfWithStringBuilderAppend(
        Object o, boolean b, char c, int i, long l, float f, double d) {
        StringBuilder sb = new StringBuilder();
        sb.append(String.valueOf(o));
        sb.append(String.valueOf(b));
        sb.append(Boolean.valueOf(b));
        sb.append(String.valueOf(c));
        sb.append(Character.valueOf(c));
        sb.append(String.valueOf(i));
        sb.append(Integer.valueOf(i));
        sb.append(String.valueOf(l));
        sb.append(Long.valueOf(l));
        sb.append(String.valueOf(f));
        sb.append(Float.valueOf(f));
        sb.append(String.valueOf(d));
        sb.append(Double.valueOf(d));
    }

    public void removeUselessCallsToValueOfWithStringBufferAppend(
            Object o, boolean b, char c, int i, long l, float f, double d) {
        StringBuffer sb = new StringBuffer();
        sb.append(String.valueOf(o));
        sb.append(String.valueOf(b));
        sb.append(Boolean.valueOf(b));
        sb.append(String.valueOf(c));
        sb.append(Character.valueOf(c));
        sb.append(String.valueOf(i));
        sb.append(Integer.valueOf(i));
        sb.append(String.valueOf(l));
        sb.append(Long.valueOf(l));
        sb.append(String.valueOf(f));
        sb.append(Float.valueOf(f));
        sb.append(String.valueOf(d));
        sb.append(Double.valueOf(d));
    }


    public void removeUselessCallsWithAppend(String s) {
        new StringBuilder().append(s.substring(0, 1));
        new StringBuffer().append(s.substring(0, 1));
        new StringBuilder().append(s.subSequence(0, 1));
        new StringBuffer().append(s.subSequence(0, 1));
    }
}
