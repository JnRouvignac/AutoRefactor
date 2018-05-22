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

import javax.management.JMX;

public class StringBuilderSample {

    public void replaceWithEfficientStringAppend(StringBuffer sbuf, StringBuilder sbui, String s) {
        // Keep this comment
        sbuf.append("" + "foo ").append("bar " + 0).append("" + 1);
        sbui.append("" + "foo ").append("bar " + 0).append("" + 1);
        sbuf.append("foo" + 0 + "bar" + 1);
        sbui.append("foo" + 0 + "bar" + 1);
    }

    public void refactorOnExpression(Object sbuf, Object sbui, String s) {
        // Keep this comment
        ((StringBuffer) sbuf).append("" + "foo ").append("bar " + 0).append("" + 1);
        ((StringBuilder) sbui).append("" + "foo ").append("bar " + 0).append("" + 1);
        ((StringBuffer) sbuf).append("foo" + 0 + "bar" + 1);
        ((StringBuilder) sbui).append("foo" + 0 + "bar" + 1);
    }

    public void useConcatenation() {
        // Keep this comment
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
        String s13 = (((new StringBuffer("foo ")).append("bar ")).append("baz")).toString();
        String s14 = (((new StringBuilder("foo ")).append("bar ")).append("baz")).toString();
    }

    public void useConcatenationInsideAppend(StringBuffer sbuf, StringBuilder sbui) {
        // Keep this comment
        String s5 = sbuf.append("foo ").append("bar").toString();
        String s6 = sbui.append("foo ").append("bar").toString();
    }

    public void useConcatenationWithStringConstant(StringBuffer sbuf, StringBuilder sbui, String text) {
        // Keep this comment
        sbuf.append("<").append(JMX.DEFAULT_VALUE_FIELD).append(">").append(text);
        sbui.append("<").append(JMX.DEFAULT_VALUE_FIELD).append(">").append(text);
    }

    public void doNotUseConcatenationWithAnyConstant(StringBuffer sbuf, StringBuilder sbui, String text) {
        sbuf.append("<").append(Integer.MAX_VALUE).append(">").append(text);
        sbui.append("<").append(Integer.MAX_VALUE).append(">").append(text);
    }

    public void removeUselessStringConcatenation() {
        // Keep this comment
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
        // Keep this comment
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

        sb.append(String.valueOf(o)).append(String.valueOf(b)).append(Boolean.valueOf(b)).append(String.valueOf(c))
                .append(Character.valueOf(c)).append(String.valueOf(i)).append(Integer.valueOf(i))
                .append(String.valueOf(l)).append(Long.valueOf(l)).append(String.valueOf(f)).append(Float.valueOf(f))
                .append(String.valueOf(d)).append(Double.valueOf(d));
    }

    public void removeUselessCallsToValueOfWithStringBufferAppend(
            Object o, boolean b, char c, int i, long l, float f, double d) {
        StringBuffer sb = new StringBuffer();
        // Keep this comment
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

        sb.append(String.valueOf(o)).append(String.valueOf(b)).append(Boolean.valueOf(b)).append(String.valueOf(c))
                .append(Character.valueOf(c)).append(String.valueOf(i)).append(Integer.valueOf(i))
                .append(String.valueOf(l)).append(Long.valueOf(l)).append(String.valueOf(f)).append(Float.valueOf(f))
                .append(String.valueOf(d)).append(Double.valueOf(d));
    }

    public void removeUselessCallsToToStringOfWithStringBuilderAppend(
            Object o, boolean bo, byte by, char c, short s, int i, long l, float f, double d) {
        StringBuilder sb = new StringBuilder();
        // Keep this comment
        sb.append(toString());
        sb.append(o.toString());
        sb.append(Boolean.toString(bo));
        sb.append(Byte.toString(by));
        sb.append(Character.toString(c));
        sb.append(Short.toString(s));
        sb.append(Integer.toString(i));
        sb.append(Long.toString(l));
        sb.append(Float.toString(f));
        sb.append(Double.toString(d));

        sb.append(toString()).append(o.toString()).append(Boolean.toString(bo)).append(Byte.toString(by))
                .append(Character.toString(c)).append(Short.toString(s)).append(Integer.toString(i))
                .append(Long.toString(l)).append(Float.toString(f)).append(Double.toString(d));
    }

    public void removeUselessCallsToToStringOfWithStringBufferAppend(
            Object o, boolean bo, byte by, char c, short s, int i, long l, float f, double d) {
        StringBuffer sb = new StringBuffer();
        // Keep this comment
        sb.append(toString());
        sb.append(o.toString());
        sb.append(Boolean.toString(bo));
        sb.append(Byte.toString(by));
        sb.append(Character.toString(c));
        sb.append(Short.toString(s));
        sb.append(Integer.toString(i));
        sb.append(Long.toString(l));
        sb.append(Float.toString(f));
        sb.append(Double.toString(d));

        sb.append(toString()).append(o.toString()).append(Boolean.toString(bo)).append(Byte.toString(by))
                .append(Character.toString(c)).append(Short.toString(s)).append(Integer.toString(i))
                .append(Long.toString(l)).append(Float.toString(f)).append(Double.toString(d));
    }

    public void removeValueOfCallsWithCast(char c, byte b, Character cObject, Byte bObject) {
        StringBuilder sb = new StringBuilder();
        sb.append(Integer.valueOf(b));
        sb.append(Integer.valueOf(c));
        sb.append(Integer.valueOf(bObject));
        sb.append(Integer.valueOf(cObject));
    }

    public void removeValueOfCallsWithoutCast(int i, Integer iObject) {
        StringBuilder sb = new StringBuilder();
        sb.append(Integer.valueOf(i));
        sb.append(Integer.valueOf(iObject));
    }

    public void removeSubstringDoubleArgsCallsWithAppend(String s) {
        // Keep this comment
        new StringBuilder().append(s.substring(0, 1));
        new StringBuffer().append(s.substring(0, 1));
        new StringBuilder().append(s.subSequence(0, 1));
        new StringBuffer().append(s.subSequence(0, 1));
    }

    public void doNotRemoveSubstringSingleArgCallsWithAppend(String s, StringBuilder builder, StringBuffer buffer) {
        builder.append(s.substring(1));
        buffer.append(s.substring(1));
    }

    public void removeAppendEmptyString(StringBuilder builder, StringBuffer buffer) {
        // Keep this comment
        builder.append("");
        buffer.append("");
    }

    public final void doNotRefactorForAppendable(Appendable buf, Object o) throws IOException {
        buf.append(o.toString());
    }

    public String formatTime(int hour, int min) {
        // Keep this comment
        return new StringBuilder()
            .append(hour < 10 ? "0" + hour : hour)
            .append(":")
            .append(min < 10 ? "0" + min : min)
            .toString();
    }

    public void concatStringLiterals(StringBuffer sbuf, StringBuilder sbui, String s) {
        // Keep this comment
        sbuf.append("").append("foo ").append("bar ").append(0).append("foo").append(1).append("bar ").append(s);
        sbui.append("").append("foo ").append("bar ").append(0).append("foo").append(1).append("bar ").append(s);
    }

    public void replaceConcatWithAppend() {
        // Keep this comment
        StringBuilder builder = new StringBuilder("foo " + 1);
        StringBuilder builder2 = new StringBuilder("foo " + 1 + " bar");
        StringBuilder builder3 = new StringBuilder("foo " + 1 + " bar " + 2);
        StringBuilder builder4 = new StringBuilder("foo " + 1 + " bar " + 2 + " foo bar");
        StringBuilder builder5 = new StringBuilder("foo " + "bar " + 2 + " foo bar");
        StringBuilder builder6 = new StringBuilder("foo " + 1 + " bar " + 2 + " foo " + "bar");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("foo " + 1);
        StringBuffer buffer2 = new StringBuffer("foo " + 1 + " bar");
        StringBuffer buffer3 = new StringBuffer("foo " + 1 + " bar " + 2);
        StringBuffer buffer4 = new StringBuffer("foo " + 1 + " bar " + 2 + " foo bar");
        StringBuffer buffer5 = new StringBuffer("foo " + "bar " + 2 + " foo bar");
        StringBuffer buffer6 = new StringBuffer("foo " + 1 + " bar " + 2 + " foo " + "bar");
    }

    public void removeEmptyString() {
        // Keep this comment
        StringBuilder builder2 = new StringBuilder("foo" + "");
        StringBuilder builder3 = new StringBuilder("" + "foo");

        // Keep this comment too
        StringBuffer buffer2 = new StringBuffer("foo" + "");
        StringBuffer buffer3 = new StringBuffer("" + "foo");
    }

    public void doNotReplaceLiteralConcat() {
        StringBuilder builder = new StringBuilder("Do not " + "replace");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("Do not " + "replace");
    }

    public void replaceWithInteger() {
        // Keep this comment
        StringBuilder builder = new StringBuilder(1 + " foo");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer(1 + " foo");
    }

    public void rewriteAppending() {
        // Keep this comment
        StringBuilder builder = new StringBuilder("one: " + 1).append(" end");
        StringBuilder builder2 = new StringBuilder("two: " + 2 + " bar").append(" end");
        StringBuilder builder3 = new StringBuilder("three: " + 3 + " bar " + 2).append(" end");
        StringBuilder builder4 = new StringBuilder("four: " + 4 + " bar " + 2 + " foo bar").append(" end");
        StringBuilder builder5 = new StringBuilder("five" + ": " + 5 + " foo bar").append(" end");
        StringBuilder builder6 = new StringBuilder("six: " + 6 + " bar " + 2 + " foo " + "bar").append(" end");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("one: " + 1).append(" end");
        StringBuffer buffer2 = new StringBuffer("two: " + 2 + " bar").append(" end");
        StringBuffer buffer3 = new StringBuffer("three: " + 3 + " bar " + 2).append(" end");
        StringBuffer buffer4 = new StringBuffer("four: " + 4 + " bar " + 2 + " foo bar").append(" end");
        StringBuffer buffer5 = new StringBuffer("five" + ": " + 5 + " foo bar").append(" end");
        StringBuffer buffer6 = new StringBuffer("six: " + 6 + " bar " + 2 + " foo " + "bar").append(" end");
    }

    public void avoidStringCreation() {
        // Keep this comment
        StringBuilder builder = new StringBuilder(Integer.toString(1));
        StringBuilder builder2 = new StringBuilder(String.valueOf(2));
        StringBuilder builder3 = new StringBuilder(Integer.toString(3)).append("...");
        StringBuilder builder4 = new StringBuilder(String.valueOf(4)).append("...");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer(Integer.toString(1));
        StringBuffer buffer2 = new StringBuffer(String.valueOf(2));
        StringBuffer buffer3 = new StringBuffer(Integer.toString(3)).append("...");
        StringBuffer buffer4 = new StringBuffer(String.valueOf(4)).append("...");
    }

    public void shortenExpression() {
        // Keep this comment
        StringBuilder builder = new StringBuilder().append("Lorem ipsum");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer().append("Lorem ipsum");
    }
}
