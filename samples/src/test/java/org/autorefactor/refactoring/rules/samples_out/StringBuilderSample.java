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
import java.util.Arrays;

import javax.management.JMX;

public class StringBuilderSample {
    private static final String CONSTANT = "CONSTANT";

    public void replaceWithEfficientStringAppend(StringBuffer sbuf, StringBuilder sbui, String s) {
        // Keep this comment
        sbuf.append("foo " + "bar ").append(0).append(1);
        sbui.append("foo " + "bar ").append(0).append(1);
        sbuf.append("foo").append(0).append("bar").append(1);
        sbui.append("foo").append(0).append("bar").append(1);
    }

    public void replaceWithParenthesis(StringBuffer sbuf, StringBuilder sbui, String s) {
        // Keep this comment
        sbuf.append("foo " + "bar ").append((0)).append((1));
    }

    public void refactorOnExpression(Object sbuf, Object sbui, String s) {
        // Keep this comment
        ((StringBuffer) sbuf).append("foo " + "bar ").append(0).append(1);
        ((StringBuilder) sbui).append("foo " + "bar ").append(0).append(1);
        ((StringBuffer) sbuf).append("foo").append(0).append("bar").append(1);
        ((StringBuilder) sbui).append("foo").append(0).append("bar").append(1);
    }

    public void useConcatenation() {
        // Keep this comment
        String s1 = "foo " + "bar " + "baz";
        String s2 = "foo " + "bar " + "baz";
        String s3 = String.valueOf(0) + 1 + "bar";
        String s4 = String.valueOf(0) + 1 + "bar";
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

    public String useConcatenationOnSeveralStatements() {
        // Keep this comment
        String buffer = "baz " + "bar";
        String s1 = buffer;

        // Keep this comment too
        String builder = "baz " + "bar";
        String s2 = builder;

        return s1 + s2;
    }

    public String useConcatenationOnFluentAppending() {
        // Keep this comment
        String buffer = "baz " + "bar";
        String s1 = buffer;

        // Keep this comment too
        String builder = "baz " + "bar";
        String s2 = builder;

        return s1 + s2;
    }

    public String useConcatenationWithVariable(String text) {
        // Keep this comment
        final String buffer = "baz " + text;
        String s1 = buffer;

        // Keep this comment too
        final String builder = "baz " + text;
        String s2 = builder;

        return s1 + s2;
    }

    public String useConcatenationWithReassignment(String text) {
        // Keep this comment
        String buffer = "baz " + text;
        String s1 = buffer;

        // Keep this comment too
        String builder = "baz " + text;
        String s2 = builder;

        return s1 + s2;
    }

    public String useConcatenationWithAppendingOnConstructor(String text) {
        // Keep this comment
        String buffer = "baz " + text;
        String s1 = buffer;

        // Keep this comment too
        String builder = "baz " + text;
        String s2 = builder;

        return s1 + s2;
    }

    public String useConcatenationWithOnlyAppendingOnConstructor(String text) {
        // Keep this comment
        String buffer = "baz " + text;
        String s1 = buffer;

        // Keep this comment too
        String builder = "baz " + text;
        String s2 = builder;

        return s1 + s2;
    }

    public int useConcatenationWithSpecialMethod() {
        // Keep this comment
        String buffer = "baz " + "bar";
        int s1 = buffer.length();

        // Keep this comment too
        String builder = "baz " + "bar";
        int s2 = builder.length();

        return s1 + s2;
    }

    public void useConcatenationInsideAppend(StringBuffer sbuf, StringBuilder sbui) {
        // Keep this comment
        String s5 = sbuf.append("foo " + "bar").toString();
        String s6 = sbui.append("foo " + "bar").toString();
    }

    public void useConcatenationWithStringConstant(StringBuffer sbuf, StringBuilder sbui, String text) {
        // Keep this comment
        sbuf.append("<" + JMX.DEFAULT_VALUE_FIELD + ">").append(text);
        sbui.append("<" + JMX.DEFAULT_VALUE_FIELD + ">").append(text);
    }

    public void useConcatenationWithStringConstantField(StringBuffer sbuf, StringBuilder sbui, String text) {
        // Keep this comment
        sbuf.append("<" + CONSTANT + ">").append(text);
        sbui.append("<" + CONSTANT + ">").append(text);
    }

    public void removeUselessStringConcatenation() {
        // Keep this comment
        String s1 = Integer.toString(42);
        String s2 = "foo " + 0 + "bar " + 1;
        String s3 = 1 + "foo";
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

    public void removeValueOfCallsWithUselessCast(Object object) {
        StringBuilder sb = new StringBuilder();
        sb.append(object);
    }

    public void removeAppendEmptyString(StringBuilder builder, StringBuffer buffer) {
    }

    public void replaceAppendByBlock(StringBuilder builder, StringBuffer buffer, boolean isValid) {
        if (isValid) {
        }

        if (isValid) {
        }
    }

    public void removeElse(StringBuilder builder, StringBuffer buffer, boolean isValid) {
        if (isValid)
            System.out.println("foo");

        if (isValid)
            System.out.println("foo");
    }

    public String formatTime(int hour, int min) {
        // Keep this comment
        return (hour < 10 ? "0" + hour : hour) + ":" + (min < 10 ? "0" + min : min);
    }

    public void concatStringLiterals(StringBuffer sbuf, StringBuilder sbui, String s) {
        // Keep this comment
        sbuf.append("foo " + "bar ").append(0).append("foo").append(1).append("bar ").append(s);
        sbui.append("foo " + "bar ").append(0).append("foo").append(1).append("bar ").append(s);
    }

    public void replaceConcatWithAppend() {
        // Keep this comment
        StringBuilder builder = new StringBuilder("foo ").append(1);
        StringBuilder builder2 = new StringBuilder("foo ").append(1).append(" bar");
        StringBuilder builder3 = new StringBuilder("foo ").append(1).append(" bar ").append(2);
        StringBuilder builder4 = new StringBuilder("foo ").append(1).append(" bar ").append(2).append(" foo bar");
        StringBuilder builder5 = new StringBuilder("foo " + "bar ").append(2).append(" foo bar");
        StringBuilder builder6 = new StringBuilder("foo ").append(1).append(" bar ").append(2).append(" foo " + "bar");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("foo ").append(1);
        StringBuffer buffer2 = new StringBuffer("foo ").append(1).append(" bar");
        StringBuffer buffer3 = new StringBuffer("foo ").append(1).append(" bar ").append(2);
        StringBuffer buffer4 = new StringBuffer("foo ").append(1).append(" bar ").append(2).append(" foo bar");
        StringBuffer buffer5 = new StringBuffer("foo " + "bar ").append(2).append(" foo bar");
        StringBuffer buffer6 = new StringBuffer("foo ").append(1).append(" bar ").append(2).append(" foo " + "bar");
    }

    public void removeEmptyString() {
        // Keep this comment
        StringBuilder builder2 = new StringBuilder("foo");
        StringBuilder builder3 = new StringBuilder("foo");

        // Keep this comment too
        StringBuffer buffer2 = new StringBuffer("foo");
        StringBuffer buffer3 = new StringBuffer("foo");
    }

    public void replaceWithInteger() {
        // Keep this comment
        StringBuilder builder = new StringBuilder().append(1).append(" foo");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer().append(1).append(" foo");
    }

    public void rewriteAppending() {
        // Keep this comment
        StringBuilder builder = new StringBuilder("one: ").append(1).append(" end");
        StringBuilder builder2 = new StringBuilder("two: ").append(2).append(" bar" + " end");
        StringBuilder builder3 = new StringBuilder("three: ").append(3).append(" bar ").append(2).append(" end");
        StringBuilder builder4 = new StringBuilder("four: ").append(4).append(" bar ").append(2).append(" foo bar" + " end");
        StringBuilder builder5 = new StringBuilder("five" + ": ").append(5).append(" foo bar" + " end");
        StringBuilder builder6 = new StringBuilder("six: ").append(6).append(" bar ").append(2).append(" foo " + "bar" + " end");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("one: ").append(1).append(" end");
        StringBuffer buffer2 = new StringBuffer("two: ").append(2).append(" bar" + " end");
        StringBuffer buffer3 = new StringBuffer("three: ").append(3).append(" bar ").append(2).append(" end");
        StringBuffer buffer4 = new StringBuffer("four: ").append(4).append(" bar ").append(2).append(" foo bar" + " end");
        StringBuffer buffer5 = new StringBuffer("five" + ": ").append(5).append(" foo bar" + " end");
        StringBuffer buffer6 = new StringBuffer("six: ").append(6).append(" bar ").append(2).append(" foo " + "bar" + " end");
    }

    public void avoidStringCreation() {
        // Keep this comment
        StringBuilder builder = new StringBuilder().append(1);
        StringBuilder builder2 = new StringBuilder().append(2);
        StringBuilder builder3 = new StringBuilder().append(3).append("...");
        StringBuilder builder4 = new StringBuilder().append(4).append("...");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer().append(1);
        StringBuffer buffer2 = new StringBuffer().append(2);
        StringBuffer buffer3 = new StringBuffer().append(3).append("...");
        StringBuffer buffer4 = new StringBuffer().append(4).append("...");
    }

    public void shortenExpression() {
        // Keep this comment
        StringBuilder builder = new StringBuilder("Lorem ipsum");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("Lorem ipsum");
    }

    public int doNotRefactorWithDangerousMethod() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("baz ");
        buffer.append("bar");
        int s1 = buffer.capacity();

        StringBuilder builder = new StringBuilder();
        builder.append("baz ");
        builder.append("bar");
        int s2 = builder.capacity();

        return s1 + s2;
    }

    public String doNotRefactorWithoutAppending() {
        StringBuffer buffer = new StringBuffer();
        String s1 = buffer.toString();

        StringBuilder builder = new StringBuilder();
        String s2 = builder.toString();

        return s1 + s2;
    }

    public String doNotRefactorWithoutUse(String text) {
        StringBuffer buffer = new StringBuffer();
        buffer.append("baz ");
        buffer.append(text);

        StringBuilder builder = new StringBuilder();
        builder.append("baz ");
        builder.append(text);

        return "s1 + s2";
    }

    public Object doNotRefactorWithVariableEscape(String text) {
        StringBuffer buffer = new StringBuffer();
        buffer.append("baz ");
        buffer.append(text);
        String s1 = buffer.toString();

        StringBuilder builder = new StringBuilder();
        builder.append("baz ");
        builder.append(text);
        String s2 = builder.toString();

        return Arrays.<Object>asList(buffer, builder);
    }

    public String doNotRefactorWithLateAppending(String text) {
        StringBuffer buffer = new StringBuffer();
        buffer.append("baz ");
        buffer.append(text);
        String s1 = buffer.toString();
        buffer.append("Wait!");

        StringBuilder builder = new StringBuilder();
        builder.append("baz ");
        builder.append(text);
        String s2 = builder.toString();
        builder.append("Wait!");

        return s1 + s2;
    }

    public String doNotRefactorWithSelfAppending(String text) {
        StringBuffer buffer = new StringBuffer();
        buffer.append("baz ");
        buffer.append(buffer);
        String s1 = buffer.toString();

        StringBuilder builder = new StringBuilder();
        builder.append("baz ");
        builder.append(builder);
        String s2 = builder.toString();

        return s1 + s2;
    }

    public String doNotRefactorWithSelfAppending(int i) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(i++);
        buffer.append(i++);
        String s1 = buffer.toString();

        StringBuilder builder = new StringBuilder();
        builder.append(i++);
        builder.append(i++);
        String s2 = builder.toString();

        return s1 + s2;
    }

    public void doNotUseConcatenationWithAnyConstant(StringBuffer sbuf, StringBuilder sbui, String text) {
        sbuf.append("<").append(Integer.MAX_VALUE).append(">").append(text);
        sbui.append("<").append(Integer.MAX_VALUE).append(">").append(text);
    }

    public void doNotRemoveStringConcatenation() {
        String s1 = 1 + "";
    }

    public final void doNotRefactorForAppendable(Appendable buf, Object o) throws IOException {
        buf.append(o.toString());
    }

    public void doNotReplaceLiteralConcat() {
        StringBuilder builder = new StringBuilder("Do not " + "replace");

        // Keep this comment too
        StringBuffer buffer = new StringBuffer("Do not " + "replace");
    }
}
