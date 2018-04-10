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
package org.autorefactor.refactoring.rules.samples_out;

public class StringSample {

    public String replaceValueOfOnStringLiteral() {
        // Keep this comment
        return "foo";
    }

    public String doNotReplaceNullableString(String s) {
        return String.valueOf(s);
    }

    public String replaceNewStringInMethodInvocation(String s, int i) {
        // Keep this comment
        return (s + i).toLowerCase();
    }

    public String replaceStringToString(String s) {
        // Keep this comment
        String s1 = s;
        String s2 = "";
        String s3 = getS();
        return s1 + s2 + s3;
    }

    public String replaceToStringCallInStringConcat() {
        // Keep this comment
        String s1 = "" + Boolean.TRUE + Boolean.FALSE;
        String s2 = Boolean.TRUE + "" + Boolean.FALSE;
        return s1 + s2;
    }

    public String replaceToStringCallOnCurrentObjectInStringConcat() {
        // Keep this comment
        String s1 = "" + this + this;
        String s2 = this + "" + this;
        return s1 + s2;
    }

    public class TestWithQualifiedToString {
        public String replaceToStringCallOnCurrentObjectInStringConcat() {
            // Keep this comment
            String s1 = "" + StringSample.this + StringSample.this + this;
            String s2 = StringSample.this + "" + this;
            return s1 + s2;
        }
    }

    public String doNotReplaceToStringCallOutsideStringConcat() {
        return Boolean.TRUE.toString();
    }

    public String doNotReplaceTwoConsecutiveToStringCalls() {
        // Keep this comment
        return Boolean.TRUE.toString() + Boolean.FALSE;
    }

    public String removeUselessToStringLeftOperand(byte b, short s) {
        // Keep this comment
        String s1 = true + " foo";
        String s2 = 'c' + " foo";
        String s3 = b + " foo";
        String s4 = s + " foo";
        String s5 = 42 + " foo";
        String s6 = 42l + " foo";
        String s7 = 42.42f + " foo";
        String s8 = 42.42 + " foo";

        String s9 = (int) 'c' + " foo";
        String s10 = (int) b + " foo";
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10;
    }

    public String removeUselessToStringRightOperand(byte b, short s) {
        // Keep this comment
        String s1 = "foo " + true;
        String s2 = "foo " + 'c';
        String s3 = "foo " + b;
        String s4 = "foo " + s;
        String s5 = "foo " + 42;
        String s6 = "foo " + 42l;
        String s7 = "foo " + 42.42f;
        String s8 = "foo " + 42.42;

        String s9 = "foo " + (int) 'c';
        String s10 = "foo " + (int) b;
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10;
    }

    public String removeUselessToStringExtendedOperand(byte b, short s) {
        // Keep this comment
        String s1 = "foo " + true + " bar";
        String s2 = "foo " + 'c' + " bar";
        String s3 = "foo " + b + " bar";
        String s4 = "foo " + s + " bar";
        String s5 = "foo " + 42 + " bar";
        String s6 = "foo " + 42l + " bar";
        String s7 = "foo " + 42.42f + " bar";
        String s8 = "foo " + 42.42 + " bar";

        String s9 = "foo " + (int) 'c' + " bar";
        String s10 = "foo " + (int) b + " bar";
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10;
    }

    public String removeUselessValueOfLeftOperand(byte b, short s) {
        // Keep this comment
        String s1 = true + " foo";
        String s2 = 'c' + " foo";
        String s3 = (int) b + " foo";
        String s4 = (int) s + " foo";
        String s5 = 42 + " foo";
        String s6 = 42l + " foo";
        String s7 = 42.42f + " foo";
        String s8 = 42.42 + " foo";
        String s9 = new Object() + " foo";
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9;
    }

    public String removeUselessValueOfRightOperand(byte b, short s) {
        // Keep this comment
        String s1 = "foo " + true;
        String s2 = "foo " + 'c';
        String s3 = "foo " + (int) b;
        String s4 = "foo " + (int) s;
        String s5 = "foo " + 42;
        String s6 = "foo " + 42l;
        String s7 = "foo " + 42.42f;
        String s8 = "foo " + 42.42;
        String s9 = "foo " + new Object();
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9;
    }

    public String removeUselessValueOfExtendedOperand(byte b, short s) {
        // Keep this comment
        String s1 = "foo " + true + " bar";
        String s2 = "foo " + 'c' + " bar";
        String s3 = "foo " + (int) b + " bar";
        String s4 = "foo " + (int) s + " bar";
        String s5 = "foo " + 42 + " bar";
        String s6 = "foo " + 42l + " bar";
        String s7 = "foo " + 42.42f + " bar";
        String s8 = "foo " + 42.42 + " bar";
        String s9 = "foo " + new Object() + " bar";
        return s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9;
    }

    public String onlyRefactorFirstStringValueOf(Object o1, Object o2) {
        // Keep this comment
        return o1 + String.valueOf(o2);
    }

    public String doNotRefactorStringValueOf1(Object o) {
        return String.valueOf(o);
    }

    public String doNotRefactorStringValueOf2(Object o) {
        // Keep this comment
        return "is null: " + (o == null);
    }

    private static String getS() {
        return null;
    }

    public void replaceCaseShift(String s1, String s2) {
        // Keep this comment
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        "lorem".equalsIgnoreCase("ipsum");
        "lorem".equalsIgnoreCase("ipsum");
    }

    public void doNotReplaceUnilateralCaseShift(String s1, String s2) {
        s1.toUpperCase().equals(s2);
        s1.toLowerCase().equals(s2);
        s1.equals(s2.toLowerCase());
        s1.equals(s2.toUpperCase());
    }

    public void doNotReplaceCaseIncompatibility(String s1, String s2) {
        s1.toLowerCase().equals(s2.toUpperCase());
    }

    public void simplifyInsensitiveCaseEquality(String s1, String s2) {
        // Keep this comment
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
        s1.equalsIgnoreCase(s2);
    }
    
    public void refactorIndexOf(){
        String b = "b";
        "a".indexOf('a', 0);
        b.indexOf('a');
        b.trim().indexOf('\t', 0);
        "a".indexOf('\n');
    }
    
    public void refactorlastIndexOfCases(){
        String b = "b";
        "a".lastIndexOf('a', 0);
        b.lastIndexOf('a');
        b.toLowerCase().lastIndexOf('\t', 0);
        "a".lastIndexOf('\n');
    }
    
    public void doNotRefactorInvocationsOtherThanOneChar(){
        "a".indexOf(1);
        "a".indexOf(1, 0);
        "a".indexOf("as", 0);
        "a".indexOf("\\t", 0);
        "a".indexOf("as");
        "a".indexOf("\\b");
        "a".lastIndexOf(1);
        "a".lastIndexOf(1, 0);
        "a".lastIndexOf("as", 0);
        "a".lastIndexOf("\\t", 0);
        "a".lastIndexOf("as");
        "a".lastIndexOf("\\b");
    }
}
