/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class UseDiamondOperatorSample {

    public List<String> refactorVariableDeclarationStatement() {
        // Keep this comment
        List<String> l = new ArrayList<String>();
        return l;
    }

    public List<String> refactorVariableDeclarationStatementWithParentheses() {
        // Keep this comment
        List<String> l = ((new ArrayList<String>()));
        return l;
    }

    public List<String> refactorAssignment() {
        List<String> l;
        // Keep this comment
        l = new ArrayList<String>();
        return l;
    }

    public List<String> refactorReturnStatement() {
        // Keep this comment
        return new ArrayList<String>();
    }

    public List<String> refactorReturnStatementWithParameter(List<String> l) {
        // Keep this comment
        return new ArrayList<String>(l);
    }

    public Map<String, String> refactorReturnStatementWithParameter(Map<String, String> l) {
        // Keep this comment
        return new HashMap<String, String>(l);
    }

    public List<Object> doNotRefactorChangeOfType(List<String> col) {
        return new ArrayList<Object>(col);
    }

    public Map<String, Object> doNotRefactorChangeOfType(Map<String, String> col) {
        return new HashMap<String, Object>(col);
    }

    /**
     * @see <a href="https://stuartmarks.wordpress.com/2011/04/29/when-should-diamond-be-used/">
     * When Should Diamond Be Used?</a>
     */
    public void doNotRefactorMethodArgument() {
        List<String> list2 = Collections.synchronizedList(new ArrayList<String>());
        System.out.println(list2);
    }

    /**
     * @see <a href="https://stuartmarks.wordpress.com/2011/04/29/when-should-diamond-be-used/">
     * When Should Diamond Be Used?</a>
     */
    public void refactorMethodArgumentInferToObject() {
        List<Object> list3 = Collections.synchronizedList(new ArrayList<Object>()); // FIXME refactor
        System.out.println(list3);
    }

    /**
     * @see <a href="https://stuartmarks.wordpress.com/2011/04/29/when-should-diamond-be-used/">
     * When Should Diamond Be Used?</a>
     */
    public void refactorMethodArgumentInferTypeFromOutside(List<String> l) {
        List<String> list4 = Collections.synchronizedList(new ArrayList<String>(l)); // FIXME refactor
        System.out.println(list4);
    }

    /**
     * @see <a href="https://stuartmarks.wordpress.com/2011/04/29/when-should-diamond-be-used/">
     * When Should Diamond Be Used?</a>
     */
    public void refactorMethodArgumentInferTypeFromOutside2(List<String> l) {
        List<? extends String> list6 = Collections.synchronizedList(new ArrayList<String>(l)); // FIXME refactor
        System.out.println(list6);
    }

    /**
     * @see <a href="http://docs.oracle.com/javase/7/docs/technotes/guides/language/type-inference-generic-instance-creation.html">
     * Type Inference for Generic Instance Creation</a>
     */
    public List<String> doNotRefactor() {
        List<String> list = new ArrayList<>();
        list.addAll(new ArrayList<String>());
        return list;
    }

    public List<String> doNotRefactorAnonymousClass() {
        return new ArrayList<String>() {
            @Override
            public String toString() {
                return super.toString();
            }
        };
    }

    private static class StringComparator implements Comparator<String> {
        @Override
        public int compare(String s1, String s2) {
            return s1.compareTo(s2);
        }
    }

    public Set<String> noExceptionThrownByRefactoringRule() {
        Set<String> s = new TreeSet<>(new StringComparator());
        return s;
    }

    public Set<String> refactorRemoveGenericType() {
        // Keep this comment
        Set<String> s = new TreeSet<String>(new StringComparator());
        return s;
    }

    public static final ParameterizedType EMPTY_NODE = null;

    class ParameterizedType<T extends Comparable<T>> {
        ParameterizedType(ParameterizedType<? extends Date> parameterizedArgument) {}
    }

    <T extends Comparable<T>> ParameterizedType<T> doNotUseDiamondOperatorForNotParameterizedArgument() {
        return new ParameterizedType<T>(EMPTY_NODE);
    }
}
