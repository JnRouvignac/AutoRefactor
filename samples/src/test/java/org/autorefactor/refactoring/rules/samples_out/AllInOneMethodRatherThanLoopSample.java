/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Separate the code.
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

import java.util.AbstractSet;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class AllInOneMethodRatherThanLoopSample {

    public void replaceAddWithForLoopByCollectionsAddAll(
            List<? super java.util.Date> output, java.util.Date[] elems1, java.sql.Date[] elems2) {
        // Keep this comment
        java.util.Collections.addAll(output, elems1);
        java.util.Collections.addAll(output, elems2);
    }

    public void replaceAddWithForEachByCollectionsAddAll(
            List<? super java.util.Date> output, java.util.Date[] elems1, java.sql.Date[] elems2) {
        // Keep this comment
        java.util.Collections.addAll(output, elems1);
        java.util.Collections.addAll(output, elems2);
    }

    public void replaceLoopOnCollectionAsExpressionWithArray(
            Map<String, List<String>> mapToFill, String[] inputList) {
        // Keep this comment
        java.util.Collections.addAll(mapToFill.get("foo"), inputList);
    }

    public void replaceLoopOnRawCollectionWithArray(
            List colToFill, String[] inputList) {
        // Keep this comment
        java.util.Collections.addAll(colToFill, inputList);
    }

    public void replaceLoopOnCollectionAsExpressionWithList(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        // Keep this comment
        mapToFill.get("foo").addAll(inputList);
    }

    public void replaceLoopOnRawCollectionWithList(
            List colToFill, List<String> inputList) {
        // Keep this comment
        colToFill.addAll(inputList);
    }

    public void doNotRefactorForEachWithListUsingLoopVariable(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        for (String input : inputList) {
            mapToFill.get(input).add(input);
        }
    }

    public void doNotRefactorForLoopWithListUsingLoopIndex(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        for (int i = 0; i < inputList.size(); i++) {
            mapToFill.get(inputList.get(i)).add(inputList.get(i));
        }
    }

    public void doNotRefactorForLoopWithListUsingLoopIterator(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        String input = null;
        for (Iterator<String> it = inputList.iterator(); it.hasNext(); input = it.next()) {
            mapToFill.get(input).add(input);
        }
    }

    public void doNotRefactorForLoopWithListUsingLoopIterator(List<String> col) {
        for (Iterator<String> it = col.iterator(); it.hasNext();) {
            System.out.println(it.next());
        }
    }

    public void doNotRefactorForEachWithArrayUsingLoopVariable(
            Map<String, List<String>> mapToFill, String[] inputArray) {
        for (String input : inputArray) {
            mapToFill.get(input).add(input);
        }
    }

    public void doNotRefactorForLoopWithArrayUsingLoopIndex(
            Map<String, List<String>> mapToFill, String[] inputArray) {
        for (int i = 0; i < inputArray.length; i++) {
            mapToFill.get(inputArray[i]).add(inputArray[i]);
        }
    }

    public void doNotRefactorForLoopAddMethodResult(List<String> output, String[] elems) {
        for (int i = 0; i < elems.length; i++) {
            output.add(doSomething(elems[i]));
        }
    }

    public void doNotRefactorForEachAddMethodResult(List<String> output, String[] elems) {
        for (String s : elems) {
            output.add(doSomething(s));
        }
    }

    public void replaceAddWithForLoopByAddAll(List<String> col, List<String> output) {
        // Keep this comment
        output.addAll(col);
    }

    public void replaceAddWithForEachByAddAll(Collection<String> col, List<String> output) {
        // Keep this comment
        output.addAll(col);
    }

    public void replaceRemoveWithForLoopByRemoveAll(List<String> col, List<String> output) {
        // Keep this comment
        output.removeAll(col);
    }

    public void replaceRemoveWithForEachByRemoveAll(Collection<String> col, List<String> output) {
        // Keep this comment
        output.removeAll(col);
    }

    public void doNotRefactorForLoopAddMethodResult(List<String> output, List<String> col) {
        for (int i = 0; i < col.size(); i++) {
            output.add(doSomething(col.get(i)));
        }
    }

    public void doNotRefactorForEachAddMethodResult(List<String> output, List<String> col) {
        for (String s : col) {
            output.add(doSomething(s));
        }
    }

    private String doSomething(String s) {
        return null;
    }

    public class MySet extends AbstractSet<String> {
        public MySet(List<String> strings) {
            // Keep this comment
            addAll(strings);
        }

        @Override
        public Iterator<String> iterator() {
            return null;
        }

        @Override
        public int size() {
            return 0;
        }
    }

    public static Set<String> refactorCollectionWithNoTypeArgument(List<String> strings) {
        class MyHashSet extends HashSet<String> {
            private static final long serialVersionUID = 1L;
        }

        final MyHashSet set = new MyHashSet();
        // Keep this comment
        set.addAll(strings);
        return set;
    }
}
