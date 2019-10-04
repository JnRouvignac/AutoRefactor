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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class AddAllRatherThanLoopSample extends ArrayList<java.util.Date> {
    private java.util.Date[] innerArray = new java.util.Date[10];

    private List<java.util.Date> innerList = new ArrayList<>();

    public Collection<? super java.util.Date> replaceAddWithForLoopByCollectionsAddAll(
            List<? super java.util.Date> output, java.util.Date[] elems1, java.sql.Date[] elems2) {
        // Keep this comment
        Collections.addAll(output, elems1);
        Collections.addAll(output, elems2);

        return output;
    }

    public void replaceAddWithForLoopByCollectionsAddAll(
            java.util.Date[] dates) {
        // Keep this comment
        Collections.addAll(this, dates);
    }

    public void replaceLoopWithFieldArray() {
        // Keep this comment
        Collections.addAll(this, innerArray);
    }

    public void replaceForeachWithFieldArray() {
        // Keep this comment
        Collections.addAll(this, this.innerArray);
    }

    public void replaceLoopWithFieldList() {
        // Keep this comment
        addAll(innerList);
    }

    public void replaceForeachWithFieldList() {
        // Keep this comment
        addAll(innerList);
    }

    public Collection replaceAddWithForEachByCollectionsAddAll(
            List<? super java.util.Date> output, java.util.Date[] elems1, java.sql.Date[] elems2) {
        // Keep this comment
        Collections.addAll(output, elems1);
        Collections.addAll(output, elems2);

        return output;
    }

    public void replaceAddWithForEachByCollectionsAddAll(
            java.util.Date[] dates) {
        // Keep this comment
        Collections.addAll(this, dates);
    }

    public Map<String, List<String>> replaceLoopOnCollectionAsExpressionWithArray(
            Map<String, List<String>> mapToFill, String[] inputList) {
        // Keep this comment
        Collections.addAll(mapToFill.get("foo"), inputList);

        return mapToFill;
    }

    public Collection replaceLoopOnRawCollectionWithArray(
            List colToFill, String[] inputList) {
        // Keep this comment
        Collections.addAll(colToFill, inputList);

        return colToFill;
    }

    public Map<String, List<String>> replaceLoopOnCollectionAsExpressionWithList(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        // Keep this comment
        mapToFill.get("foo").addAll(inputList);

        return mapToFill;
    }

    public Collection replaceLoopOnRawCollectionWithList(
            List colToFill, List<String> inputList) {
        // Keep this comment
        colToFill.addAll(inputList);

        return colToFill;
    }

    public void doNotReplaceLoopWithFieldList(List<java.util.Date> input) {
        for (int i = 0; i < input.size(); i++) {
            add(innerList.get(i));
        }
    }

    public Map<String, List<String>> doNotRefactorForEachWithListUsingLoopVariable(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        for (String input : inputList) {
            mapToFill.get(input).add(input);
        }

        return mapToFill;
    }

    public Map<String, List<String>> doNotRefactorForLoopWithListUsingLoopIndex(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        for (int i = 0; i < inputList.size(); i++) {
            mapToFill.get(inputList.get(i)).add(inputList.get(i));
        }

        return mapToFill;
    }

    public Map<String, List<String>> doNotRefactorForLoopWithListUsingLoopIterator(
            Map<String, List<String>> mapToFill, List<String> inputList) {
        String input = null;
        for (Iterator<String> it = inputList.iterator(); it.hasNext(); input = it.next()) {
            mapToFill.get(input).add(input);
        }

        return mapToFill;
    }

    public void doNotRefactorForLoopWithListUsingLoopIterator(List<String> col) {
        for (Iterator<String> it = col.iterator(); it.hasNext();) {
            System.out.println(it.next());
        }
    }

    public Map<String, List<String>> doNotRefactorForEachWithArrayUsingLoopVariable(
            Map<String, List<String>> mapToFill, String[] inputArray) {
        for (String input : inputArray) {
            mapToFill.get(input).add(input);
        }

        return mapToFill;
    }

    public Map<String, List<String>> doNotRefactorForLoopWithArrayUsingLoopIndex(
            Map<String, List<String>> mapToFill, String[] inputArray) {
        for (int i = 0; i < inputArray.length; i++) {
            mapToFill.get(inputArray[i]).add(inputArray[i]);
        }

        return mapToFill;
    }

    public Collection<String> doNotRefactorForLoopAddMethodResult(List<String> output, String[] elems) {
        for (int i = 0; i < elems.length; i++) {
            output.add(doSomething(elems[i]));
        }

        return output;
    }

    public Collection<String> doNotRefactorForEachAddMethodResult(List<String> output, String[] elems) {
        for (String s : elems) {
            output.add(doSomething(s));
        }

        return output;
    }

    public Collection<String> replaceAddWithForLoopByAddAll(List<String> col, List<String> output) {
        // Keep this comment
        output.addAll(col);

        return output;
    }

    public Collection<String> replaceAddWithForEachByAddAll(Collection<String> col, List<String> output) {
        // Keep this comment
        output.addAll(col);

        return output;
    }

    public Collection<String> doNotRefactorRemoveWithPossibleDoubles() {
        List<String> col = new ArrayList<>();
        col.add("redundant");
        List<String> output = new ArrayList<>();
        output.add("redundant");
        output.add("redundant");

        for (int i = 0; i < col.size(); i++) {
            output.remove(col.get(i));
        }

        return output;
    }

    public Collection<String> replaceRemoveWithForLoopByRemoveAll(List<String> col, Set<String> output) {
        // Keep this comment
        output.removeAll(col);

        return output;
    }

    public Collection<String> replaceRemoveWithForEachByRemoveAll(Collection<String> col, Set<String> output) {
        // Keep this comment
        output.removeAll(col);

        return output;
    }

    public Collection<String> doNotRefactorForLoopAddMethodResult(List<String> output, List<String> col) {
        for (int i = 0; i < col.size(); i++) {
            output.add(doSomething(col.get(i)));
        }

        return output;
    }

    public Collection<String> doNotRefactorForEachAddMethodResult(List<String> output, List<String> col) {
        for (String s : col) {
            output.add(doSomething(s));
        }

        return output;
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
