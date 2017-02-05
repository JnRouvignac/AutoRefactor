/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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

import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Map.Entry;

public class CollectionSample {

    public void replaceNewNoArgsAssignmentThenAddAll(List<String> col, List<String> output) {
        // Keep this comment
        output = new ArrayList<String>();
        output.addAll(col);
    }

    public void doNotReplaceStackCtor(List<String> col, List<String> output) {
        output = new Stack<String>();
        output.addAll(col);
    }

    public List<String> replaceNewNoArgsThenAddAll(List<String> col) {
        // Keep this comment
        final List<String> output = new ArrayList<String>();
        output.addAll(col);
        return output;
    }

    public List<String> replaceNewOneArgThenAddAll(List<String> col) {
        // Keep this comment
        final List<String> output = new ArrayList<String>(0);
        output.addAll(col);
        return output;
    }

    public List<String> replaceNewCollectionSizeThenAddAll(List<String> col) {
        // Keep this comment
        final List<String> output = new ArrayList<String>(col.size());
        output.addAll(col);
        return output;
    }

    public List<Object> doNotReplaceNewThenAddAllIncompatibleTypes(List<String> col) {
        final List<Object> output = new ArrayList<>();
        output.addAll(col);
        return output;
    }

    public Object replaceNewThenAddAllParameterizedType(Map<String, String> map) {
        // Keep this comment
        List<Entry<String, String>> output = new ArrayList<Entry<String, String>>();
        output.addAll(map.entrySet());
        return output;
    }

    public void replaceAddWithForLoopByCollectionsAddAll(
            List<? super java.util.Date> output, java.util.Date[] elems1, java.sql.Date[] elems2) {
        // Keep this comment
        for (int i = 0; i < elems1.length; i++) {
            output.add(elems1[i]);
        }
        for (int i = 0; i < elems2.length; i++) {
            output.add(elems2[i]);
        }
    }

    public void replaceAddWithForEachByCollectionsAddAll(
            List<? super java.util.Date> output, java.util.Date[] elems1, java.sql.Date[] elems2) {
        // Keep this comment
        for (java.util.Date d : elems1) {
            output.add(d);
        }
        for (java.sql.Date d : elems2) {
            output.add(d);
        }
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
        for (int i = 0; i < col.size(); i++) {
            output.add(col.get(i));
        }
    }

    public void replaceAddWithForEachByAddAll(Collection<String> col, List<String> output) {
        // Keep this comment
        for (String s : col) {
            output.add(s);
        }
    }

    public void replaceContainsWithForLoopByContainsAll(List<String> col, List<String> output) {
        // Keep this comment
        for (int i = 0; i < col.size(); i++) {
            output.contains(col.get(i));
        }
    }

    public void replaceContainsWithForEachByContainsAll(Collection<String> col, List<String> output) {
        // Keep this comment
        for (String s : col) {
            output.contains(s);
        }
    }

    public void replaceRemoveWithForLoopByRemoveAll(List<String> col, List<String> output) {
        // Keep this comment
        for (int i = 0; i < col.size(); i++) {
            output.remove(col.get(i));
        }
    }

    public void replaceRemoveWithForEachByRemoveAll(Collection<String> col, List<String> output) {
        // Keep this comment
        for (String s : col) {
            output.remove(s);
        }
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

    public void replaceCheckOnSetNotContainsBeforeAdd(Set<String> col, String s) {
        // Keep this comment
        if (!col.contains(s)) {
            col.add(s);
            System.out.println("OK");
        } else {
            System.out.println("KO");
        }
    }

    public void replaceCheckOnSetContainsBeforeAdd(Set<String> col, String s) {
        // Keep this comment
        if (col.contains(s)) {
            System.out.println("KO");
        } else {
            col.add(s);
            System.out.println("OK");
        }
    }

    public void replaceCheckOnSetContainsOneAddStatement(Set<String> col, String s) {
        // Keep this comment
        if (col.contains(s)) {
        } else {
            col.add(s);
        }
    }

    public void replaceCheckOnSetNotContainsOneAddStatement(Set<String> col, String s) {
        // Keep this comment
        if (!col.contains(s)) {
            col.add(s);
        }
    }

    public void replaceCheckOnSetContainsBeforeRemove(Set<String> col, String s) {
        // Keep this comment
        if (!col.contains(s)) {
            System.out.println("KO");
        } else {
            col.remove(s);
            System.out.println("OK");
        }
    }

    public void replaceCheckOnSetNotContainsBeforeRemove(Set<String> col, String s) {
        // Keep this comment
        if (col.contains(s)) {
            col.remove(s);
            System.out.println("OK");
        } else {
            System.out.println("KO");
        }
    }

    public void replaceCheckOnSetContainsOneRemoveStatement(Set<String> col, String s) {
        // Keep this comment
        if (col.contains(s)) {
            col.remove(s);
        }
    }

    public void replaceCheckOnSetNotContainsOneRemoveStatement(Set<String> col, String s) {
        // Keep this comment
        if (!col.contains(s)) {
        } else {
            col.remove(s);
        }
    }

    public void doNotReplaceWhenCheckedValueIsDifferent(Set<String> col) {
        if (!col.contains("this")) {
            col.add("that");
            System.out.println("OK");
        }
        if (col.contains("this")) {
            col.remove("that");
            System.out.println("OK");
        }
    }

    public void doNotReplaceWhenCollectionsAreDifferent(Set<String> col1, Set<String> col2) {
        if (!col1.contains("that")) {
            col2.add("that");
            System.out.println("OK");
        }
        if (col1.contains("that")) {
            col2.remove("that");
            System.out.println("OK");
        }
    }

    public void doNotReplaceCheckOnListContainsBeforeAdd(List<String> col, String s) {
        if (!col.contains(s)) {
            col.add(s);
            System.out.println("OK");
        } else {
            System.out.println("KO");
        }
        if (!col.contains(s)) {
            System.out.println("KO");
        } else {
            col.remove(s);
            System.out.println("OK");
        }
    }

    public class MySet extends AbstractSet<String> {
        public MySet(List<String> strings) {
            // Keep this comment
            for (String s : strings) {
                add(s);
            }
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
        for (String s : strings) {
            set.add(s);
        }
        return set;
    }
}
