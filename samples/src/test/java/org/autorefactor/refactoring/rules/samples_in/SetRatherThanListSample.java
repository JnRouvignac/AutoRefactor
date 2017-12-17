/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Observable;
import java.util.Properties;

public class SetRatherThanListSample {

    public void replaceArrayListInstanceCreation() {
        new ArrayList<String>().contains("foo");
        new ArrayList<String>(new java.util.ArrayList<String>()).contains("bar");
    }

    public void replaceLinkedListInstanceCreation() {
        new LinkedList<String>().contains("foo");
        new LinkedList<String>(new java.util.LinkedList<String>()).contains("bar");
    }

    public void replaceOnlyWithContainsMethod() {
        new ArrayList<String>().contains("foo");
        new ArrayList<String>().isEmpty();
    }

    public void replaceOnlyWithContainsLinkedListMethod() {
        new LinkedList<String>().contains("foo");
        new LinkedList<String>().isEmpty();
    }

    public void replaceRawArrayList() {
        new ArrayList().contains("foo");
        new ArrayList(new java.util.ArrayList<String>()).contains("bar");
    }

    public void replaceRawLinkedList() {
        new LinkedList().contains("foo");
        new LinkedList(new java.util.LinkedList<String>()).contains("bar");
    }

    public void replaceFullyQualifiedArrayList() {
        new java.util.ArrayList<Date>().contains("foo");
        new java.util.ArrayList(10).contains("bar");
    }

    public void replaceFullyQualifiedLinkedList() {
        new java.util.LinkedList<Date>().contains("foo");
        new java.util.LinkedList().contains("bar");
    }

    public void doNotReplaceArrayListVariableDeclaration() {
        new ArrayList<String>();
    }

    public void doNotReplaceLinkedListVariableDeclaration() {
        new LinkedList<String>();
    }

    public void replaceArrayListVariableUse() {
        // Keep this comment
        ArrayList<String> collection = new ArrayList<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceLinkedListVariableUse() {
        // Keep this comment
        LinkedList<String> collection = new LinkedList<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceArrayListAndListInterface() {
        // Keep this comment
        List<String> collection = new ArrayList<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceLinkedListAndListInterface() {
        // Keep this comment
        List<String> collection = new LinkedList<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceArrayListAndCollectionInterface() {
        // Keep this comment
        Collection<String> collection = new ArrayList<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceLinkedListAndCollectionInterface() {
        // Keep this comment
        Collection<String> collection = new LinkedList<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public boolean refactorWithMethod() {
        // Keep this comment
        ArrayList<Observable> collection = new ArrayList<Observable>();
        // Keep this comment too
        collection.add(0, new Observable());
        return collection.contains(new Observable());
    }

    public boolean refactorWithLinkedListMethod() {
        // Keep this comment
        LinkedList<Observable> collection = new LinkedList<Observable>();
        // Keep this comment too
        collection.add(0, new Observable());
        return collection.contains(new Observable());
    }

    public boolean replaceArrayListWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Date> collection = new ArrayList<Date>();
        for (Date date : dates) {
            collection.add(date);
        }

        return collection.contains("foo");
    }

    public boolean replaceLinkedListWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Date> collection = new LinkedList<Date>();
        for (Date date : dates) {
            collection.add(date);
        }

        return collection.contains("foo");
    }

    public boolean replaceArrayListWithModifier() {
        // Keep this comment
        final ArrayList<byte[]> collection = new ArrayList<byte[]>();
        collection.add(new byte[] {1});
        return collection.contains(new byte[] {2});
    }

    public boolean replaceLinkedListWithModifier() {
        // Keep this comment
        final LinkedList<byte[]> collection = new LinkedList<byte[]>();
        collection.add(new byte[] {1});
        return collection.contains(new byte[] {2});
    }

    public boolean replaceArrayListWithParameter() {
        // Keep this comment
        ArrayList<Integer> collection = new ArrayList<Integer>(new java.util.ArrayList<Integer>());
        collection.add(1);
        return collection.contains(2);
    }

    public boolean replaceLinkedListWithParameter() {
        // Keep this comment
        LinkedList<Integer> collection = new LinkedList<Integer>(new java.util.ArrayList<Integer>());
        collection.add(1);
        return collection.contains(2);
    }

    public boolean replaceReassignedArrayList() {
        // Keep this comment
        ArrayList<String> collection1 = new ArrayList<String>();
        collection1.add("FOO");

        // Keep this comment too
        Collection<String> collection2 = collection1;
        collection2.add("BAR");

        return collection2.contains("foo");
    }

    public boolean replaceReassignedLinkedList() {
        // Keep this comment
        LinkedList<String> collection1 = new LinkedList<String>();
        collection1.add("FOO");

        // Keep this comment too
        Collection<String> collection2 = collection1;
        collection2.add("BAR");

        return collection2.contains("foo");
    }

    public void doNotReplaceArrayListWithImplicitItertor() {
        ArrayList<Properties> iterableList = new ArrayList<Properties>();
        for (Properties properties : iterableList) {
            System.out.println("The properties: " + properties);
        }
    }

    public boolean doNotReplaceArrayListParameter(ArrayList<String> aArrayList) {
        ArrayList<String> list = aArrayList;
        list.add("bar");
        return list.contains("foo");
    }

    public void doNotReplaceArrayListPassedToAMethod() {
        String.valueOf(new ArrayList<String>());
    }

    public ArrayList<Date> doNotReplaceReturnedArrayList() {
        return new ArrayList<Date>();
    }

    public void doNotReplaceReassignedVariable() {
        new ArrayList<String>();
        new ArrayList<String>();
    }

    public void doNotReplaceEnsureCapacity(int index) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        list.ensureCapacity(index);
    }

    public String doNotReplaceGet(int index) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.get(index);
    }

    public int doNotReplaceIndexOf(Object o) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.indexOf(o);
    }

    public Iterator<String> doNotReplaceIterator() {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.iterator();
    }

    public int doNotReplaceLastIndexOf(Object o) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.lastIndexOf(o);
    }

    public ListIterator<String> doNotReplaceListIterator() {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.listIterator();
    }

    public ListIterator<String> doNotReplaceListIterator(int index) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.listIterator(index);
    }

    public String doNotReplaceRemove(int index) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.remove(index);
    }

    public boolean doNotReplaceRemove(Object o) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.remove(o);
    }

    public boolean doNotReplaceRemoveAll(Collection<?> c) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.removeAll(c);
    }

//    public boolean doNotReplaceRemoveIf(Predicate<String> filter) {
//        ArrayList<String> list = new ArrayList<String>();
//        list.contains("bar");
//        return list.removeIf(filter);
//    }
//
//    public void doNotReplaceReplaceAll(UnaryOperator<String> operator) {
//        ArrayList<String> list = new ArrayList<String>();
//        list.contains("bar");
//        list.replaceAll(operator);
//    }

    public boolean doNotReplaceRetainAll(Collection<?> c) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.retainAll(c);
    }

    public String doNotReplaceSet(int index, String element) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.set(index, element);
    }

    public int doNotReplaceSize() {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.size();
    }

//    public void doNotReplaceSort(Comparator<String> comparator) {
//        ArrayList<String> list = new ArrayList<String>();
//        list.contains("bar");
//        list.sort(comparator);
//    }
//
//    public Spliterator<String> doNotReplaceSpliterator() {
//        ArrayList<String> list = new ArrayList<String>();
//        list.contains("bar");
//        return list.spliterator();
//    }

    public List<String> doNotReplaceSubList(int fromIndex, int toIndex) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.subList(fromIndex, toIndex);
    }

    public Object[] doNotReplaceToArray() {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.toArray();
    }

    public String[] doNotReplaceToArray(String[] a) {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        return list.toArray(a);
    }

    public void doNotReplaceTrimToSize() {
        ArrayList<String> list = new ArrayList<String>();
        list.contains("bar");
        list.trimToSize();
    }

    public boolean refactorMethods(Collection collection) {
        // Keep this comment
        ArrayList<String> list = new ArrayList<String>();
        list.add(0, "bar");
        list.addAll(1, collection);
        return list.contains("foo");
    }

    public boolean refactorLinkedListMethods(Collection collection) {
        // Keep this comment
        LinkedList<String> list = new LinkedList<String>();
        list.add(0, "bar");
        list.addAll(1, collection);
        return list.contains("foo");
    }

    public void replaceListInRunnable() {
        // Keep this comment
        final ArrayList<String> list = new ArrayList<String>();
        new Runnable() {

            @Override
            public void run() {
                list.add("foo");
                list.contains("bar");
            }
        };
    }

    public void replaceLinkedListInRunnable() {
        // Keep this comment
        final LinkedList<String> list = new LinkedList<String>();
        new Runnable() {

            @Override
            public void run() {
                list.add("foo");
                list.contains("bar");
            }
        };
    }

    public void replaceListInsideRunnable() {
        // Keep this comment
        final List<String> list = new ArrayList<String>();
        list.contains("bar");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final ArrayList<String> localList = new ArrayList<String>();
                localList.add("foo");
                localList.contains("bar");
            }
        };
    }

    public void replaceLinkedListInsideRunnable() {
        // Keep this comment
        final List<String> list = new LinkedList<String>();
        list.contains("bar");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final LinkedList<String> localList = new LinkedList<String>();
                localList.add("foo");
                localList.contains("bar");
            }
        };
    }
}
