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
package org.autorefactor.refactoring.rules.samples_out;

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
        new java.util.HashSet<String>().contains("foo");
        new java.util.HashSet<String>(new java.util.ArrayList<String>()).contains("bar");
    }

    public void replaceLinkedListInstanceCreation() {
        new java.util.HashSet<String>().contains("foo");
        new java.util.HashSet<String>(new java.util.LinkedList<String>()).contains("bar");
    }

    public void replaceOnlyWithContainsMethod() {
        new java.util.HashSet<String>().contains("foo");
        new ArrayList<String>().isEmpty();
    }

    public void replaceOnlyWithContainsLinkedListMethod() {
        new java.util.HashSet<String>().contains("foo");
        new LinkedList<String>().isEmpty();
    }

    public void replaceRawArrayList() {
        new java.util.HashSet().contains("foo");
        new java.util.HashSet(new java.util.ArrayList<String>()).contains("bar");
    }

    public void replaceRawLinkedList() {
        new java.util.HashSet().contains("foo");
        new java.util.HashSet(new java.util.LinkedList<String>()).contains("bar");
    }

    public void replaceFullyQualifiedArrayList() {
        new java.util.HashSet<Date>().contains("foo");
        new java.util.HashSet(10).contains("bar");
    }

    public void replaceFullyQualifiedLinkedList() {
        new java.util.HashSet<Date>().contains("foo");
        new java.util.HashSet().contains("bar");
    }

    public void doNotReplaceArrayListVariableDeclaration() {
        new ArrayList<String>();
    }

    public void doNotReplaceLinkedListVariableDeclaration() {
        new LinkedList<String>();
    }

    public void replaceArrayListVariableUse() {
        // Keep this comment
        java.util.HashSet<String> collection = new java.util.HashSet<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceLinkedListVariableUse() {
        // Keep this comment
        java.util.HashSet<String> collection = new java.util.HashSet<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceArrayListAndListInterface() {
        // Keep this comment
        java.util.Set<String> collection = new java.util.HashSet<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceLinkedListAndListInterface() {
        // Keep this comment
        java.util.Set<String> collection = new java.util.HashSet<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceArrayListAndCollectionInterface() {
        // Keep this comment
        Collection<String> collection = new java.util.HashSet<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public void replaceLinkedListAndCollectionInterface() {
        // Keep this comment
        Collection<String> collection = new java.util.HashSet<String>();
        // Keep this comment too
        collection.contains("foo");
    }

    public boolean refactorWithMethod() {
        // Keep this comment
        java.util.HashSet<Observable> collection = new java.util.HashSet<Observable>();
        // Keep this comment too
        collection.add(new Observable());
        return collection.contains(new Observable());
    }

    public boolean refactorWithLinkedListMethod() {
        // Keep this comment
        java.util.HashSet<Observable> collection = new java.util.HashSet<Observable>();
        // Keep this comment too
        collection.add(new Observable());
        return collection.contains(new Observable());
    }

    public boolean replaceArrayListWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Date> collection = new java.util.HashSet<Date>();
        for (Date date : dates) {
            collection.add(date);
        }

        return collection.contains("foo");
    }

    public boolean replaceLinkedListWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Date> collection = new java.util.HashSet<Date>();
        for (Date date : dates) {
            collection.add(date);
        }

        return collection.contains("foo");
    }

    public boolean replaceArrayListWithModifier() {
        // Keep this comment
        final java.util.HashSet<byte[]> collection = new java.util.HashSet<byte[]>();
        collection.add(new byte[] {1});
        return collection.contains(new byte[] {2});
    }

    public boolean replaceLinkedListWithModifier() {
        // Keep this comment
        final java.util.HashSet<byte[]> collection = new java.util.HashSet<byte[]>();
        collection.add(new byte[] {1});
        return collection.contains(new byte[] {2});
    }

    public boolean replaceArrayListWithParameter() {
        // Keep this comment
        java.util.HashSet<Integer> collection = new java.util.HashSet<Integer>(new java.util.ArrayList<Integer>());
        collection.add(1);
        return collection.contains(2);
    }

    public boolean replaceLinkedListWithParameter() {
        // Keep this comment
        java.util.HashSet<Integer> collection = new java.util.HashSet<Integer>(new java.util.ArrayList<Integer>());
        collection.add(1);
        return collection.contains(2);
    }

    public boolean replaceReassignedArrayList() {
        // Keep this comment
        java.util.HashSet<String> collection1 = new java.util.HashSet<String>();
        collection1.add("FOO");

        // Keep this comment too
        Collection<String> collection2 = collection1;
        collection2.add("BAR");

        return collection2.contains("foo");
    }

    public boolean replaceReassignedLinkedList() {
        // Keep this comment
        java.util.HashSet<String> collection1 = new java.util.HashSet<String>();
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
        java.util.HashSet<String> list = new java.util.HashSet<String>();
        list.add("bar");
        list.addAll(collection);
        return list.contains("foo");
    }

    public boolean refactorLinkedListMethods(Collection collection) {
        // Keep this comment
        java.util.HashSet<String> list = new java.util.HashSet<String>();
        list.add("bar");
        list.addAll(collection);
        return list.contains("foo");
    }

    public void replaceListInRunnable() {
        // Keep this comment
        final java.util.HashSet<String> list = new java.util.HashSet<String>();
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
        final java.util.HashSet<String> list = new java.util.HashSet<String>();
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
        final java.util.Set<String> list = new java.util.HashSet<String>();
        list.contains("bar");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final java.util.HashSet<String> localList = new java.util.HashSet<String>();
                localList.add("foo");
                localList.contains("bar");
            }
        };
    }

    public void replaceLinkedListInsideRunnable() {
        // Keep this comment
        final java.util.Set<String> list = new java.util.HashSet<String>();
        list.contains("bar");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final java.util.HashSet<String> localList = new java.util.HashSet<String>();
                localList.add("foo");
                localList.contains("bar");
            }
        };
    }
}
