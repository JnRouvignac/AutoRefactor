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

import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.NavigableSet;
import java.util.Properties;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

public class HashSetRatherThanTreeSetSample {

    public void replaceTreeSetInstanceCreation() {
        // Keep this comment
        boolean isFooContained = new TreeSet<String>().contains("foo");
        // Keep this comment too
        int size = new TreeSet<String>(new java.util.HashSet<String>()).size();
    }

    public void replaceRawTreeSet() {
        // Keep this comment
        boolean isFooContained = new TreeSet().contains("foo");
        // Keep this comment too
        int size = new TreeSet(new java.util.HashSet<String>()).size();
    }

    public void replaceFullyQualifiedTreeSet() {
        // Keep this comment
        boolean isFooContained = new java.util.TreeSet<Integer>().contains("foo");
        // Keep this comment too
        int size = new java.util.TreeSet(new java.util.HashSet<String>()).size();
    }

    public void replaceTreeSetVariableDeclaration() {
        // Keep this comment
        TreeSet<String> set = new TreeSet<String>();
    }

    public void replaceSetVariableDeclaration() {
        // Keep this comment
        Set<String> set = new TreeSet<String>();
    }

    public void replaceCollectionVariableDeclaration() {
        // Keep this comment
        Collection<String> set = new TreeSet<String>();
    }

    public void replaceTreeSetVariableUse() {
        // Keep this comment
        TreeSet<String> set = new TreeSet<String>();
        // Keep this comment too
        set.add("foo");
    }

    public void replaceSetVariableUse() {
        // Keep this comment
        Set<String> set = new TreeSet<String>();
        // Keep this comment too
        set.add("foo");
    }

    public void replaceCollectionVariableUse() {
        // Keep this comment
        Collection<String> set = new TreeSet<String>();
        // Keep this comment too
        set.add("foo");
    }

    public boolean replaceTreeSetWithLoop(List<Date> dates) {
        // Keep this comment
        TreeSet<Long> set = new TreeSet<Long>();
        for (Date date : dates) {
            set.add(date.getTime());
        }

        return set.remove(789L);
    }

    public boolean replaceSetWithLoop(List<Date> dates) {
        // Keep this comment
        Set<Long> set = new TreeSet<Long>();
        for (Date date : dates) {
            set.add(date.getTime());
        }

        return set.remove(789L);
    }

    public boolean replaceCollectionWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Long> set = new TreeSet<Long>();
        for (Date date : dates) {
            set.add(date.getTime());
        }

        return set.remove(789L);
    }

    public boolean replaceTreeSetWithModifier() {
        // Keep this comment
        final TreeSet<String> set = new TreeSet<String>();
        return set.add("foo");
    }

    public boolean replaceSetWithModifier() {
        // Keep this comment
        final Set<String> set = new TreeSet<String>();
        return set.add("foo");
    }

    public boolean replaceCollectionWithModifier() {
        // Keep this comment
        final Collection<String> set = new TreeSet<String>();
        return set.add("foo");
    }

    public void replaceTreeSetWithParameter() {
        // Keep this comment
        TreeSet<char[]> set = new TreeSet<char[]>(new java.util.HashSet<char[]>());
        set.add(new char[] {'a'});
    }

    public void replaceSetWithParameter() {
        // Keep this comment
        Set<char[]> set = new TreeSet<char[]>(new java.util.HashSet<char[]>());
        set.add(new char[] {'a'});
    }

    public void replaceCollectionWithParameter() {
        // Keep this comment
        Collection<char[]> set = new TreeSet<char[]>(new java.util.HashSet<char[]>());
        set.add(new char[] {'a'});
    }

    public void doNotReplaceTreeSetWithComparator(Comparator<Long> comparator) {
        TreeSet<Long> set = new TreeSet<Long>(comparator);
        set.add(123L);
    }

    public boolean replaceReassignedTreeSet() {
        // Keep this comment
        TreeSet<String> set1 = new TreeSet<String>();
        set1.add("foo");

        // Keep this comment too
        TreeSet<String> set2 = set1;
        set2.add("bar");

        return set2.isEmpty();
    }

    public boolean replaceReassignedSet() {
        // Keep this comment
        Set<String> set1 = new TreeSet<String>();
        set1.add("foo");

        // Keep this comment too
        Set<String> set2 = set1;
        set2.add("bar");

        return set2.isEmpty();
    }

    public boolean replaceReassignedCollection() {
        // Keep this comment
        Collection<String> set1 = new TreeSet<String>();
        set1.add("foo");

        // Keep this comment too
        Collection<String> set2 = set1;
        set2.add("bar");

        return set2.isEmpty();
    }

    public void doNotReplaceArrayListWithImplicitItertor() {
        TreeSet<Properties> iterableList = new TreeSet<Properties>();
        for (Properties properties : iterableList) {
            System.out.println("The properties: " + properties);
        }
    }

    public void doNotReplaceTreeSetParameter(TreeSet<String> aSet) {
        TreeSet<String> set = aSet;
        set.add("foo");
    }

    public void doNotReplaceTreeSetPassedToAMethod() {
        String text = String.valueOf(new TreeSet<String>());
    }

    public TreeSet<Integer> doNotReplaceReturnedTreeSet() {
        return new TreeSet<Integer>();
    }

    public void doNotReplaceReassignedVariable() {
        TreeSet<String> set = new TreeSet<String>();
        set = new TreeSet<String>();
    }

    public String doNotReplaceCeiling(String e) {
        TreeSet<String> set = new TreeSet<String>();
        return set.ceiling(e);
    }

    public Object doNotReplaceClone() {
        TreeSet<String> set = new TreeSet<String>();
        return set.clone();
    }

    public Comparator doNotReplaceComparator() {
        TreeSet<String> set = new TreeSet<String>();
        return set.comparator();
    }

    public Iterator<String> doNotReplaceDescendingIterator() {
        TreeSet<String> set = new TreeSet<String>();
        return set.descendingIterator();
    }

    public NavigableSet<String> doNotReplaceDescendingSet() {
        TreeSet<String> set = new TreeSet<String>();
        return set.descendingSet();
    }

    public String doNotReplaceFirst() {
        TreeSet<String> set = new TreeSet<String>();
        return set.first();
    }

    public String doNotReplaceFloor(String e) {
        TreeSet<String> set = new TreeSet<String>();
        return set.floor(e);
    }

    public SortedSet<String> doNotReplaceHeadSet(String toElement) {
        TreeSet<String> set = new TreeSet<String>();
        return set.headSet(toElement);
    }

    public NavigableSet<String> doNotReplaceHeadSet(String toElement, boolean inclusive) {
        TreeSet<String> set = new TreeSet<String>();
        return set.headSet(toElement, inclusive);
    }

    public String doNotReplaceHigher(String e) {
        TreeSet<String> set = new TreeSet<String>();
        return set.higher(e);
    }

    public Iterator<String> doNotReplaceIterator() {
        TreeSet<String> set = new TreeSet<String>();
        return set.iterator();
    }

    public String doNotReplaceLower(String e) {
        TreeSet<String> set = new TreeSet<String>();
        return set.lower(e);
    }

    public String doNotReplacePollFirst() {
        TreeSet<String> set = new TreeSet<String>();
        return set.pollFirst();
    }

    public String doNotReplacePollLast() {
        TreeSet<String> set = new TreeSet<String>();
        return set.pollLast();
    }

    public NavigableSet<String> doNotReplaceSubSet(String fromElement, boolean fromInclusive, String toElement,
            boolean toInclusive) {
        TreeSet<String> set = new TreeSet<String>();
        return set.subSet(fromElement, fromInclusive, toElement, toInclusive);
    }

    public SortedSet<String> doNotReplaceSubSet(String fromElement, String toElement) {
        TreeSet<String> set = new TreeSet<String>();
        return set.subSet(fromElement, toElement);
    }

    public SortedSet<String> doNotReplaceTailSet(String fromElement) {
        TreeSet<String> set = new TreeSet<String>();
        return set.tailSet(fromElement);
    }

    public NavigableSet<String> doNotReplaceTailSet(String fromElement, boolean inclusive) {
        TreeSet<String> set = new TreeSet<String>();
        return set.tailSet(fromElement, inclusive);
    }

    public Object[] doNotReplaceToArray() {
        TreeSet<String> set = new TreeSet<String>();
        return set.toArray();
    }

    public String[] doNotReplaceToArray(String[] a) {
        TreeSet<String> set = new TreeSet<String>();
        return set.toArray(a);
    }

    public void refactorWithTreeSetMethods(Collection<Integer> anotherCollection) throws InterruptedException {
        // Keep this comment
        TreeSet<Integer> set = new TreeSet<Integer>();
        set.add(123);
        set.clear();
        set.contains(anotherCollection);
        set.isEmpty();
        set.remove(123);
        set.size();
        set.removeAll(anotherCollection);
        set.addAll(anotherCollection);
        set.containsAll(anotherCollection);
        set.retainAll(anotherCollection);
        set.notify();
        set.notifyAll();
        set.wait();
        set.wait(1000);
        set.wait(1000, 1000);
    }

    public void refactorWithSetMethods(Collection<Integer> anotherCollection) throws InterruptedException {
        // Keep this comment
        Set<Integer> set = new TreeSet<Integer>();
        set.add(123);
        set.clear();
        set.contains(anotherCollection);
        set.isEmpty();
        set.remove(123);
        set.size();
        set.removeAll(anotherCollection);
        set.addAll(anotherCollection);
        set.containsAll(anotherCollection);
        set.retainAll(anotherCollection);
        set.notify();
        set.notifyAll();
        set.wait();
        set.wait(1000);
        set.wait(1000, 1000);
    }

    public void refactorWithCollectionMethods(Collection<Integer> anotherCollection) throws InterruptedException {
        // Keep this comment
        Collection<Integer> collection = new TreeSet<Integer>();
        collection.add(123);
        collection.clear();
        collection.contains(anotherCollection);
        collection.isEmpty();
        collection.remove(123);
        collection.size();
        collection.removeAll(anotherCollection);
        collection.addAll(anotherCollection);
        collection.containsAll(anotherCollection);
        collection.retainAll(anotherCollection);
        collection.notify();
        collection.notifyAll();
        collection.wait();
        collection.wait(1000);
        collection.wait(1000, 1000);
    }

    public void replaceTreeSetThroughRunnable() {
        // Keep this comment
        final TreeSet<String> set = new TreeSet<String>();
        new Runnable() {

            @Override
            public void run() {
                set.add("foo");
            }
        };
    }

    public void replaceSetThroughRunnable() {
        // Keep this comment
        final Set<String> set = new TreeSet<String>();
        new Runnable() {

            @Override
            public void run() {
                set.add("foo");
            }
        };
    }

    public void replaceCollectionThroughRunnable() {
        // Keep this comment
        final Collection<String> set = new TreeSet<String>();
        new Runnable() {

            @Override
            public void run() {
                set.add("foo");
            }
        };
    }

    public void replaceTreeSetInsideRunnable() {
        // Keep this comment
        final TreeSet<String> set = new TreeSet<String>();
        set.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final TreeSet<String> localSet = new TreeSet<String>();
                localSet.add("foo");
            }
        };
    }

    public void replaceSetInsideRunnable() {
        // Keep this comment
        final Set<String> set = new TreeSet<String>();
        set.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final Set<String> localSet = new TreeSet<String>();
                localSet.add("foo");
            }
        };
    }

    public void replaceCollectionInsideRunnable() {
        // Keep this comment
        final Collection<String> set = new TreeSet<String>();
        set.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final Collection<String> localSet = new TreeSet<String>();
                localSet.add("foo");
            }
        };
    }
}
