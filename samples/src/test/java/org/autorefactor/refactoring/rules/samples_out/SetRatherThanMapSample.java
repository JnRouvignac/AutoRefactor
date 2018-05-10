/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
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

import java.util.Collection;
import java.util.Date;
import java.util.EventObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Set;
import java.util.TreeMap;

public class SetRatherThanMapSample {

    public SetRatherThanMapSample() {
        this(new TreeMap<String, String>());
    }

    public SetRatherThanMapSample(int i) {
        this(new HashMap<String, String>(i));
    }

    public SetRatherThanMapSample(Map<String, String> parameter) {
    }

    public void replaceMapInstanceCreation() {
        // Keep this comment
        boolean isInside = new java.util.HashSet<String>().isEmpty();
        boolean isInside2 = new java.util.TreeSet<String>().isEmpty();

        // Keep this comment too
        int size = new java.util.HashSet<String>(10).size();
        int size2 = new java.util.TreeSet<String>().size();

        // Keep this comment also
        new java.util.HashSet<String>(10).clear();
        new java.util.TreeSet<String>().clear();
    }

    public void replaceRawMap() {
        // Keep this comment
        boolean isInside = new java.util.HashSet().isEmpty();
        boolean isInside2 = new java.util.TreeSet().isEmpty();

        // Keep this comment too
        int size = new java.util.HashSet(10).size();
        int size2 = new java.util.TreeSet().size();
    }

    public void replaceFullyQualifiedMap() {
        // Keep this comment
        boolean isInside = new java.util.HashSet<Integer>().isEmpty();
        boolean isInside2 = new java.util.TreeSet<Integer>().isEmpty();

        // Keep this comment too
        int size = new java.util.HashSet(10).size();
        int size2 = new java.util.TreeSet().size();
    }

    public void replaceMapVariableDeclaration() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
    }

    public void replaceHashMapVariableUse() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        // Keep this comment too
        aggregate.add("foo");

        // Keep this comment
        java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        // Keep this comment too
        aggregate2.add("foo");

        // Keep this comment
        java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
        // Keep this comment too
        aggregate3.add("foo");
    }

    public boolean refactorHashMapMethod() {
        // Keep this comment
        return new java.util.HashSet<String>().contains("foo");
    }

    public boolean refactorTreeMapMethod() {
        // Keep this comment
        return new java.util.TreeSet<String>().contains("foo");
    }

    public void refactorVariableAndMethod() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        // Keep this comment too
        aggregate.contains("foo");

        // Keep this comment
        java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        // Keep this comment too
        aggregate2.contains("foo");

        // Keep this comment
        java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
        // Keep this comment too
        aggregate3.contains("foo");
    }

    public int replaceMapWithLoop(List<Date> dates) {
        // Keep this comment
        java.util.HashSet<Long> aggregate = new java.util.HashSet<Long>();
        for (Date date : dates) {
            aggregate.add(date.getTime());
        }

        // Keep this comment too
        java.util.TreeSet<Long> aggregate2 = new java.util.TreeSet<Long>();
        for (Date date : dates) {
            aggregate2.add(date.getTime());
        }

        // Keep this comment also
        java.util.Set<Long> aggregate3 = new java.util.TreeSet<Long>();
        for (Date date : dates) {
            aggregate3.add(date.getTime());
        }

        return aggregate.size() + aggregate2.size() + aggregate3.size();
    }

    public int doNotRemoveActiveCode(List<PrintCounter> referenceCounters,
            List<PrintCounter> counters) {
        long key = 0;

        HashMap<Long, String> aggregate = new HashMap<Long, String>();
        for (PrintCounter counter : referenceCounters) {
            aggregate.put(key++, "The counter is " + counter);
        }

        TreeMap<Long, String> aggregate2 = new TreeMap<Long, String>();
        for (PrintCounter counter : referenceCounters) {
            aggregate2.put(key++, "The counter is " + counter);
        }

        Map<Long, String> aggregate3 = new TreeMap<Long, String>();
        for (PrintCounter counter : referenceCounters) {
            aggregate3.put(key++, "The counter is " + counter);
        }

        return aggregate.size() + aggregate2.size() + aggregate3.size();
    }

    public class PrintCounter {
        private int count;

        public int getCount() {
            return count;
        }

        @Override
        public String toString() {
            count++;
            return "counting";
        }
    }

    public void replaceHashMapWithModifier() {
        // Keep this comment
        final java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        aggregate.add("foo");

        // Keep this comment too
        final java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        aggregate2.add("foo");

        // Keep this comment also
        final java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
        aggregate3.add("foo");
    }

    public void replaceHashMapWithParameter() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>(10);
        aggregate.add("foo");

        // Keep this comment too
        java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        aggregate2.add("foo");

        // Keep this comment also
        java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
        aggregate3.add("foo");
    }

    public int replaceReassignedHashMap() {
        // Keep this comment
        java.util.HashSet<String> aggregate1 = new java.util.HashSet<String>();
        aggregate1.add("foo");

        // Keep this comment too
        java.util.HashSet<String> aggregate2 = aggregate1;
        aggregate2.add("bar");

        // Keep this comment
        java.util.TreeSet<String> aggregate3 = new java.util.TreeSet<String>();
        aggregate3.add("foo");

        // Keep this comment too
        java.util.TreeSet<String> aggregate4 = aggregate3;
        aggregate4.add("bar");

        // Keep this comment
        java.util.Set<String> aggregate5 = new java.util.TreeSet<String>();
        aggregate3.add("foo");

        // Keep this comment too
        java.util.Set<String> aggregate6 = aggregate5;
        aggregate4.add("bar");

        return aggregate1.size() + aggregate2.size() + aggregate3.size() + aggregate4.size() + aggregate5.size();
    }

    public void doNotReplaceHashMapParameter(HashMap<String, String> aMap,
            TreeMap<String, String> aMap2,
            TreeMap<String, String> aMap3) {
        HashMap<String, String> map = aMap;
        map.put("foo", "bar");

        TreeMap<String, String> map2 = aMap2;
        map2.put("foo", "bar");

        Map<String, String> map3 = aMap3;
        map2.put("foo", "bar");
    }

    public void doNotReplaceHashMapPassedToAMethod() {
        String text = String.valueOf(new HashMap<String, String>());
        equals(new HashMap<String, String>());

        String text2 = String.valueOf(new TreeMap<String, String>());
        equals(new TreeMap<String, String>());
    }

    public Object doNotReplaceHashMapPassedToAConstructor() {
        return new EventObject(new HashMap<String, String>());
    }

    public Object doNotReplaceHashMapInConditionalExpression(boolean b) {
        return b ? new HashMap<String, String>() : null;
    }

    public HashMap<Integer, Date> doNotReplaceReturnedHashMap() {
        return new HashMap<Integer, Date>();
    }

    public void doNotReplaceReassignedVariable() {
        HashMap<String, String> map = new HashMap<String, String>();
        map = new HashMap<String, String>();

        TreeMap<String, String> map2 = new TreeMap<String, String>();
        map2 = new TreeMap<String, String>();

        Map<String, String> map3 = new TreeMap<String, String>();
        map3 = new TreeMap<String, String>();
    }

    public void replaceHashMapInAnnomymousClass() {
        final java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate.add("foo");
            }
        };

        final java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate2.add("foo");
            }
        };

        final java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate3.add("foo");
            }
        };
    }

    public void replaceMapInsideRunnable() {
        // Keep this comment
        final java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        aggregate.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final java.util.HashSet<String> localAggregate = new java.util.HashSet<String>();
                localAggregate.add("bar");
            }
        };

        // Keep this comment
        final java.util.TreeSet<String> aggregate2 = new java.util.TreeSet<String>();
        aggregate2.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final java.util.TreeSet<String> localAggregate = new java.util.TreeSet<String>();
                localAggregate.add("bar");
            }
        };

        // Keep this comment
        final java.util.Set<String> aggregate3 = new java.util.TreeSet<String>();
        aggregate3.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final java.util.Set<String> localAggregate = new java.util.TreeSet<String>();
                localAggregate.add("bar");
            }
        };
    }

    public void doNotReplaceSpecificInstantiation(HashMap<String, String> aMap) {
        HashMap<String, String> map = new HashMap<String, String>(aMap);
        map.put("foo", "bar");

        TreeMap<String, String> map2 = new TreeMap<String, String>(aMap);
        map.put("foo", "bar");

        Map<String, String> map3 = new TreeMap<String, String>(aMap);
        map.put("foo", "bar");
    }

    public Object doNotReplaceCloneMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.clone();
    }

    public boolean doNotReplaceContainsValueMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.containsValue("bar");
    }

    public Collection<String> doNotReplaceValuesMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.values();
    }

    public Set<Entry<String, String>> doNotReplaceEntrySetMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.entrySet();
    }

    public boolean doNotReplaceEqualsMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.equals(123);
    }

    public String doNotReplaceGetMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.get("foo");
    }

    public int doNotReplaceHashCodeMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.hashCode();
    }

    public String doNotReplaceToStringMethod() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.toString();
    }

    public String doNotReplaceRemoveHashMapMethodInReturn() {
        HashMap<String, String> map = new HashMap<String, String>();
        return map.remove("foo");
    }

    public String doNotReplaceRemoveTreeMapMethodInReturn() {
        TreeMap<String, String> map = new TreeMap<String, String>();
        return map.remove("foo");
    }

    public void refactorWithMethods(Collection<Integer> anotherCollection) throws InterruptedException {
        // Keep this comment
        java.util.HashSet<Integer> aggregate = new java.util.HashSet<Integer>();
        aggregate.add(123);
        aggregate.clear();
        aggregate.contains(anotherCollection);
        aggregate.isEmpty();
        aggregate.remove(456);
        aggregate.size();
        aggregate.notify();
        aggregate.notifyAll();
        aggregate.wait();
        aggregate.wait(1000);
        aggregate.wait(1000, 1000);

        // Keep this comment too
        java.util.TreeSet<Integer> aggregate2 = new java.util.TreeSet<Integer>();
        aggregate2.add(123);
        aggregate2.clear();
        aggregate2.contains(anotherCollection);
        aggregate2.isEmpty();
        aggregate2.remove(456);
        aggregate2.size();
        aggregate2.notify();
        aggregate2.notifyAll();
        aggregate2.wait();
        aggregate2.wait(1000);
        aggregate2.wait(1000, 1000);

        // Keep this comment also
        java.util.Set<Integer> aggregate3 = new java.util.TreeSet<Integer>();
        aggregate3.add(123);
        aggregate3.clear();
        aggregate3.contains(anotherCollection);
        aggregate3.isEmpty();
        aggregate3.remove(456);
        aggregate3.size();
        aggregate3.notify();
        aggregate3.notifyAll();
        aggregate3.wait();
        aggregate3.wait(1000);
        aggregate3.wait(1000, 1000);
    }
}
