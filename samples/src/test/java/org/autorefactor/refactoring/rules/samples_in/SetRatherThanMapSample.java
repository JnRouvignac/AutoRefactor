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
package org.autorefactor.refactoring.rules.samples_in;

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
        boolean isInside = new HashMap<String, String>().isEmpty();
        boolean isInside2 = new TreeMap<String, String>().isEmpty();

        // Keep this comment too
        int size = new HashMap<String, String>(10).size();
        int size2 = new TreeMap<String, String>().size();

        // Keep this comment also
        new HashMap<String, String>(10).clear();
        new TreeMap<String, String>().clear();
    }

    public void replaceRawMap() {
        // Keep this comment
        boolean isInside = new HashMap().isEmpty();
        boolean isInside2 = new TreeMap().isEmpty();

        // Keep this comment too
        int size = new HashMap(10).size();
        int size2 = new TreeMap().size();
    }

    public void replaceFullyQualifiedMap() {
        // Keep this comment
        boolean isInside = new java.util.HashMap<Integer, Date>().isEmpty();
        boolean isInside2 = new java.util.TreeMap<Integer, Date>().isEmpty();

        // Keep this comment too
        int size = new java.util.HashMap(10).size();
        int size2 = new java.util.TreeMap().size();
    }

    public void replaceMapVariableDeclaration() {
        // Keep this comment
        HashMap<String, Observable> aggregate = new HashMap<String, Observable>();
        TreeMap<String, Observable> aggregate2 = new TreeMap<String, Observable>();
        Map<String, Observable> aggregate3 = new TreeMap<String, Observable>();
    }

    public void replaceHashMapVariableUse() {
        // Keep this comment
        HashMap<String, String> aggregate = new HashMap<String, String>();
        // Keep this comment too
        aggregate.put("foo", "bar");

        // Keep this comment
        TreeMap<String, String> aggregate2 = new TreeMap<String, String>();
        // Keep this comment too
        aggregate2.put("foo", "bar");

        // Keep this comment
        Map<String, String> aggregate3 = new TreeMap<String, String>();
        // Keep this comment too
        aggregate3.put("foo", "bar");
    }

    public boolean refactorHashMapMethod() {
        // Keep this comment
        return new HashMap<String, String>().containsKey("foo");
    }

    public boolean refactorTreeMapMethod() {
        // Keep this comment
        return new TreeMap<String, String>().containsKey("foo");
    }

    public void refactorVariableAndMethod() {
        // Keep this comment
        HashMap<String, String> aggregate = new HashMap<String, String>();
        // Keep this comment too
        aggregate.containsKey("foo");

        // Keep this comment
        TreeMap<String, String> aggregate2 = new TreeMap<String, String>();
        // Keep this comment too
        aggregate2.containsKey("foo");

        // Keep this comment
        Map<String, String> aggregate3 = new TreeMap<String, String>();
        // Keep this comment too
        aggregate3.containsKey("foo");
    }

    public int replaceMapWithLoop(List<Date> dates) {
        // Keep this comment
        HashMap<Long, Date> aggregate = new HashMap<Long, Date>();
        for (Date date : dates) {
            aggregate.put(date.getTime(), date);
        }

        // Keep this comment too
        TreeMap<Long, Date> aggregate2 = new TreeMap<Long, Date>();
        for (Date date : dates) {
            aggregate2.put(date.getTime(), date);
        }

        // Keep this comment also
        Map<Long, Date> aggregate3 = new TreeMap<Long, Date>();
        for (Date date : dates) {
            aggregate3.put(date.getTime(), date);
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
        final HashMap<String, String> aggregate = new HashMap<String, String>();
        aggregate.put("foo", "bar");

        // Keep this comment too
        final TreeMap<String, String> aggregate2 = new TreeMap<String, String>();
        aggregate2.put("foo", "bar");

        // Keep this comment also
        final Map<String, String> aggregate3 = new TreeMap<String, String>();
        aggregate3.put("foo", "bar");
    }

    public void replaceHashMapWithParameter() {
        // Keep this comment
        HashMap<String, String> aggregate = new HashMap<String, String>(10);
        aggregate.put("foo", "bar");

        // Keep this comment too
        TreeMap<String, String> aggregate2 = new TreeMap<String, String>();
        aggregate2.put("foo", "bar");

        // Keep this comment also
        Map<String, String> aggregate3 = new TreeMap<String, String>();
        aggregate3.put("foo", "bar");
    }

    public int replaceReassignedHashMap() {
        // Keep this comment
        HashMap<String, String> aggregate1 = new HashMap<String, String>();
        aggregate1.put("foo", "FOO");

        // Keep this comment too
        HashMap<String, String> aggregate2 = aggregate1;
        aggregate2.put("bar", "BAR");

        // Keep this comment
        TreeMap<String, String> aggregate3 = new TreeMap<String, String>();
        aggregate3.put("foo", "FOO");

        // Keep this comment too
        TreeMap<String, String> aggregate4 = aggregate3;
        aggregate4.put("bar", "BAR");

        // Keep this comment
        Map<String, String> aggregate5 = new TreeMap<String, String>();
        aggregate3.put("foo", "FOO");

        // Keep this comment too
        Map<String, String> aggregate6 = aggregate5;
        aggregate4.put("bar", "BAR");

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
        final HashMap<String, String> aggregate = new HashMap<String, String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate.put("foo", "bar");
            }
        };

        final TreeMap<String, String> aggregate2 = new TreeMap<String, String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate2.put("foo", "bar");
            }
        };

        final Map<String, String> aggregate3 = new TreeMap<String, String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate3.put("foo", "bar");
            }
        };
    }

    public void replaceMapInsideRunnable() {
        // Keep this comment
        final HashMap<String, long[]> aggregate = new HashMap<String, long[]>();
        aggregate.put("foo", new long[] {0, 1, 2, 3});
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final HashMap<String, long[]> localAggregate = new HashMap<String, long[]>();
                localAggregate.put("bar", new long[] {4, 5, 6, 7});
            }
        };

        // Keep this comment
        final TreeMap<String, long[]> aggregate2 = new TreeMap<String, long[]>();
        aggregate2.put("foo", new long[] {0, 1, 2, 3});
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final TreeMap<String, long[]> localAggregate = new TreeMap<String, long[]>();
                localAggregate.put("bar", new long[] {4, 5, 6, 7});
            }
        };

        // Keep this comment
        final Map<String, long[]> aggregate3 = new TreeMap<String, long[]>();
        aggregate3.put("foo", new long[] {0, 1, 2, 3});
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final Map<String, long[]> localAggregate = new TreeMap<String, long[]>();
                localAggregate.put("bar", new long[] {4, 5, 6, 7});
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
        HashMap<Integer, String> aggregate = new HashMap<Integer, String>();
        aggregate.put(123, "123");
        aggregate.clear();
        aggregate.containsKey(anotherCollection);
        aggregate.isEmpty();
        aggregate.remove(456);
        aggregate.size();
        aggregate.notify();
        aggregate.notifyAll();
        aggregate.wait();
        aggregate.wait(1000);
        aggregate.wait(1000, 1000);

        // Keep this comment too
        TreeMap<Integer, String> aggregate2 = new TreeMap<Integer, String>();
        aggregate2.put(123, "123");
        aggregate2.clear();
        aggregate2.containsKey(anotherCollection);
        aggregate2.isEmpty();
        aggregate2.remove(456);
        aggregate2.size();
        aggregate2.notify();
        aggregate2.notifyAll();
        aggregate2.wait();
        aggregate2.wait(1000);
        aggregate2.wait(1000, 1000);

        // Keep this comment also
        Map<Integer, String> aggregate3 = new TreeMap<Integer, String>();
        aggregate3.put(123, "123");
        aggregate3.clear();
        aggregate3.containsKey(anotherCollection);
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
