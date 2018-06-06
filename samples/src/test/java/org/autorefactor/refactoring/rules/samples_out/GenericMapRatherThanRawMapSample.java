/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

public class GenericMapRatherThanRawMapSample {

    public void replaceTreeMapInstanceCreation(Map<String, String> sourceMap) {
        // Keep this comment
        boolean isFooContained = new TreeMap<String, String>(sourceMap).containsKey("foo");
        // Keep this comment too
        int size = new TreeMap<String, String>(sourceMap).size();
    }

    public void replaceHashMapInstanceCreation(Map<String, String> sourceMap) {
        // Keep this comment
        boolean isFooContained = new HashMap<String, String>(sourceMap).containsKey("foo");
        // Keep this comment too
        int size = new HashMap<String, String>(sourceMap).size();
    }

    public void replaceFullyQualifiedTreeMap(Date aDate, Map<String, String> sourceMap) {
        // Keep this comment
        boolean isFooContained = new java.util.TreeMap<String, String>(sourceMap).containsKey(aDate);
        // Keep this comment too
        int size = new java.util.TreeMap<String, String>(sourceMap).size();
    }

    public void replaceFullyQualifiedHashMap(Date aDate, Map<String, String> sourceMap) {
        // Keep this comment
        boolean isFooContained = new java.util.HashMap<String, String>(sourceMap).containsKey(aDate);
        // Keep this comment too
        int size = new java.util.HashMap<String, String>(sourceMap).size();
    }

    public void replaceTreeMapVariableDeclaration(Map<TreeMap<String, String>, TreeMap<String, String>> sourceMap) {
        // Keep this comment
        java.util.TreeMap<TreeMap<String, String>, TreeMap<String, String>> map = new java.util.TreeMap<TreeMap<String, String>, TreeMap<String, String>>(sourceMap);
    }

    public void replaceHashMapVariableDeclaration(Map<TreeMap<String, String>, TreeMap<String, String>> sourceMap) {
        // Keep this comment
        java.util.HashMap<TreeMap<String, String>, TreeMap<String, String>> map = new java.util.HashMap<TreeMap<String, String>, TreeMap<String, String>>(sourceMap);
    }

    public void replaceInterface(Map<String, String> sourceMap) {
        // Keep this comment
        Map<String, String> map = new java.util.TreeMap<String, String>(sourceMap);
    }

    public void replaceTreeMapVariableUse() {
        // Keep this comment
        java.util.TreeMap<String, BigDecimal> map = new java.util.TreeMap<String, BigDecimal>();
        // Keep this comment too
        map.put("bar", BigDecimal.ONE);
    }

    public void replaceHashMapVariableUse() {
        // Keep this comment
        java.util.HashMap<String, BigDecimal> map = new java.util.HashMap<String, BigDecimal>();
        // Keep this comment too
        map.put("bar", BigDecimal.ONE);
    }

    public void refactorMapVariableUse() {
        // Keep this comment
        Map<String, double[]> map = new java.util.TreeMap<String, double[]>();
        // Keep this comment too
        Set<Entry<String, double[]>> theKeys = (Set<Entry<String, double[]>>) map.entrySet();
        double[] theValue = (double[]) map.remove("bar");
    }

    public void refactorWithMethod() {
        // Keep this comment
        java.util.TreeMap<Observable, Observable> map = new java.util.TreeMap<Observable, Observable>();
        // Keep this comment too
        map.put(new Observable(), new Observable());
    }

    public void refactorMapWithMethod() {
        // Keep this comment
        Map<Integer, AtomicInteger> map = new java.util.TreeMap<Integer, AtomicInteger>();
        // Keep this comment too
        map.put(Integer.valueOf(1), new AtomicInteger());
        BiConsumer<Integer, AtomicInteger> action = (i, dynamicInteger) -> dynamicInteger.addAndGet(i);
        map.forEach(action);
    }

    public String replaceTreeMapWithLoop(Date[] dates) {
        // Keep this comment
        java.util.TreeMap<Long, Date> map = new java.util.TreeMap<Long, Date>();
        for (Date date : dates) {
            map.put(date.getTime(), date);
        }

        return map.toString();
    }

    public String replaceHashMapWithLoop(Date[] dates) {
        // Keep this comment
        java.util.HashMap<Long, Date> map = new java.util.HashMap<Long, Date>();
        for (Date date : dates) {
            map.put(date.getTime(), date);
        }

        return map.toString();
    }

    public String refactorMapWithLoop(Date[] dates) {
        // Keep this comment
        Map<Long, Date> map = new java.util.TreeMap<Long, Date>();
        for (Date date : dates) {
            map.put(date.getTime(), date);
        }

        return map.toString();
    }

    public void replaceTreeMapWithModifier() {
        // Keep this comment
        final java.util.TreeMap<String, BigDecimal> map = new java.util.TreeMap<String, BigDecimal>();
        map.put("bar", BigDecimal.ONE);
    }

    public void replaceHashMapWithModifier() {
        // Keep this comment
        final java.util.HashMap<String, BigDecimal> map = new java.util.HashMap<String, BigDecimal>();
        map.put("bar", BigDecimal.ONE);
    }

    public void refactorMapWithModifier() {
        // Keep this comment
        final Map<String, BigDecimal> map = new java.util.TreeMap<String, BigDecimal>();
        map.put("bar", BigDecimal.ONE);
    }

    public void replaceTreeMapWithParameter() {
        // Keep this comment
        java.util.TreeMap<String, BigDecimal> map = new java.util.TreeMap<String, BigDecimal>(new java.util.TreeMap<String, BigDecimal>());
        map.put("bar", BigDecimal.ONE);
    }

    public void replaceHashMapWithParameter() {
        // Keep this comment
        java.util.HashMap<String, BigDecimal> map = new java.util.HashMap<String, BigDecimal>(new java.util.TreeMap<String, BigDecimal>());
        map.put("bar", BigDecimal.ONE);
    }

    public void refactorMapWithParameter() {
        // Keep this comment
        Map<String, BigDecimal> map = new java.util.TreeMap<String, BigDecimal>(new java.util.TreeMap<String, BigDecimal>());
        map.put("bar", BigDecimal.ONE);
    }

    public boolean replaceReassignedTreeMap() {
        // Keep this comment
        java.util.TreeMap<String, char[]> map1 = new java.util.TreeMap<String, char[]>();
        char[] theValue = (char[]) map1.put("FOO", new char[0]);

        // Keep this comment too
        java.util.TreeMap<String, char[]> map2 = map1;
        theValue = (char[]) map2.put("BAR", new char[0]);

        return map2.isEmpty();
    }

    public boolean replaceReassignedHashMap() {
        // Keep this comment
        java.util.HashMap<String, char[]> map1 = new java.util.HashMap<String, char[]>();
        char[] theValue = (char[]) map1.put("FOO", new char[0]);

        // Keep this comment too
        java.util.HashMap<String, char[]> map2 = map1;
        theValue = (char[]) map2.put("BAR", new char[0]);

        return map2.isEmpty();
    }

    public boolean replaceReassignedMap() {
        // Keep this comment
        java.util.TreeMap<String, char[]> map1 = new java.util.TreeMap<String, char[]>();
        char[] theValue = (char[]) map1.put("FOO", new char[0]);

        // Keep this comment too
        Map<String, char[]> map2 = map1;
        theValue = (char[]) map2.put("BAR", new char[0]);

        return map2.isEmpty();
    }

    public void doNotReplaceTreeMapParameter(TreeMap aTreeMap) {
        TreeMap map = aTreeMap;
        map.put("bar", BigDecimal.ONE);
    }

    public void doNotReplaceTreeMapPassedToAMethod() {
        String text = String.valueOf(new TreeMap());
    }

    public TreeMap doNotReplaceReturnedTreeMap() {
        return new TreeMap();
    }

    public void doNotReplaceReassignedVariable() {
        TreeMap map = new TreeMap();
        map = new TreeMap();
    }

    public void genericizeEquals(Object object1) {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.equals(object1);
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeToString() {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.toString();
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeNotify() {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.notify();
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeNotifyAll() {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.notifyAll();
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeWait() throws InterruptedException {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.wait();
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeWait(long long1) throws InterruptedException {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.wait(long1);
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeWait(long long1, int int1) throws InterruptedException {
        // Keep this comment
        Map<boolean[], Short[]> map = new HashMap<boolean[], Short[]>();
        map.wait(long1, int1);
        map.putAll(new HashMap<boolean[], Short[]>());
    }

    public void genericizeClear() {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.clear();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeContainsKey​(String object1) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.containsKey(object1);
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeContainsValue(String object1) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.containsValue(object1);
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeEquals(String object1) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.equals(object1);
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeForEach(BiConsumer biConsumer1) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.forEach(biConsumer1);
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeHashCode() {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.hashCode();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeIsEmpty() {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.isEmpty();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeSize() {
        // Keep this comment
        Map<String, Exception> map = new HashMap<String, Exception>();
        map.size();
        map.putAll(new HashMap<String, Exception>());
    }

    public void genericizeRemove(String object1, List<String> object2) {
        // Keep this comment
        Map<String, List<String>> map = new HashMap<String, List<String>>();
        map.remove(object1, object2);
        map.putAll(new HashMap<String, List<String>>());
    }

    public void genericizePutAll(Map<Integer, Date> map1) {
        // Keep this comment
        Map<Integer, Date> map = new HashMap<Integer, Date>();
        map.putAll(map1);
    }

    public void genericizeGet(String object1) {
        // Keep this comment
        Map<String, Double> map = new HashMap<String, Double>();
        Double theValue = (Double) map.get(object1);
        map.putAll(new HashMap<String, Double>());
    }

    public void genericizeRemove(String object1) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        String theValue = (String) map.remove(object1);
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeLastKey() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        String theValue = (String) map.lastKey();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeFirstKey() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        String theValue = (String) map.firstKey();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeKeySet() {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        Set<String> theKeys = (Set<String>) map.keySet();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeComparator() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Comparator<String> theComparator = (Comparator<String>) map.comparator();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeDescendingKeySet() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Set<String> theKeys = (Set<String>) map.descendingKeySet();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeNavigableKeySet() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Set<String> theKeys = (Set<String>) map.navigableKeySet();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeValues() {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        Collection<String> theValues = (Collection<String>) map.values();
        map.putAll(new HashMap<String, String>());
    }

    public void genericizeDescendingMap() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.descendingMap();
    }

    public void genericizeFirstEntry() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.firstEntry();
    }

    public void genericizeLastEntry() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.lastEntry();
    }

    public void genericizePollFirstEntry() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.pollFirstEntry();
    }

    public void genericizePollLastEntry() {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.pollLastEntry();
    }

    public void genericizeCeilingEntry(String object1) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.ceilingEntry(object1);
    }

    public void genericizeFloorEntry(String object1) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.floorEntry(object1);
    }

    public void genericizeHeadMap(String object1) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.headMap(object1);
    }

    public void genericizeHeadMap(String object1, boolean boolean2) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.headMap(object1, boolean2);
    }

    public void genericizeHigherEntry(String object1) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.higherEntry(object1);
    }

    public void genericizeLowerEntry(String object1) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Entry<String, String> theEntry = (Entry<String, String>) map.lowerEntry(object1);
    }

    public void genericizeTailMap(String object1) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.tailMap(object1);
    }

    public void genericizeTailMap(String object1, boolean boolean2) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.tailMap(object1, boolean2);
    }

    public void genericizeSubMap(String object1, String object2) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.subMap(object1, object2);
    }

    public void genericizeEntrySet() {
        // Keep this comment
        Map<String, Float> map = new HashMap<String, Float>();
        Set<Entry<String, Float>> theEntry = (Set<Entry<String, Float>>) map.entrySet();
    }

    public void genericizeCeilingKey(String object1) {
        // Keep this comment
        TreeMap<String, int[]> map = new TreeMap<String, int[]>();
        map.ceilingKey(object1);
        Collection<int[]> theValues = (Collection<int[]>) map.values();
    }

    public void genericizeFloorKey(String object1) {
        // Keep this comment
        TreeMap<String, int[]> map = new TreeMap<String, int[]>();
        map.floorKey(object1);
        Collection<int[]> theValues = (Collection<int[]>) map.values();
    }

    public void genericizeHigherKey(String object1) {
        // Keep this comment
        TreeMap<String, int[]> map = new TreeMap<String, int[]>();
        map.higherKey(object1);
        Collection<int[]> theValues = (Collection<int[]>) map.values();
    }

    public void genericizeLowerKey(String object1) {
        // Keep this comment
        TreeMap<String, int[]> map = new TreeMap<String, int[]>();
        map.lowerKey(object1);
        Collection<int[]> theValues = (Collection<int[]>) map.values();
    }

    public void genericizeGetOrDefault(String object1, byte[] object2) {
        // Keep this comment
        TreeMap<String, byte[]> map = new TreeMap<String, byte[]>();
        byte[] theValue = (byte[]) map.getOrDefault(object1, object2);
        map.lowerKey(object1);
    }

    public void genericizePut(String object1, String object2) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        String theValue = (String) map.put(object1, object2);
    }

    public void genericizePutIfAbsent(String object1, String object2) {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
        map.putIfAbsent(object1, object2);
    }

    public void genericizeReplace(Long object1, Date object2) {
        // Keep this comment
        Map<Long, Date> map = new HashMap<Long, Date>();
        Date theValue = (Date) map.replace(object1, object2);
    }

    public void genericizeReplace(BigInteger object1, Boolean object2, Boolean object3) {
        // Keep this comment
        Map<BigInteger, Boolean> map = new HashMap<BigInteger, Boolean>();
        map.replace(object1, object2, object3);
    }

    public void genericizeSubMap(String object1, boolean boolean2, String object3,
            boolean boolean4) {
        // Keep this comment
        TreeMap<String, String> map = new TreeMap<String, String>();
        Map<String, String> theMap = (Map<String, String>) map.subMap(object1, boolean2, object3, boolean4);
    }

    public void genericizeCompute(String object1, BiFunction<String, Integer, Integer> biFunction2) {
        // Keep this comment
        Map<String, Integer> map = new HashMap<String, Integer>();
        Integer theValue = (Integer) map.compute(object1, biFunction2);
    }

    public void genericizeComputeIfPresent(String object1, BiFunction<String, Integer, Integer> biFunction2) {
        // Keep this comment
        Map<String, Integer> map = new HashMap<String, Integer>();
        Integer theValue = (Integer) map.computeIfPresent(object1, biFunction2);
    }

    public void genericizeComputeIfAbsent(String object1, Function<String, Integer> function2) {
        // Keep this comment
        Map<String, Integer> map = new HashMap<String, Integer>();
        Integer theValue = (Integer) map.computeIfAbsent(object1, function2);
    }

    public void genericizeMerge(String object1, BigDecimal object2,
            BiFunction<BigDecimal, BigDecimal, BigDecimal> biFunction3) {
        // Keep this comment
        Map<String, BigDecimal> map = new HashMap<String, BigDecimal>();
        BigDecimal theValue = (BigDecimal) map.merge(object1, object2, biFunction3);
    }

    public void genericizeReplaceAll(BiFunction<String, BigDecimal, BigDecimal> biFunction1) {
        // Keep this comment
        TreeMap<String, BigDecimal> map = new TreeMap<String, BigDecimal>();
        map.replaceAll(biFunction1);
        String theKey = (String) map.firstKey();
    }

    public void replaceTreeMapWithRunnable() {
        // Keep this comment
        final java.util.TreeMap<String, String> map = new java.util.TreeMap<String, String>();
        String theValue = (String) map.putIfAbsent("foo", "nothing");
        new Runnable() {

            @Override
            public void run() {
                final java.util.TreeMap<String, String> localMap = new java.util.TreeMap<String, String>();
                String theValue = (String) localMap.put("foo", "foo");
            }
        };
    }

    public void replaceMapWithRunnable() {
        // Keep this comment
        final Map<String, String> map = new java.util.TreeMap<String, String>();
        String theValue = (String) map.putIfAbsent("foo", "nothing");
        new Runnable() {

            @Override
            public void run() {
                final Map<String, String> localMap = new java.util.TreeMap<String, String>();
                String theValue = (String) localMap.put("foo", "foo");
            }
        };
    }

    public void doNotReplaceField() {
        Comparator c = new Comparator() {

            private String doNotRefactorTheExpression = (String) new TreeMap().put("foo", "foo");

            private TreeMap doNotReplaceField = new TreeMap();

            @Override
            public int compare(Object arg0, Object arg1) {
                return doNotReplaceField.containsKey(arg0) || doNotReplaceField.containsValue(arg1) ? 1 : -1;
            }
        };
    }
}
