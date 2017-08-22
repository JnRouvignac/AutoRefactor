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

public class SetRatherThanMapSample {

    public SetRatherThanMapSample() {
        this(new HashMap<String, String>());
    }

    public SetRatherThanMapSample(Map<String, String> parameter) {
    }

    public void replaceHashMapInstanceCreation() {
        // Keep this comment
        boolean isInside = new java.util.HashSet<String>().isEmpty();
        // Keep this comment too
        int size = new java.util.HashSet<String>(10).size();
    }

    public void replaceRawHashMap() {
        // Keep this comment
        boolean isInside = new java.util.HashSet().isEmpty();
        // Keep this comment too
        int size = new java.util.HashSet(10).size();
    }

    public void replaceFullyQualifiedHashMap() {
        // Keep this comment
        boolean isInside = new java.util.HashSet<Integer>().isEmpty();
        // Keep this comment too
        int size = new java.util.HashSet(10).size();
    }

    public void replaceHashMapVariableDeclaration() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
    }

    public void doNotReplaceInterface() {
        // Keep this comment
        Map<String, String> map = new HashMap<String, String>();
    }

    public void replaceHashMapVariableUse() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        // Keep this comment too
        aggregate.add("foo");
    }

    public boolean refactorMethod() {
        // Keep this comment
        return new java.util.HashSet<String>().contains("foo");
    }

    public void refactorVariableAndMethod() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        // Keep this comment too
        aggregate.contains("foo");
    }

    public int replaceHashMapWithLoop(List<Date> dates) {
        // Keep this comment
        java.util.HashSet<Long> aggregate = new java.util.HashSet<Long>();
        for (Date date : dates) {
            aggregate.add(date.getTime());
        }

        return aggregate.size();
    }

    public int doNotRemoveActiveCode(List<Date> referenceDates, List<Date> dates) {
        HashMap<Long, Boolean> aggregate = new HashMap<Long, Boolean>();
        for (Date date : referenceDates) {
            aggregate.put(date.getTime(), dates.remove(date));
        }

        return aggregate.size();
    }

    public void replaceHashMapWithModifier() {
        // Keep this comment
        final java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        aggregate.add("foo");
    }

    public void replaceHashMapWithParameter() {
        // Keep this comment
        java.util.HashSet<String> aggregate = new java.util.HashSet<String>(10);
        aggregate.add("foo");
    }

    public int replaceReassignedHashMap() {
        // Keep this comment
        java.util.HashSet<String> aggregate1 = new java.util.HashSet<String>();
        aggregate1.add("foo");

        // Keep this comment too
        java.util.HashSet<String> aggregate2 = aggregate1;
        aggregate2.add("bar");

        return aggregate2.size();
    }

    public void doNotReplaceHashMapParameter(HashMap<String, String> aMap) {
        HashMap<String, String> map = aMap;
        map.put("foo", "bar");
    }

    public void doNotReplaceHashMapPassedToAMethod() {
        String text = String.valueOf(new HashMap<String, String>());
        equals(new HashMap<String, String>());
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
    }

    public void replaceHashMapInAnnomymousClass() {
        final java.util.HashSet<String> aggregate = new java.util.HashSet<String>();
        new Runnable() {

            @Override
            public void run() {
                aggregate.add("foo");
            }
        };
    }

    public void replaceHashMapInsideRunnable() {
        // Keep this comment
        final java.util.HashSet<String> set = new java.util.HashSet<String>();
        set.add("foo");
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final java.util.HashSet<String> localSet = new java.util.HashSet<String>();
                localSet.add("bar");
            }
        };
    }

    public void doNotReplaceSpecificInstantiation(HashMap<String, String> aMap) {
        HashMap<String, String> map = new HashMap<String, String>(aMap);
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

    public void refactorWithMethods(Collection<Integer> anotherCollection) throws InterruptedException {
        // Keep this comment
        java.util.HashSet<Integer> aggregate = new java.util.HashSet<Integer>();
        aggregate.add(123);
        aggregate.clear();
        aggregate.contains(anotherCollection);
        aggregate.isEmpty();
        aggregate.remove(123);
        aggregate.size();
        aggregate.notify();
        aggregate.notifyAll();
        aggregate.wait();
        aggregate.wait(1000);
        aggregate.wait(1000, 1000);
    }
}
