/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.io.File;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.EventObject;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class TreeSetRatherThanTreeMapSample {
    public TreeSetRatherThanTreeMapSample() {
        this(new TreeMap<String, Exception>());
    }

    public TreeSetRatherThanTreeMapSample(Map<String, Exception> parameter) {
    }

    public void doNotReplaceMapInstanceCreation() {
        boolean isInside = new TreeMap<String, Exception>().isEmpty();

        int size = new TreeMap<String, Exception>().size();

        new TreeMap<String, Exception>().clear();
    }

    public void doNotReplaceRawMap() {
        boolean isInside = new TreeMap().isEmpty();

        int size = new TreeMap().size();
    }

    public void doNotReplaceFullyQualifiedMap() {
        boolean isInside = new java.util.TreeMap<Integer, Date>().isEmpty();

        int size = new java.util.TreeMap().size();
    }

    public void doNotReplaceMapVariableDeclaration() {
        TreeMap<String, Observable> aggregate = new TreeMap<String, Observable>();
        Map<String, Observable> aggregate2 = new TreeMap<String, Observable>();
    }

    public void doNotReplaceMapVariableDeclarationWithDiamondOperator() {
        TreeMap<String, Observable> aggregate = new TreeMap<>();
        Map<String, Observable> aggregate2 = new TreeMap<>();
    }

    public void replaceTreeMapVariableUse() {
        // Keep this comment
        TreeSet<File> aggregate = new TreeSet<File>(Comparator.comparing((File o) -> o.separator));
        File e = new File("");
        // Keep this comment too
        aggregate.add(e);

        // Keep this comment
        Set<Exception> aggregate2 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e2 = new NullPointerException();
        // Keep this comment too
        aggregate2.add(e2);
    }

    public boolean doNotRefactorTreeMapMethod() {
        return new TreeMap<String, Exception>().containsValue(new NullPointerException());
    }

    public void refactorVariableAndMethod() {
        // Keep this comment
        TreeSet<Exception> aggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e = new NullPointerException();
        aggregate.add(e);
        // Keep this comment too
        aggregate.contains(e);

        // Keep this comment
        Set<Exception> aggregate2 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e2 = new NullPointerException();
        aggregate2.add(e2);
        // Keep this comment too
        aggregate2.contains(e2);
    }

    public int replaceMapWithLoop(List<Date> dates) {
        // Keep this comment too
        TreeSet<Date> aggregate = new TreeSet<Date>(Comparator.comparing(Date::getTime));
        for (Date date : dates) {
            aggregate.add(date);
        }

        // Keep this comment also
        Set<Date> aggregate2 = new TreeSet<Date>(Comparator.comparing(Date::getTime));
        for (Date date : dates) {
            aggregate2.add(date);
        }

        return aggregate.size() + aggregate2.size();
    }

    public int doNotRemoveActiveCode(List<PrintCounter> referenceCounters,
            List<PrintCounter> counters) {
        long key = 0;

        TreeMap<Long, String> aggregate = new TreeMap<Long, String>();
        for (PrintCounter counter : referenceCounters) {
            aggregate.put(key++, "The counter is " + counter);
        }

        Map<Long, String> aggregate2 = new TreeMap<Long, String>();
        for (PrintCounter counter : referenceCounters) {
            aggregate2.put(key++, "The counter is " + counter);
        }

        return aggregate.size() + aggregate2.size();
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

    public void replaceTreeMapWithModifier() {
        // Keep this comment too
        final TreeSet<Exception> aggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e = new NullPointerException();
        aggregate.add(e);

        // Keep this comment also
        final Set<Exception> aggregate2 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e2 = new NullPointerException();
        aggregate2.add(e2);
    }

    public void replaceTreeMapWithParameter() {
        // Keep this comment too
        TreeSet<Exception> aggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e = new NullPointerException();
        aggregate.add(e);

        // Keep this comment also
        Set<Exception> aggregate2 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e2 = new NullPointerException();
        aggregate2.add(e2);
    }

    public int replaceReassignedTreeMap() {
        // Keep this comment
        TreeSet<Exception> aggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e = new NullPointerException();
        aggregate.add(e);

        // Keep this comment too
        TreeSet<Exception> aggregate2 = aggregate;
        Exception e2 = new NullPointerException();
        aggregate2.add(e2);

        // Keep this comment
        Set<Exception> aggregate3 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e3 = new NullPointerException();
        aggregate3.add(e3);

        // Keep this comment too
        Set<Exception> aggregate4 = aggregate3;
        Exception e4 = new NullPointerException();
        aggregate4.add(e4);

        return aggregate.size() + aggregate2.size() + aggregate3.size() + aggregate4.size();
    }

    public void doNotReplaceTreeMapParameter(TreeMap<String, Exception> aMap,
            TreeMap<String, Exception> aMap2) {
        TreeMap<String, Exception> map = aMap;
        Exception e = new NullPointerException();
        map.put(e.getMessage(), e);

        Map<String, Exception> map2 = aMap2;
        Exception e2 = new NullPointerException();
        map.put(e2.getMessage(), e2);
    }

    public void doNotReplaceTreeMapPassedToAMethod() {
        String text = String.valueOf(new TreeMap<String, Exception>());
        equals(new TreeMap<String, Exception>());
    }

    public Object doNotReplaceTreeMapPassedToAConstructor() {
        return new EventObject(new TreeMap<String, Exception>());
    }

    public Object doNotReplaceTreeMapInConditionalExpression(boolean b) {
        return b ? new TreeMap<String, Exception>() : null;
    }

    public TreeMap<Integer, Date> doNotReplaceReturnedTreeMap() {
        return new TreeMap<Integer, Date>();
    }

    public void doNotReplaceReassignedVariable() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        map = new TreeMap<String, Exception>();

        Map<String, Exception> map2 = new TreeMap<String, Exception>();
        map2 = new TreeMap<String, Exception>();
    }

    public void replaceTreeMapInAnnomymousClass() {
        final TreeSet<Exception> aggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        new Runnable() {

            @Override
            public void run() {
                Exception e = new NullPointerException();
                aggregate.add(e);
            }
        };

        final Set<Exception> aggregate2 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        new Runnable() {

            @Override
            public void run() {
                Exception e = new NullPointerException();
                aggregate2.add(e);
            }
        };
    }

    public void replaceMapInsideRunnable() {
        // Keep this comment
        final TreeSet<Exception> aggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e = new NullPointerException();
        aggregate.add(e);
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final TreeSet<Exception> localAggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
                Exception e = new NullPointerException();
                localAggregate.add(e);
            }
        };

        // Keep this comment
        final Set<Exception> aggregate2 = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
        Exception e2 = new NullPointerException();
        aggregate2.add(e2);
        new Runnable() {

            @Override
            public void run() {
                // Keep this comment too
                final Set<Exception> localAggregate = new TreeSet<Exception>(Comparator.comparing(Exception::getMessage));
                Exception e = new NullPointerException();
                localAggregate.add(e);
            }
        };
    }

    public void doNotReplaceSpecificInstantiation(TreeMap<String, Exception> aMap) {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>(aMap);
        Exception e = new NullPointerException();
        map.put(e.getMessage(), e);

        Map<String, Exception> map2 = new TreeMap<String, Exception>(aMap);
        Exception e2 = new NullPointerException();
        map2.put(e2.getMessage(), e2);
    }

    public Object doNotReplaceCloneMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.clone();
    }

    public boolean doNotReplaceContainsKeyMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.containsKey("bar");
    }

    public Set<Entry<String, Exception>> doNotReplaceEntrySetMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.entrySet();
    }

    public boolean doNotReplaceEqualsMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.equals(123);
    }

    public Exception doNotReplaceGetMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.get("foo");
    }

    public int doNotReplaceHashCodeMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.hashCode();
    }

    public String doNotReplaceToStringMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        return map.toString();
    }

    public void doNotReplaceRemoveTreeMapMethod() {
        TreeMap<String, Exception> map = new TreeMap<String, Exception>();
        map.remove("foo");
    }

    public void refactorWithMethods(Collection<Integer> anotherCollection) throws InterruptedException {
        // Keep this comment
        TreeSet<String> aggregate = new TreeSet<String>(Comparator.comparing(String::length));
        String text = "123";
        aggregate.add(text);
        aggregate.clear();
        aggregate.contains(anotherCollection);
        aggregate.isEmpty();
        aggregate.size();
        aggregate.notify();
        aggregate.notifyAll();
        aggregate.size();
        aggregate.wait();
        aggregate.wait(1000);
        aggregate.wait(1000, 1000);

        // Keep this comment too
        Set<String> aggregate2 = new TreeSet<String>(Comparator.comparing(String::length));
        aggregate2.add(text);
        aggregate2.clear();
        aggregate2.contains(anotherCollection);
        aggregate2.isEmpty();
        aggregate2.size();
        aggregate2.notify();
        aggregate2.notifyAll();
        aggregate2.size();
        aggregate2.wait();
        aggregate2.wait(1000);
        aggregate2.wait(1000, 1000);
    }
}
