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

import java.util.Comparator;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observer;
import java.util.Set;

public class HashMapRatherThanHashtableSample {

    private Set<Entry<String, Observer>> doNotRefactorTheExpression = new Hashtable<String, Observer>().entrySet();

    private Hashtable<String, Integer> doNotReplaceField = new Hashtable<String, Integer>();

    public void replaceHashtableInstanceCreation() {
        // Keep this comment
        Set<Entry<String, String>> entrySet = new Hashtable<String, String>().entrySet();
        // Keep this comment too
        int size = new Hashtable<String, String>(10).size();
    }

    public void replaceRawHashtable() {
        // Keep this comment
        Set<Entry> entrySet = new Hashtable().entrySet();
        // Keep this comment too
        int size = new Hashtable(10).size();
    }

    public void replaceFullyQualifiedHashtable() {
        // Keep this comment
        Set<Entry<Integer, Date>> entrySet = new java.util.Hashtable<Integer, Date>().entrySet();
        // Keep this comment too
        int size = new java.util.Hashtable(10).size();
    }

    public void replaceHashtableVariableDeclaration() {
        // Keep this comment
        Hashtable<String, String> map = new Hashtable<String, String>();
    }

    public void replaceMapVariableDeclaration() {
        // Keep this comment
        Map<String, String> map = new Hashtable<String, String>();
    }

    public void replaceHashtableVariableUse() {
        // Keep this comment
        Hashtable<String, String> map = new Hashtable<String, String>();
        // Keep this comment too
        map.put("foo", "bar");
    }

    public void replaceMapVariableUse() {
        // Keep this comment
        Map<String, String> map = new Hashtable<String, String>();
        // Keep this comment too
        map.put("foo", "bar");
    }

    public void refactorHashtableMethod() {
        // Keep this comment
        Hashtable<String, List<String>[]> map = new Hashtable<String, List<String>[]>();
        // Keep this comment too
        map.values();
    }

    public void refactorMapMethod() {
        // Keep this comment
        Map<String, List<String>[]> map = new Hashtable<String, List<String>[]>();
        // Keep this comment too
        map.values();
    }

    public String replaceHashtableWithLoop(List<Date> dates) {
        // Keep this comment
        Hashtable<Long, Date> map = new Hashtable<Long, Date>();
        for (Date date : dates) {
            map.put(date.getTime(), date);
        }

        return map.toString();
    }

    public String replaceMapWithLoop(List<Date> dates) {
        // Keep this comment
        Map<Long, Date> map = new Hashtable<Long, Date>();
        for (Date date : dates) {
            map.put(date.getTime(), date);
        }

        return map.toString();
    }

    public void replaceHashtableWithModifier() {
        // Keep this comment
        final Hashtable<String, int[]> map = new Hashtable<String, int[]>();
        map.put("foo", new int[] {1, 2, 3});
    }

    public void replaceMapWithModifier() {
        // Keep this comment
        final Map<String, int[]> map = new Hashtable<String, int[]>();
        map.put("foo", new int[] {1, 2, 3});
    }

    public void replaceHashtableWithParameter() {
        // Keep this comment
        Hashtable<String, String> map = new Hashtable<String, String>(10);
        map.put("foo", "bar");
    }

    public void replaceMapWithParameter() {
        // Keep this comment
        Map<String, String> map = new Hashtable<String, String>(10);
        map.put("foo", "bar");
    }

    public Set<Entry<String, String>> replaceReassignedHashtable() {
        // Keep this comment
        Hashtable<String, String> map1 = new Hashtable<String, String>();
        map1.put("foo", "FOO");

        // Keep this comment too
        Hashtable<String, String> map2 = map1;
        map2.put("bar", "BAR");

        return map2.entrySet();
    }

    public Set<Entry<String, String>> replaceReassignedMap() {
        // Keep this comment
        Map<String, String> map1 = new Hashtable<String, String>();
        map1.put("foo", "FOO");

        // Keep this comment too
        Map<String, String> map2 = map1;
        map2.put("bar", "BAR");

        return map2.entrySet();
    }

    public void doNotReplaceHashtableParameter(Hashtable<String, String> aMap) {
        Hashtable<String, String> map = aMap;
        map.put("foo", "bar");
    }

    public void doNotReplaceHashtablePassedToAMethod() {
        String text = String.valueOf(new Hashtable<String, String>());
    }

    public Hashtable<Integer, Date> doNotReplaceReturnedHashtable() {
        return new Hashtable<Integer, Date>();
    }

    public void doNotReplaceReassignedVariable() {
        Hashtable<String, String> map = new Hashtable<String, String>();
        map = new Hashtable<String, String>();
    }

    public void replaceThreadLocalHashtable() {
        final Hashtable<String, String> map = new Hashtable<String, String>();
        map.put("foo", "bar");
        new Runnable() {

            @Override
            public void run() {
                final Hashtable<String, String> localMap = new Hashtable<String, String>();
                localMap.put("foo", "bar");
            }
        };
    }

    public void replaceThreadLocalMap() {
        final Map<String, String> map = new Hashtable<String, String>();
        map.put("foo", "bar");
        new Runnable() {

            @Override
            public void run() {
                final Map<String, String> localMap = new Hashtable<String, String>();
                localMap.put("foo", "bar");
            }
        };
    }

    public void doNotReplaceThreadSharedHashtable() {
        final Hashtable<String, String> map = new Hashtable<String, String>();
        new Runnable() {

            @Override
            public void run() {
                map.put("No conflict", "please");
            }
        };
    }

    public void doNotReplaceField() {
        Comparator<String> c = new Comparator<String>() {

            private Set<Entry<String, Observer>> doNotRefactorTheExpression = new Hashtable<String, Observer>().entrySet();

            private Hashtable<String, Integer> doNotReplaceField = new Hashtable<String, Integer>();

            @Override
            public int compare(String arg0, String arg1) {
                return doNotReplaceField.get(arg1) - doNotReplaceField.get(arg0);
            }

        };
    }
}
