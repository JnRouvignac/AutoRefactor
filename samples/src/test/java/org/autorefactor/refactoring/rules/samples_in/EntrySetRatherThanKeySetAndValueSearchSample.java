/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Zsombor Gegesy - initial API and implementation
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class EntrySetRatherThanKeySetAndValueSearchSample {
    private static class Dummy {
    }

    private Map<String, Long> mapField;
    private Map<String, Map<?, ?>> mapFieldWithWildcards;

    public void refactorMapKeySet(Map<String, Long> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            if (map.get(key) != null) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void refactorValueTypeUsesPrimitive(Map<String, byte[]> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            if (map.get(key).length != 0) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void refactorValueTypeUsesGenerics(Map<String, List<Map<Integer, List<Long>>>> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            if (!map.get(key).isEmpty()) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void refactorValueTypeUsesGenericsWithCapture(Map<String, Map<?, ? extends Integer>> map) {
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            System.out.println("the value of "+ key + " is " + map.get(key));
        }
    }

    public void refactorValueTypeUsesGenericsWithCaptures(Map<String, Map<?, ? extends Integer>> map) {
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            if (!map.get(key).isEmpty()) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void refactorValueTypeUsesGenericsWithWildcards() {
        for (String key : mapFieldWithWildcards.keySet()) {
            System.out.println("hello " + mapFieldWithWildcards.get("name"));
            if (!mapFieldWithWildcards.get(key).isEmpty()) {
                System.out.println("the value of "+ key + " is " + mapFieldWithWildcards.get(key));
            }
        }
    }

    public void refactorVariableDeclarationStatement(Map<String, Long> map) {
        Map<String, Long> map2 = map;
        // Keep this comment
        for (String key : map2.keySet()) {
            System.out.println("hello " + map2.get("name"));
            if (map2.get(key) != null) {
                System.out.println("the value of "+ key + " is " + map2.get(key));
            }
        }
    }

    public void refactorVariableDeclarationExpression(Map<String, Long> map) {
        int i = 0;
        for (Map<String, Long> map2 = map; i <= 0; i++) {
            // Keep this comment
            for (String key : map2.keySet()) {
                System.out.println("hello " + map2.get("name"));
                if (map2.get(key) != null) {
                    System.out.println("the value of "+ key + " is " + map2.get(key));
                }
            }
        }
    }

    public void refactorField() {
        // Keep this comment
        for (String key : mapField.keySet()) {
            System.out.println("hello " + mapField.get("name"));
            if (mapField.get(key) != null) {
                System.out.println("the value of "+ key + " is " + mapField.get(key));
            }
        }
    }

    public void refactorMethodInvocation() {
        ProcessBuilder pb = new ProcessBuilder();
        // Keep this comment
        for (String key : pb.environment().keySet()) {
            System.out.println("hello " + pb.environment().get("name"));
            if (pb.environment().get(key) != null) {
                System.out.println("the value of "+ key + " is " + pb.environment().get(key));
            }
        }
    }

    public void refactorRawMap(Map map) {
        // Keep this comment
        for (Object key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            if (((Long) map.get(key)) != null) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void refactorMapKeySetWithUnboxingOfKey(Map<Byte, Long> map) {
        // Keep this comment
        for (byte key : map.keySet()) {
            System.out.println("hello " + map.get("name"));
            if (map.get(key) != null) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void doNotRefactor_noUseOfMapGet(Map<String, Long> map) {
        for (String key : map.keySet()) {
            System.out.println("key is "+key);
        }
    }

    public void doNotRefactor_forWithIterator(Map<String, Long> map) {
        for (Iterator<String> iter = map.keySet().iterator(); iter.hasNext();) {
            String key = iter.next();
            if (map.get(key) != null) {
                System.out.println(map.get(key));
            }
        }
    }

    public void refactorAndAvoidNameConflictsWithExistingVariableBefore(Map<String, Long> map, long entry) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println(map.get(key));
        }
    }

    public void refactorAndAvoidNameConflictsWithExistingVariableAfter(Map<String, Long> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println(map.get(key));
        }
        long entry = 0;
        System.out.println(entry);
    }

    public void refactorAndAvoidNameShadowingOfExistingVariableUse(Map<String, Long> map, long entry) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println(map.get(key));
        }
        System.out.println(entry);
    }

    public void refactorAndAvoidNameShadowingOfExistingFieldUse(Map<String, Long> map, long entry) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println(map.get(key));
        }
        System.out.println(entry);
    }

    public void refactorMapKeySetUseSimpleNameWhenInSamePackage(Map<String, Dummy> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get(key));
        }
    }

    public <V>void refactorMapWithTypeVariable(Map<String, V> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get(key));
        }
    }

    public <V extends Object>void refactorMapWithUpperBoundedTypeVariable(Map<String, V> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get(key));
        }
    }

    public <V extends ArrayList<Integer>>void refactorMapWithLowerBoundedTypeVariable(Map<String, V> map) {
        // Keep this comment
        for (String key : map.keySet()) {
            System.out.println("hello " + map.get(key));
        }
    }
}
