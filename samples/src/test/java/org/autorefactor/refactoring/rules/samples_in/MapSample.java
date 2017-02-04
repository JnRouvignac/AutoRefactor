/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.HashMap;
import java.util.Map;

public class MapSample {

    public void replaceNewNoArgsAssignmentThenPutAll(Map<String, String> map, Map<String, String> output) {
        // Keep this comment 1
        output = new HashMap<String, String>();
        output.putAll(map);
    }

    public Map<String, String> replaceNewNoArgsThenPutAll(Map<String, String> map) {
        // Keep this comment 1
        final Map<String, String> output = new HashMap<String, String>();
        output.putAll(map);
        return output;
    }

    public Map<String, String> replaceNew0ArgThenPutAll(Map<String, String> map) {
        // Keep this comment 1
        final Map<String, String> output = new HashMap<String, String>(0);
        output.putAll(map);
        return output;
    }

    public Map<String, String> replaceNew1ArgThenPutAll(Map<String, String> map) {
        // Keep this comment 1
        final Map<String, String> output = new HashMap<String, String>(0);
        output.putAll(map);
        return output;
    }

    public Map<String, String> replaceNewMapSizeThenPutAll(Map<String, String> map) {
        // Keep this comment 1
        final Map<String, String> output = new HashMap<String, String>(map.size());
        output.putAll(map);
        return output;
    }

    public Map<Object, Object> doNotReplaceNewThenAddAllIncompatibleTypes(Map<String, String> map) {
        final Map<Object, Object> output = new HashMap<>();
        output.putAll(map);
        return output;
    }

    public void replaceChecksOnSize(Map<String, String> map) {
        // Keep this comment 1
        System.out.println(map.size() > 0);
        System.out.println(map.size() >= 0);
        System.out.println(map.size() == 0);
        System.out.println(map.size() <= 0);
        System.out.println(map.size() < 0);

        System.out.println(0 < map.size());
        System.out.println(0 <= map.size());
        System.out.println(0 == map.size());
        System.out.println(0 >= map.size());
        System.out.println(0 > map.size());
    }
}
