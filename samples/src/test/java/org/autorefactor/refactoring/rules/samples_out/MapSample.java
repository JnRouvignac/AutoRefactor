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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MapSample {

    public void replaceNewNoArgsAssignmentThenPutAll(Map<String, String> map, Map<String, String> output) {
        // Keep this comment
        output = new HashMap<String, String>(map);
    }

    public Map<String, String> replaceNewNoArgsThenPutAll(Map<String, String> map) {
        // Keep this comment
        final Map<String, String> output = new HashMap<String, String>(map);
        return output;
    }

    public Map<String, String> replaceNew0ArgThenPutAll(Map<String, String> map) {
        // Keep this comment
        final Map<String, String> output = new HashMap<String, String>(map);
        return output;
    }

    public Map<String, String> replaceNew1ArgThenPutAll(Map<String, String> map) {
        // Keep this comment
        final Map<String, String> output = new HashMap<String, String>(map);
        return output;
    }

    public Map<String, String> replaceNewMapSizeThenPutAll(Map<String, String> map) {
        // Keep this comment
        final Map<String, String> output = new HashMap<String, String>(map);
        return output;
    }

    public Map<String, String> replaceWithSizeOfSubMap(List<Map<String, String>> listOfMap) {
        // Keep this comment
        final Map<String, String> output = new HashMap<String, String>(listOfMap.get(0));
        return output;
    }

    public Map<String, String> doNotReplaceAlreadyInitedMap(Map<String, String> map1, Map<String, String> map2) {
        final Map<String, String> output = new HashMap<String, String>(map1);
        output.putAll(map2);
        return output;
    }

    public Map<String, String> doNotReplaceWithSpecificSize(Map<String, String> map) {
        final Map<String, String> output = new HashMap<String, String>(10);
        output.putAll(map);
        return output;
    }

    public Map<Object, Object> doNotReplaceNewThenAddAllIncompatibleTypes(Map<String, String> map) {
        final Map<Object, Object> output = new HashMap<>();
        output.putAll(map);
        return output;
    }
}
