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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.Map;

public class MethodOnMapRatherThanMethodOnKeySetSample {
    public int replaceUnnecesaryCallsToMapKeySet(Map<String, String> map) {
        // Keep this comment
        int x = map.size();
        if (map.containsKey("hello")) {
            map.remove("hello");
        }
        if (map.keySet().remove("world")) {
            // Cannot replace, because `map.removeKey("world") != null` is not strictly equivalent
            System.out.println(map);
        }
        // Keep this comment also
        map.clear();
        // Keep this comment too
        if (map.isEmpty()) {
            x++;
        }
        return x;
    }
}
