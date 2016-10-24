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

import java.util.Iterator;
import java.util.Map;

public class MapEliminateKeySetCallsSample {
    public int replaceUnnecesaryCallsToMapKeySet(Map<String, String> map) {
        int x = map.keySet().size();
        if (map.keySet().contains("hello")) {
            map.keySet().remove("hello");
        }
        map.keySet().clear();
        if (map.keySet().isEmpty()) {
            x++;
        }
        return x;
    }

    public void iterate(Map<String, Long> map) {
        for (String key : map.keySet()) {
            System.out.println("hello "+map.get("name"));
            if (map.get(key).longValue() > 0) {
                System.out.println("the value of "+ key + " is " + map.get(key));
            }
        }
    }

    public void justListKeys(Map<String, Long> map) {
        for (String key : map.keySet()) {
            System.out.println("key is "+key);
        }
    }

    public void iterateOldWay(Map<String, Long> map) {
        for (Iterator<String> iter = map.keySet().iterator(); iter.hasNext();) {
            String key = iter.next();
            if (map.get(key).longValue() > 0) {
                System.out.println(map.get(key));
            }
        }
    }

}
