/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import static com.google.gwt.thirdparty.guava.common.collect.Maps.newHashMap;
import static com.google.gwt.thirdparty.guava.common.collect.Maps.newIdentityHashMap;
import static com.google.gwt.thirdparty.guava.common.collect.Maps.newLinkedHashMap;
import static com.google.gwt.thirdparty.guava.common.collect.Maps.newTreeMap;
import static com.google.gwt.thirdparty.guava.common.collect.Maps.newEnumMap;

import java.time.DayOfWeek;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Observer;
import java.util.TreeMap;
import com.google.gwt.thirdparty.guava.common.collect.Lists;

public class AggregateConstructorRatherThanGWTMethodSample {
    public List<String> replaceGWTMethodByListConstructor() {
        // Keep this comment
        return new ArrayList<>();
    }

    public List<String> replaceGWTMethodByLinkedListConstructor() {
        // Keep this comment
        return new LinkedList<>();
    }

    public Map<String, Integer> replaceGWTMethodByMapConstructor() {
        // Keep this comment
        return new HashMap<>();
    }

    public Map<String, Integer> replaceGWTMethodByTreeMapConstructor() {
        // Keep this comment
        return new TreeMap<>();
    }

    public Map<String, Integer> replaceGWTMethodByLinkedHashMapConstructor() {
        // Keep this comment
        return new LinkedHashMap<>();
    }

    public Map<String, Integer> replaceGWTMethodByIdentityHashMapConstructor() {
        // Keep this comment
        return new IdentityHashMap<>();
    }

    public Map<DayOfWeek, Integer> replaceGWTMethodByEnumMapConstructor() {
        // Keep this comment
        return new EnumMap<>(DayOfWeek.class);
    }

    public List<Date> replaceGuavaMethodByListConstructor() {
        // Keep this comment
        return new ArrayList<>();
    }

    public Map<String, Observer> replaceGuavaMethodByMapConstructor() {
        // Keep this comment
        return new HashMap<>();
    }

    public List<String> doNotReplaceWithParameters() {
        return Lists.newArrayList("foo", "bar");
    }
}
