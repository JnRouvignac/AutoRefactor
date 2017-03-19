/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

public class CollectionSample {

    public void replaceNewNoArgsAssignmentThenAddAll(List<String> col, List<String> output) {
        // Keep this comment
        output = new ArrayList<String>();
        output.addAll(col);
    }

    public void doNotReplaceStackCtor(List<String> col, List<String> output) {
        output = new Stack<String>();
        output.addAll(col);
    }

    public List<String> replaceNewNoArgsThenAddAll(List<String> col) {
        // Keep this comment
        final List<String> output = new ArrayList<String>();
        output.addAll(col);
        return output;
    }

    public List<String> replaceNewOneArgThenAddAll(List<String> col) {
        // Keep this comment
        final List<String> output = new ArrayList<String>(0);
        output.addAll(col);
        return output;
    }

    public List<String> replaceNewCollectionSizeThenAddAll(List<String> col) {
        // Keep this comment
        final List<String> output = new ArrayList<String>(col.size());
        output.addAll(col);
        return output;
    }

    public List<Object> doNotReplaceNewThenAddAllIncompatibleTypes(List<String> col) {
        final List<Object> output = new ArrayList<>();
        output.addAll(col);
        return output;
    }

    public Object replaceNewThenAddAllParameterizedType(Map<String, String> map) {
        // Keep this comment
        List<Entry<String, String>> output = new ArrayList<Entry<String, String>>();
        output.addAll(map.entrySet());
        return output;
    }
}
