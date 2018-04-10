/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016-2017 Fabrice Tiercelin - Annoying remaining loop variable occurrence
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

import java.util.Collection;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

public class IsEmptyRatherThanSizeSample {

    public void replaceChecksOnSize(Collection<Integer> col) {
        // Keep this comment
        System.out.println(col.size() > 0);
        System.out.println(col.size() >= 0);
        System.out.println(col.size() == 0);
        System.out.println(col.size() != 0);
        System.out.println(col.size() <= 0);
        System.out.println(col.size() < 0);
        System.out.println(col.size() < 1);
        System.out.println(col.size() >= 1);

        System.out.println(0 < col.size());
        System.out.println(0 <= col.size());
        System.out.println(0 == col.size());
        System.out.println(0 != col.size());
        System.out.println(0 >= col.size());
        System.out.println(0 > col.size());
        System.out.println(1 > col.size());
        System.out.println(1 <= col.size());
    }

    public void doNotRefactorChecksOtherThanEmptiness(Collection<Long> col) {
        System.out.println(col.size() == 1);
        System.out.println(col.size() != 1);
        System.out.println(col.size() > 1);
        System.out.println(col.size() <= 1);
        System.out.println(col.size() >= 2);

        System.out.println(1 == col.size());
        System.out.println(1 != col.size());
        System.out.println(1 < col.size());
        System.out.println(1 >= col.size());
        System.out.println(2 <= col.size());
    }

    public void replaceChecksOnSize(Map<Short, Observable> map) {
        // Keep this comment
        System.out.println(map.size() > 0);
        System.out.println(map.size() >= 0);
        System.out.println(map.size() == 0);
        System.out.println(map.size() != 0);
        System.out.println(map.size() <= 0);
        System.out.println(map.size() < 0);
        System.out.println(map.size() < 1);
        System.out.println(map.size() >= 1);

        System.out.println(0 < map.size());
        System.out.println(0 <= map.size());
        System.out.println(0 == map.size());
        System.out.println(0 != map.size());
        System.out.println(0 >= map.size());
        System.out.println(0 > map.size());
        System.out.println(1 > map.size());
        System.out.println(1 <= map.size());
    }

    public void doNotRefactorChecksOtherThanEmptiness(Map<Byte, Observer> map) {
        System.out.println(map.size() == 1);
        System.out.println(map.size() != 1);
        System.out.println(map.size() > 1);
        System.out.println(map.size() <= 1);
        System.out.println(map.size() >= 2);

        System.out.println(1 == map.size());
        System.out.println(1 != map.size());
        System.out.println(1 < map.size());
        System.out.println(1 >= map.size());
        System.out.println(2 <= map.size());
    }

    public void replaceChecksOnSize(String text) {
        // Keep this comment
        System.out.println(text.length() > 0);
        System.out.println(text.length() >= 0);
        System.out.println(text.length() == 0);
        System.out.println(text.length() != 0);
        System.out.println(text.length() <= 0);
        System.out.println(text.length() < 0);
        System.out.println(text.length() < 1);
        System.out.println(text.length() >= 1);

        System.out.println(0 < text.length());
        System.out.println(0 <= text.length());
        System.out.println(0 == text.length());
        System.out.println(0 != text.length());
        System.out.println(0 >= text.length());
        System.out.println(0 > text.length());
        System.out.println(1 > text.length());
        System.out.println(1 <= text.length());
    }

    public void doNotRefactorChecksOtherThanEmptiness(String text) {
        System.out.println(text.length() == 1);
        System.out.println(text.length() != 1);
        System.out.println(text.length() > 1);
        System.out.println(text.length() <= 1);
        System.out.println(text.length() >= 2);

        System.out.println(1 == text.length());
        System.out.println(1 != text.length());
        System.out.println(1 < text.length());
        System.out.println(1 >= text.length());
        System.out.println(2 <= text.length());
    }
}
