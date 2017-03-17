/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial implementation
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

import java.util.List;

public class LazyLogicalRatherThanEagerSample {

    private static int staticField = 0;

    public void replaceOperatorWithPrimitiveTypes(boolean b1, boolean b2) {
        // Keep this comment
        boolean newBoolean1 = b1 & b2;
        boolean newBoolean2 = b1 | b2;
    }

    public void replaceOperatorWithWrappers(Boolean b1, Boolean b2) {
        // Keep this comment
        boolean newBoolean1 = b1 & b2;
        boolean newBoolean2 = b1 | b2;
    }

    public void doNotReplaceOperatorWithIntegers(int i1, int i2) {
        int newInteger1 = i1 & i2;
        int newInteger2 = i1 | i2;
    }

    public void replaceOperatorWithExpressions(int i1, int i2, int i3, int i4) {
        // Keep this comment
        boolean newBoolean1 = (i1 == i2) & (i3 != i4);
        boolean newBoolean2 = (i1 == i2) | (i3 != i4);
    }

    public void doNotReplaceOperatorWithMethods(List<String> myList) {
        boolean newBoolean1 = myList.remove("lorem") & myList.remove("ipsum");
        boolean newBoolean2 = myList.remove("lorem") | myList.remove("ipsum");
    }

    public void doNotReplaceOperatorWithIncrements(int i1, int i2, int i3, int i4) {
        boolean newBoolean1 = (i1 == i2) & (i3 != i4++);
        boolean newBoolean2 = (i1 == i2) & (i3 != ++i4);
        boolean newBoolean3 = (i1 == i2) & (i3 != i4--);
        boolean newBoolean4 = (i1 == i2) & (i3 != --i4);

        boolean newBoolean5 = (i1 == i2) | (i3 != i4++);
        boolean newBoolean6 = (i1 == i2) | (i3 != ++i4);
        boolean newBoolean7 = (i1 == i2) | (i3 != i4--);
        boolean newBoolean8 = (i1 == i2) | (i3 != --i4);
    }

    public void doNotReplaceOperatorWithAssignments(int i1, int i2, boolean b1, boolean b2) {
        boolean newBoolean1 = (i1 == i2) & (b1 = b2);
        boolean newBoolean2 = (i1 == i2) | (b1 = b2);
    }

    private class SideEffect {
        private SideEffect() {
            staticField++;
        }
    }

    public void doNotReplaceOperatorWithInstanciations(Boolean b1) {
        boolean newBoolean1 = b1 & new SideEffect() instanceof SideEffect;
        boolean newBoolean2 = b1 | new SideEffect() instanceof SideEffect;
    }
}
