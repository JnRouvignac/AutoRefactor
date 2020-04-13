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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

public class BooleanEqualsRatherThanNullCheckSample {

    private static int staticField = 0;

    public void replaceNullCheck(Boolean b1) {
        // Keep this comment
        boolean newBoolean1 = Boolean.TRUE.equals(b1);
        boolean newBoolean2 = !Boolean.FALSE.equals(b1);
        boolean newBoolean3 = Boolean.FALSE.equals(b1);
        boolean newBoolean4 = !Boolean.TRUE.equals(b1);

        boolean newBoolean5 = Boolean.TRUE.equals(b1);
        boolean newBoolean6 = !Boolean.FALSE.equals(b1);
        boolean newBoolean7 = Boolean.FALSE.equals(b1);
        boolean newBoolean8 = !Boolean.TRUE.equals(b1);

        boolean newBoolean9 = Boolean.TRUE.equals(b1);
        boolean newBoolean10 = !Boolean.FALSE.equals(b1);
        boolean newBoolean11 = Boolean.FALSE.equals(b1);
        boolean newBoolean12 = !Boolean.TRUE.equals(b1);

        boolean newBoolean13 = Boolean.TRUE.equals(b1);
        boolean newBoolean14 = !Boolean.FALSE.equals(b1);
        boolean newBoolean15 = Boolean.FALSE.equals(b1);
        boolean newBoolean16 = !Boolean.TRUE.equals(b1);
    }

    private static class SideEffect {
        private static Boolean isActive() {
            staticField++;
            return Boolean.TRUE;
        }
    }

    public void doNotReplaceNullCheckWithMethods() {
        boolean newBoolean1 = SideEffect.isActive() != null && SideEffect.isActive();
        boolean newBoolean2 = SideEffect.isActive() == null || SideEffect.isActive();
        boolean newBoolean3 = SideEffect.isActive() != null && !SideEffect.isActive();
        boolean newBoolean4 = SideEffect.isActive() == null || !SideEffect.isActive();
    }
}
