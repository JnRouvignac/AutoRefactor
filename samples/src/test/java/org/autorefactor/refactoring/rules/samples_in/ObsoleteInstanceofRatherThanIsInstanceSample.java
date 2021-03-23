/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice TIERCELIN - initial API and implementation
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

public class ObsoleteInstanceofRatherThanIsInstanceSample {
    public boolean useInstanceof(Object o) {
        // Keep this comment
        return String.class.isInstance(o);
    }

    public boolean useInstanceofOnComplexType(Object o) {
        // Keep this comment
        return String[].class.isInstance(o);
    }

    public boolean useInstanceofOnQualifiedType(Object o) {
        // Keep this comment
        return java.util.Date.class.isInstance(o);
    }

    public boolean doNotUseInstanceofOnPrimitive(Object o) {
        return int.class.isInstance(o);
    }

    public boolean doNotUseInstanceofOnDynamicClass(Object o, Class<?> clazz) {
        return clazz.isInstance(o);
    }

    public boolean doNotUseInstanceofOnOtherMethod(Object o) {
        return String.class.equals(o);
    }
}