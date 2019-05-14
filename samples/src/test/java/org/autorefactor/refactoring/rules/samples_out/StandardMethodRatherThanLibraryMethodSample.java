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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.Date;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;

public class StandardMethodRatherThanLibraryMethodSample {
    public String replaceGoogleMethods(Date object1, Date object2) {
        // Keep comment
        Date notNullObject1 = java.util.Objects.requireNonNull(object1);

        // Comment 2
        Date notNullObject2 = java.util.Objects.requireNonNull(object2, "object2");

        // Keep this comment
        boolean b1 = java.util.Objects.equals(notNullObject1, notNullObject2);

        // Keep this comment too
        int i2 = java.util.Objects.hash(object1, object2);

        return String.valueOf(b1) + i2;
    }
}
