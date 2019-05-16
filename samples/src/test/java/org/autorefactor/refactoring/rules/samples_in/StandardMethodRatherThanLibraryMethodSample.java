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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.Date;

import org.apache.commons.lang3.Validate;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;

public class StandardMethodRatherThanLibraryMethodSample {
    public String replaceGoogleMethods(Date object1, Date object2) {
        // Keep comment
        Date notNullObject1 = Preconditions.checkNotNull(object1);

        // Comment 2
        Date notNullObject2 = Preconditions.checkNotNull(object2, "object2");

        // Double check
        Date justToBeSure1 = Validate.notNull(notNullObject1);
        Date justToBeSure2 = Validate.notNull(notNullObject2, "you never know");

        // Keep questioning...
        Date justToBeSure3 = Validate.notNull(justToBeSure1, "message:%s", justToBeSure2);
        Date justToBeSure4 = Validate.notNull(notNullObject2, "you never know:%s - %s", justToBeSure3, notNullObject1);

        // Keep this comment
        boolean b1 = Objects.equal(justToBeSure2, justToBeSure4);

        // Keep this comment too
        int i2 = Objects.hashCode(object1, object2);

        return String.valueOf(b1) + i2;
    }
}
