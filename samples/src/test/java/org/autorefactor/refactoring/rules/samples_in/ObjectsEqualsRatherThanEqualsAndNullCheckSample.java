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

import java.util.Map;
import java.util.Observable;

public class ObjectsEqualsRatherThanEqualsAndNullCheckSample {
    private Map<Integer, String> textById;
    private Observable anObservable;
    private String aText;

    /* (non-Javadoc)
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ObjectsEqualsRatherThanEqualsAndNullCheckSample other = (ObjectsEqualsRatherThanEqualsAndNullCheckSample) obj;
        if (aText == null) {
            if (other.aText != null)
                return false;
        } else if (!aText.equals(other.aText))
            return false;
        if (anObservable == null) {
            if (other.anObservable != null)
                return false;
        } else if (!anObservable.equals(other.anObservable))
            return false;
        if (textById == null) {
            if (other.textById != null)
                return false;
        } else if (!textById.equals(other.textById))
            return false;
        return true;
    }
}
