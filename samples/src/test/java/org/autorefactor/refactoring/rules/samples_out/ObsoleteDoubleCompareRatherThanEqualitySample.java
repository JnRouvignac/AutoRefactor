/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
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

public class ObsoleteDoubleCompareRatherThanEqualitySample {
    public boolean rewriteThoseprimitiveDoubleComparisons(double primitiveDouble1, double primitiveDouble2) {
        boolean b;

        // Keep this comment
        b = Double.compare(primitiveDouble1, primitiveDouble2) == 0;
        b = Double.compare(primitiveDouble1, primitiveDouble2) != 0;
        b = Double.compare(primitiveDouble1, primitiveDouble2) < 0;
        b = Double.compare(primitiveDouble1, primitiveDouble2) > 0;
        b = Double.compare(primitiveDouble1, primitiveDouble2) <= 0;
        b = Double.compare(primitiveDouble1, primitiveDouble2) >= 0;

        return b;
    }

    public boolean rewriteThoseObjectDoubleComparisons(Double objectDouble1, Double objectDouble2) {
        boolean b;

        // Keep this comment
        b = Double.compare(objectDouble1, objectDouble2) == 0;
        b = Double.compare(objectDouble1, objectDouble2) != 0;
        b = Double.compare(objectDouble1, objectDouble2) < 0;
        b = Double.compare(objectDouble1, objectDouble2) > 0;
        b = Double.compare(objectDouble1, objectDouble2) <= 0;
        b = Double.compare(objectDouble1, objectDouble2) >= 0;

        return b;
    }

    public boolean rewriteThoseDoubleComparisons(double primitiveDouble, Double objectDouble) {
        boolean b;

        // Keep this comment
        b = Double.compare(primitiveDouble, objectDouble) == 0;
        b = Double.compare(primitiveDouble, objectDouble) != 0;
        b = Double.compare(primitiveDouble, objectDouble) < 0;
        b = Double.compare(primitiveDouble, objectDouble) > 0;
        b = Double.compare(primitiveDouble, objectDouble) <= 0;
        b = Double.compare(primitiveDouble, objectDouble) >= 0;

        return b;
    }

    public boolean rewriteThoseExpression(double primitiveDouble, Double objectDouble) {
        boolean b;

        // Keep this comment
        b = Double.compare(primitiveDouble, objectDouble * 2) == 0;
        b = Double.compare(primitiveDouble, objectDouble * 2) != 0;
        b = Double.compare(primitiveDouble, objectDouble * 2) < 0;
        b = Double.compare(primitiveDouble, objectDouble * 2) > 0;
        b = Double.compare(primitiveDouble, objectDouble * 2) <= 0;
        b = Double.compare(primitiveDouble, objectDouble * 2) >= 0;

        return b;
    }
}
