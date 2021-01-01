/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Comparator;

import com.google.common.primitives.Doubles;

public class ComparisonSample implements Comparator<Double> {
    public boolean refactorComparableComparingToZero() {
        boolean b = true;
        final String s = "";

        // Valid, do no change these ones
        b &= s.compareTo("smaller") < 0;
        b &= s.compareTo("smaller") <= 0;
        b &= s.compareTo("equal") == 0;
        b &= s.compareTo("different") != 0;
        b &= s.compareTo("greater") >= 0;
        b &= s.compareTo("greater") > 0;
        b &= s.compareToIgnoreCase("equal") == 0;

        // Invalid, refactor them
        b &= s.compareTo("smaller") < 0;
        b &= s.compareTo("greater") >= 0;
        b &= s.compareTo("smaller") <= 0;
        b &= s.compareTo("greater") > 0;
        b &= s.compareToIgnoreCase("greater") > 0;
        b &= s.compareTo("smaller") < 0;
        b &= s.compareTo("greater") >= 0;
        b &= s.compareTo("smaller") <= 0;
        b &= s.compareTo("greater") > 0;
        b &= s.compareToIgnoreCase("greater") > 0;

        return b;
    }

    public boolean refactorComparatorComparingToZero(Comparator<String> comparator) {
        boolean b = true;
        final String s = "";

        // Valid, do no change these ones
        b &= comparator.compare(s, "smaller") < 0;
        b &= comparator.compare(s, "smaller") <= 0;
        b &= comparator.compare(s, "equal") == 0;
        b &= comparator.compare(s, "different") != 0;
        b &= comparator.compare(s, "greater") >= 0;
        b &= comparator.compare(s, "greater") > 0;

        // Invalid, refactor them
        b &= comparator.compare(s, "smaller") < 0;
        b &= comparator.compare(s, "greater") >= 0;
        b &= comparator.compare(s, "smaller") <= 0;
        b &= comparator.compare(s, "greater") > 0;
        b &= comparator.compare(s, "smaller") < 0;
        b &= comparator.compare(s, "greater") >= 0;
        b &= comparator.compare(s, "smaller") <= 0;
        b &= comparator.compare(s, "greater") > 0;

        return b;
    }

    public boolean doNotRefactorLocalComparingToZero() {
        boolean b = true;
        final Double s = 123d;

        b &= compare(s, 100d) < 100;
        b &= compare(s, 100d) <= 100;
        b &= compare(s, 123d) == 100;
        b &= compare(s, 321d) != 100;
        b &= compare(s, 200d) >= 100;
        b &= compare(s, 200d) > 100;

        b &= compare(s, 100d) == 99;
        b &= compare(s, 200d) != 99;
        b &= compare(s, 100d) != 101;
        b &= (compare(s, 200d)) == 101;
        b &= 99 == (compare(s, 100d));
        b &= 99 != compare(s, 200d);
        b &= 101 != compare(s, 100d);
        b &= 101 == compare(s, 200d);

        return b;
    }

    @Override
    public int compare(Double o1, Double o2) {
        return Doubles.compare(o1, o2) + 100;
    }
}
