/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice TIERCELIN - initial API and implementation
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

import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

public class LambdaExpressionRatherThanComparatorSample {
    public List<Date> useMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = Comparator.comparing(Date::toString);
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useReversedMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = Comparator.comparing(Date::toString).reversed();
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> useTypedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator<File> comparator = Comparator.comparing(f1 -> f1.separator);
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> useUntypedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator comparator = Comparator.comparing((File f1) -> f1.separator);
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> useReversedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator<File> comparator = Comparator.comparing((File f1) -> f1.separator).reversed();
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public Comparator<Date> doNotRefactorComparisonWithoutCompareToMethod(List<Date> listToSort) {
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                return (int) (o1.getTime() - o2.getTime());
            }
        };

        return comparator;
    }

    public Comparator<Date> doNotRemoveSecondaryMethod(List<Date> listToSort) {
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                return o1.toString().compareTo(o2.toString());
            }

            @Override
            public String toString() {
                return "Compare formatted dates";
            }
        };

        return comparator;
    }
}