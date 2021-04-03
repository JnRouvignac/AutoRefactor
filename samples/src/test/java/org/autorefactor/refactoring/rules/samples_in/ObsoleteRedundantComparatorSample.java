/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2021 Fabrice TIERCELIN - initial API and implementation
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

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.function.Function;

public class ObsoleteRedundantComparatorSample {
    public List<Date> removeComparatorClass(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o1.compareTo(o2);
            }

        });

        return listToSort;
    }

    public Date removeComparatorOnMax(List<Date> listToSort) {
        // Keep this comment
        return Collections.max(listToSort, new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o1.compareTo(o2);
            }

        });
    }

    public Date removeComparatorOnMin(List<Date> listToSort) {
        // Keep this comment
        return Collections.min(listToSort, new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o1.compareTo(o2);
            }

        });
    }

    public List<Long> removeLambdaExpression(List<Long> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, (Long o1, Long o2) -> o1.compareTo(o2));

        return listToSort;
    }

    public List<String> removeLambdaBody(List<String> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, (String o1, String o2) -> {
            return o1.compareTo(o2);
        });

        return listToSort;
    }

    public List<Double> removeUntypedLambda(List<Double> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, (o1, o2) -> {
            return o1.compareTo(o2);
        });

        return listToSort;
    }

    public List<Integer> removeComparatorOnPrimitive(List<Integer> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, (o1, o2) -> {
            return Integer.compare(o1, o2);
        });

        return listToSort;
    }

    public List<Date> removeIdentityFunction(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, Comparator.comparing(Function.identity()));

        return listToSort;
    }

    public List<Date> removeComparingLambda(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, Comparator.comparing((Date d) -> d));

        return listToSort;
    }

    public List<Date> removeComparingBody(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, Comparator.comparing((Date d) -> {
            return d;
        }));

        return listToSort;
    }

    public List<Date> removeUntypedParameter(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, Comparator.comparing(d -> {
            return d;
        }));

        return listToSort;
    }

    public List<Date> removeNaturalOrder(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, Comparator.naturalOrder());

        return listToSort;
    }

    public List<Date> removeNullComparator(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, null);

        return listToSort;
    }

    public List<BigDecimal> removeOpposedComparatorClass(List<BigDecimal> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, new Comparator<BigDecimal>() {
            @Override
            public int compare(BigDecimal o1, BigDecimal o2) {
                return -o2.compareTo(o1);
            }

        });

        return listToSort;
    }

    public List<Date> removeTwiceReversedComparatorClass(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o2.compareTo(o1);
            }

        }.reversed());

        return listToSort;
    }

    public List<Date> refactoreSortedList(List<Date> listToSort) {
        // Keep this comment
        listToSort.sort(new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o2.compareTo(o1);
            }

        }.reversed());

        return listToSort;
    }

    public List<String> doNotRemoveComparatorWithoutCompareToMethod(List<String> listToSort) {
        Collections.sort(listToSort, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.compareToIgnoreCase(o2);
            }

        });

        return listToSort;
    }

    public List<String> doNotRemoveComparatorWithOtherStatement(List<String> listToSort) {
        Collections.sort(listToSort, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                System.out.println("Don't lose me!");
                return o1.compareTo(o2);
            }

        });

        return listToSort;
    }

    public List<String> doNotRemoveLambdaWithOtherStatement(List<String> listToSort) {
        Collections.sort(listToSort, (String o1, String o2) -> {
            System.out.println("Don't lose me!");
            return o1.compareTo(o2);
        });

        return listToSort;
    }

    public List<Date> doNotRemoveReservedComparatorClass(List<Date> listToSort) {
        Collections.sort(listToSort, new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o2.compareTo(o1);
            }

        });

        return listToSort;
    }

    public List<Date> doNotRemoveReservedComparatorOnMethod(List<Date> listToSort) {
        Collections.sort(listToSort, new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                return o1.toString().compareTo(o2.toString());
            }

        });

        return listToSort;
    }

    public List<Date> doNotRemoveReservedLambdaExpression(List<Date> listToSort) {
        Collections.sort(listToSort, (Date o1, Date o2) -> o2.compareTo(o1));

        return listToSort;
    }

    public List<Date> doNotRemoveReservedLambdaBody(List<Date> listToSort) {
        Collections.sort(listToSort, (Date o1, Date o2) -> {
            return o2.compareTo(o1);
        });

        return listToSort;
    }

    public List<Date> doNotRemoveReservedUntypedLambda(List<Date> listToSort) {
        Collections.sort(listToSort, (o1, o2) -> {
            return o2.compareTo(o1);
        });

        return listToSort;
    }

    public List<Date> doNotRemoveComparatorOnSpecialMethod(List<Date> listToSort) {
        Collections.sort(listToSort, Comparator.comparing(Date::toString));

        return listToSort;
    }

    public List<Date> doNotRemoveReservedIdentityFunction(List<Date> listToSort) {
        Collections.sort(listToSort, Comparator.<Date, Date>comparing(Function.identity()).reversed());

        return listToSort;
    }

    private class NonComparable {
        public int compareTo(Object anotherObject) {
            return 42;
        }
    }

    public List<NonComparable> doNotRemoveComparatorOnNonComparable(List<NonComparable> listToSort) {
        Collections.sort(listToSort, new Comparator<NonComparable>() {
            @Override
            public int compare(NonComparable o1, NonComparable o2) {
                return o1.compareTo(o2);
            }

        });

        return listToSort;
    }

    public List<NonComparable> doNotRemoveLambdaOnNonComparable(List<NonComparable> listToSort) {
        Collections.sort(listToSort, (NonComparable o1, NonComparable o2) -> {
            return o1.compareTo(o2);
        });

        return listToSort;
    }

    public List<NonComparable> doNotRemoveMethodRefOnNonComparable(List<NonComparable> listToSort) {
        Collections.sort(listToSort, NonComparable::compareTo);

        return listToSort;
    }
}
