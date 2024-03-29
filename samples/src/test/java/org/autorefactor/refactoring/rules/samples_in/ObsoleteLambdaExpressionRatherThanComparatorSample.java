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
package org.autorefactor.refactoring.rules.samples_in;

import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Locale;

public class ObsoleteLambdaExpressionRatherThanComparatorSample {
    public List<Date> useMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                return o1.toString().compareTo(o2.toString());
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useReversedMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                return o2.toString().compareTo(o1.toString());
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useNegatedMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                return -o1.toString().compareTo(o2.toString());
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> useTypedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator<File> comparator = new Comparator<File>() {

            @Override
            public int compare(File f1, File f2) {
                return f1.separator.compareTo(f2.separator);
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> useUntypedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator comparator = new Comparator<File>() {

            @Override
            public int compare(File f1, File f2) {
                return f1.separator.compareTo(f2.separator);
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> useReversedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator<File> comparator = new Comparator<File>() {

            @Override
            public int compare(File f1, File f2) {
                return f2.separator.compareTo(f1.separator);
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> o1.toString().compareTo(o2.toString());
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByReversedMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> o2.toString().compareTo(o1.toString());
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByNegatedMethodRef(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> -o1.toString().compareTo(o2.toString());
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> replaceLambdaByTypedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator<File> comparator = (f1, f2) -> f1.separator.compareTo(f2.separator);
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<File> replaceLambdaByReversedLambdaExpression(List<File> listToSort) {
        // Keep this comment
        Comparator<File> comparator = (f1, f2) -> f2.separator.compareTo(f1.separator);
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useMethodRefNullFirst(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                if (o1 != null) {
                    if (o2 != null) {
                        return o1.toString().compareTo(o2.toString());
                    }

                    return 1;
                } else if (o2 != null) {
                    return -1;
                } else {
                    return 0;
                }
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useMethodRefNullLast(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                if (o1 != null) {
                    if (null != o2) {
                        return o1.toString().compareTo(o2.toString());
                    } else {
                        return -10;
                    }
                } else {
                    return (o2 == null) ? 0 : 20;
                }
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useReversedMethodRefNullFirst(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                if (o1 != null)
                    if (null == o2)
                        return 123;
                     else
                        return o2.toString().compareTo(o1.toString());

                return (o2 == null) ? 0 : -123;
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> useReversedMethodRefNullLast(List<Date> listToSort) {
        // Keep this comment
        Collections.sort(listToSort, new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                if (o1 != null) {
                    if (null == o2) {
                        return -10;
                    } else {
                        return Long.compare(o2.getTime(), o1.getTime());
                    }
                }

                return (o2 == null) ? 0 : 20;
            }

        });

        return listToSort;
    }

    public List<Date> useMethodRefWithNegation(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {
            @Override
            public int compare(Date o1, Date o2) {
                if (!(o1 != null)) {
                    if (o2 != null) {
                        return -1;
                    } else {
                        return 0;
                    }
                } else {
                    if (o2 != null) {
                        return -o1.toString().compareTo(o2.toString());
                    }

                    return 1;
                }
            }
        };
        listToSort.sort(comparator);

        return listToSort;
    }

    public List<Date> useMethodRefUnorderedCondition(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                if (o2 != null) {
                    if (o1 != null) {
                        return o1.toString().compareTo(o2.toString());
                    }

                    return -1;
                } else if (o1 != null) {
                    return 1;
                } else {
                    return 0;
                }
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByMethodRefNullFirst(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> {
            if (o1 != null) {
                if (o2 != null) {
                    return o1.toString().compareTo(o2.toString());
                }

                return 1;
            } else if (o2 != null) {
                return -1;
            } else {
                return 0;
            }
        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByMethodRefNullLast(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> {
            if (o1 != null) {
                if (null != o2) {
                    return o1.toString().compareTo(o2.toString());
                } else {
                    return -10;
                }
            } else {
                return (o2 == null) ? 0 : 20;
            }
        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByReversedMethodRefNullFirst(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> {
            if (o1 != null)
                if (null == o2)
                    return 123;
                 else
                    return o2.toString().compareTo(o1.toString());

            return (o2 == null) ? 0 : -123;
        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByReversedMethodRefNullLast(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator= (o1, o2) -> {
            if (o1 != null) {
                if (null == o2) {
                    return -10;
                } else {
                    return Long.compare(o2.getTime(), o1.getTime());
                }
            }

            return (o2 == null) ? 0 : 20;
        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByMethodRefWithNegation(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> {
            if (!(o1 != null)) {
                if (o2 != null) {
                    return -1;
                } else {
                    return 0;
                }
            } else {
                if (o2 != null) {
                    return -o1.toString().compareTo(o2.toString());
                }

                return 1;
            }
        };
        listToSort.sort(comparator);

        return listToSort;
    }

    public List<Date> replaceLambdaByMethodRefUnorderedCondition(List<Date> listToSort) {
        // Keep this comment
        Comparator<Date> comparator = (o1, o2) -> {
            if (o2 != null) {
                if (o1 != null) {
                    return o1.toString().compareTo(o2.toString());
                }

                return -1;
            } else if (o1 != null) {
                return 1;
            } else {
                return 0;
            }
        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<Date> doNotUseMethodRefWithWeirdBehavior(List<Date> listToSort) {
        Comparator<Date> comparator = new Comparator<Date>() {

            @Override
            public int compare(Date o1, Date o2) {
                if (o1 != null) {
                    if (o2 != null) {
                        return o1.toString().compareTo(o2.toString());
                    } else {
                        return 1;
                    }
                } else if (o2 != null) {
                    return -1;
                } else {
                    return 100;
                }
            }

        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<String> doNotUseMethodRef(List<String> listToSort) {
        Comparator<String> comparator = new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.toLowerCase(Locale.ENGLISH).compareTo(o2.toLowerCase(Locale.ENGLISH));
            }
        };
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

    public List<Date> doNotReplaceLambdaByUseMethodRefWithWeirdBehavior(List<Date> listToSort) {
        Comparator<Date> comparator = (o1, o2) -> {
            if (o1 != null) {
                if (o2 != null) {
                    return o1.toString().compareTo(o2.toString());
                } else {
                    return 1;
                }
            } else if (o2 != null) {
                return -1;
            } else {
                return 100;
            }
        };
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public List<String> doNotReplaceLambdaByUseMethodRef(List<String> listToSort) {
        Comparator<String> comparator = (o1, o2) -> o1.toLowerCase(Locale.ENGLISH).compareTo(o2.toLowerCase(Locale.ENGLISH));
        Collections.sort(listToSort, comparator);

        return listToSort;
    }

    public Comparator<Date> doNotReplaceLambdaByRefactorComparisonWithoutCompareToMethod(List<Date> listToSort) {
        Comparator<Date> comparator = (o1, o2) -> (int) (o1.getTime() - o2.getTime());

        return comparator;
    }

    public Comparator<Date> doNotReplaceLambdaByRemoveSecondaryMethod(List<Date> listToSort) {
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
