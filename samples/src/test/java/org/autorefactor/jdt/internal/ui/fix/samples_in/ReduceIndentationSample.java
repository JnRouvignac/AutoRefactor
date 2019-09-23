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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

public class ReduceIndentationSample {
    private Date j = new Date();

    public int refactor(List<Date> events) {
        // Keep this comment
        if (events == null) {
            throw new NullPointerException("Events null");
        } else if (events.isEmpty()) {
            throw new IllegalArgumentException("At least one event must be present");
        } else {
            Date firstEvent = events.get(0);
            if (firstEvent.before(new Date())) {
                throw new IllegalArgumentException("The first event should not be in the past");
            } else {
                Calendar cal = Calendar.getInstance();
                cal.set(2019, Calendar.JULY, 10);
                if (events.contains(cal.getTime())) {
                    throw new IllegalArgumentException("2019-07-10 is forbidden");
                } else {
                    return events.size();
                }
            }
        }
    }

    public int refactorElse(int i) {
        // Keep this comment
        if (i > 0) {
            return 0;
        } else {
            i = i + 1;
        }

        return i;
    }

    public int refactorThen(int i) {
        // Keep this comment
        if (i > 0) {
            i = i + 1;
        } else {
            return 0;
        }

        return i;
    }

    public int refactorIndentation(int i) {
        // Keep this comment
        if (i > 0) {
            return 0;
        } else {
            return 1;
        }
    }

    public int refactorElseIf(int i) {
        // Keep this comment
        if (i < 0) {
            return -1;
        } else if (i > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    public int refactorElseIf(int i, List<Integer> integers, boolean isVisible) {
        // Keep this comment
        if (i < 0) {
            for (Integer integer : integers) {
                if (integer < 0) {
                    System.out.println("is negative");
                } else {
                    System.out.println("is not negative");
                }
            }
            return 51;
        } else if (i > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    public int refactorGreatestIndentation(boolean isActive, boolean isVisible) {
        // Keep this comment
        if (isActive) {
            if (isVisible) {
                return 0;
            } else {
                return 1;
            }
        } else {
            return 2;
        }
    }

    public int doNotRefactorWithNameConflict(int i) {
        if (i > 0) {
            return 0;
        } else {
            int j = 123;
            i = i + j;
        }

        int j = 321;

        return i + j;
    }

    public int refactorWithoutNameConflict(int i) {
        System.out.println("Today: " + j);

        // Keep this comment
        if (i > 0) {
            return 0;
        } else {
            int j = 123;
            i = i + j;
        }

        return i;
    }

    public int refactorWithThrow(int i) {
        // Keep this comment
        if (i > 0) {
            throw new IllegalArgumentException("Positive argument");
        } else {
            i = i + 1;
        }

        return i;
    }

    public void refactorWithContinue(List<Integer> integers) {
        for (Integer integer : integers) {
            // Keep this comment
            if (integer > 0) {
                continue;
            } else {
                System.out.println(integer);
            }
        }
    }

    public void refactorWithBreak(List<Integer> integers) {
        for (Integer integer : integers) {
            // Keep this comment
            if (integer > 0) {
                break;
            } else {
                System.out.println(integer);
            }
        }
    }
}
