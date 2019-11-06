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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

public class ReduceIndentationSample {
    private Date j = new Date();

    public int refactor(List<Date> events) {
        // Keep this comment
        if (events == null) {
            throw new NullPointerException("Events null");
        }
        if (events.isEmpty()) {
            throw new IllegalArgumentException("At least one event must be present");
        }
        Date firstEvent = events.get(0);
        if (firstEvent.before(new Date())) {
            throw new IllegalArgumentException("The first event should not be in the past");
        }
        Calendar cal = Calendar.getInstance();
        cal.set(2019, Calendar.JULY, 10);
        if (events.contains(cal.getTime())) {
            throw new IllegalArgumentException("2019-07-10 is forbidden");
        }
        return events.size();
    }

    public int refactorElse(int i) {
        // Keep this comment
        if (i > 0) {
            // Keep this comment too
            return 0;
        }
        // Keep this comment also
        i = i + 1;

        return i;
    }

    public int refactorThen(int i) {
        // Keep this comment
        if (!(i > 0)) {
            // Keep this comment also
            return 0;
        }
        // Keep this comment too
        i = i + 1;

        return i;
    }

    public int refactorIndentation(int i) {
        // Keep this comment
        if (i > 0) {
            // Keep this comment too
            return 0;
        }
        // Keep this comment also
        return 1;
    }

    public int refactorElseIf(int i) {
        // Keep this comment
        if (i < 0) {
            // Keep this comment too
            return -1;
        }
        if (i > 0) {
            // Keep this comment also
            return 1;
        }
        // Keep this comment again
        return 0;
    }

    public int refactorElseIf(int i, List<Integer> integers, boolean isVisible) {
        // Keep this comment
        if (i < 0) {
            // Keep this comment too
            for (Integer integer : integers) {
                if (integer < 0) {
                    System.out.println("is negative");
                } else {
                    System.out.println("is not negative");
                }
            }
            return 51;
        }
        if (i > 0) {
            // Keep this comment also
            return 1;
        }
        // Keep this comment again
        return 0;
    }

    public int refactorThenInUnbrackettedForLoop(int[] integers) {
        for (int integer : integers) {
            if (!(integer > 0)) {
                // Keep this comment
                return 0;
            }
            // Keep this comment too
            integer = integer + 1;
        }

        return -1;
    }

    public int refactorElseInUnbrackettedForLoop(double[] reals) {
        for (double real : reals) {
            if (real > 0) {
                // Keep this comment
                return 0;
            }
            // Keep this comment too
            real = real + 1;
            System.out.println("New value: " + real);
        }

        return -1;
    }

    public int refactorElseInSwitch(int discriminant, boolean isVisible) {
        switch (discriminant) {
        case 0:
            if (isVisible) {
                // Keep this comment
                return 0;
            }
            // Keep this comment too
            discriminant = discriminant + 1;
            System.out.println("New value: " + discriminant);
        }

        return -1;
    }

    public int refactorElseInTry(int discriminant, boolean isVisible) {
        try {
            if (isVisible) {
                // Keep this comment
                return 0;
            }
            // Keep this comment too
            discriminant = discriminant + 1;
            System.out.println("New value: " + discriminant);
        } finally {
            System.out.println("Finally");
        }

        return -1;
    }

    public int refactorElseInCatch(int discriminant, boolean isVisible) {
        try {
            System.out.println("Very dangerous code");
        } catch (Exception e) {
            if (isVisible) {
                // Keep this comment
                return 0;
            }
            // Keep this comment too
            discriminant = discriminant + 1;
            System.out.println("New value: " + discriminant);
        }

        return -1;
    }

    public int refactorElseInFinally(int discriminant, boolean isVisible) {
        try {
            System.out.println("Very dangerous code");
        } finally {
            if (isVisible) {
                // Keep this comment
                return 0;
            }
            // Keep this comment too
            discriminant = discriminant + 1;
            System.out.println("New value: " + discriminant);
        }

        return -1;
    }

    public int refactorGreatestIndentation(boolean isActive, boolean isVisible) {
        // Keep this comment
        if (isActive) {
            // Keep this comment too
            if (isVisible) {
                return 0;
            }
            return 1;
        }
        // Keep this comment also
        return 2;
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
            // Keep this comment too
            return 0;
        }
        // Keep this comment also
        int j = 123;
        i = i + j;

        return i;
    }

    public int doNotRefactorWithoutNameConfusion(int i) {
        if (i > 0) {
            return 0;
        } else {
            int j = 123;
            i = i + j;
        }

        System.out.println("Today: " + j);

        return i;
    }

    public int refactorWithThrow(int i) {
        // Keep this comment
        if (i > 0) {
            // Keep this comment too
            throw new IllegalArgumentException("Positive argument");
        }
        // Keep this comment also
        i = i + 1;

        return i;
    }

    public void refactorWithContinue(List<Integer> integers) {
        for (Integer integer : integers) {
            // Keep this comment
            if (integer > 0) {
                // Keep this comment too
                continue;
            }
            // Keep this comment also
            System.out.println(integer);
        }
    }

    public void refactorWithBreak(List<Integer> integers) {
        for (Integer integer : integers) {
            // Keep this comment
            if (integer > 0) {
                // Keep this comment too
                break;
            }
            // Keep this comment also
            System.out.println(integer);
        }
    }
}
