/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.util.Date;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

public class AtomicObjectRatherThanMonoIndexArraySample {
    public static boolean useAtomicBoolean() {
        // Keep this comment
        AtomicBoolean booleanRef= new AtomicBoolean();
        // Keep this comment also
        Runnable runnable = () -> booleanRef.set(true);
        runnable.run();
        // Keep this comment too
        return booleanRef.get();
    }

    public static int useAtomicInteger() {
        // Keep this comment
        AtomicInteger intRef= new AtomicInteger();
        // Keep this comment also
        Runnable runnable = () -> intRef.set(42);
        runnable.run();
        // Keep this comment too
        return intRef.get();
    }

    public static int doNotRefactorIncrementalAssignment() {
        int[] intRef= new int[1];
        Runnable runnable = () -> intRef[0] += 42;
        runnable.run();
        return intRef[0];
    }

    public static int doNotRefactorDecrementalAssignment() {
        int[] intRef= new int[1];
        Runnable runnable = () -> intRef[0] -= 42;
        runnable.run();
        return intRef[0];
    }

    public static long useAtomicLong() {
        // Keep this comment
        AtomicLong longRef= new AtomicLong();
        // Keep this comment also
        Runnable runnable = () -> longRef.set(42);
        runnable.run();
        // Keep this comment too
        return longRef.get();
    }

    public static short doNotRefactorShortArray() {
        short[] shortRef= new short[1];
        Runnable runnable = () -> shortRef[0] = 42;
        runnable.run();
        return shortRef[0];
    }

    public static Date useAtomicReference() {
        // Keep this comment
        AtomicReference<Date> dateRef= new AtomicReference<>();
        // Keep this comment also
        Runnable runnable = () -> dateRef.set(new Date());
        runnable.run();
        // Keep this comment too
        return dateRef.get();
    }

    public static Long useAtomicLongObject() {
        // Keep this comment
        AtomicReference<Long> longRef = new AtomicReference<>();
        // Keep this comment also
        Runnable runnable = () -> longRef.set(Long.valueOf(0));
        runnable.run();
        // Keep this comment too
        return longRef.get();
    }

    public static Date useAtomicReferenceInAnonymousClass() {
        // Keep this comment
        AtomicReference<Date> dateRef= new AtomicReference<>();
        // Keep this comment also
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                dateRef.set(new Date());
            }
        };
        runnable.run();
        // Keep this comment too
        return dateRef.get();
    }

    public static Date doNotTouchUnknownPurpose() {
        Date[] dateRef= new Date[1];
        dateRef[0] = new Date();
        return dateRef[0];
    }

    public static void doNotTouchUnknownPurposeInLambda() {
        Runnable runnable = () -> {
            Date[] dateRef= new Date[1];
            dateRef[0] = new Date();
            System.out.println(dateRef[0]);
        };
    }

    public static Date doNotRefactorGreatArray() {
        Date[] dateRef= new Date[2];
        Runnable runnable = () -> dateRef[0] = new Date();
        runnable.run();
        return dateRef[0];
    }

    public static Date doNotRefactorEmptyArray() {
        Date[] dateRef= new Date[0];
        Runnable runnable = () -> dateRef[0] = new Date();
        runnable.run();
        return dateRef[0];
    }

    public static Date doNotRefactorWrongIndex() {
        Date[] dateRef= new Date[1];
        Runnable runnable = () -> dateRef[1] = new Date();
        runnable.run();
        return dateRef[0];
    }

    public static Date doNotRefactorAnotherWrongIndex() {
        Date[] dateRef= new Date[1];
        Runnable runnable = () -> dateRef[0] = new Date();
        runnable.run();
        return dateRef[1];
    }

    public static Date doNotRefactorSeveralDimensionArray() {
        Date[][] dateRef= new Date[1][1];
        Runnable runnable = () -> dateRef[0][0] = new Date();
        runnable.run();
        return dateRef[0][0];
    }

    public static Date useAtomicReferenceOnExtraDimension() {
        // Keep this comment
        AtomicReference<Date> dateRef= new AtomicReference<>();
        // Keep this comment also
        Runnable runnable = () -> dateRef.set(new Date());
        runnable.run();
        // Keep this comment too
        return dateRef.get();
    }

    public static Date[][] doNotRefactorSeveralDeclarations() {
        Date[][] dateRef= new Date[1][1], iAmHereToo= null;
        Runnable runnable = () -> dateRef[0][0] = new Date();
        runnable.run();
        System.out.println(dateRef[0]);
        return iAmHereToo;
    }

    public static void doNotRefactorReturnedAssignment() {
        Date[][] dateRef= new Date[1][1];
        Supplier<Date> supplier = () -> dateRef[0][0] = new Date();
        supplier.get();
        System.out.println(dateRef[0]);
    }

    public static void doNotRefactorReadAssignment() {
        Date[][] dateRef= new Date[1][1];
        Runnable runnable = () -> {
            if ((dateRef[0][0] = new Date()) != null)
                System.out.println("Filled");
        };
        runnable.run();
        System.out.println(dateRef[0]);
    }

    public static int doNotRefactorIncrementalRead() {
        int[] intRef= new int[1];
        Runnable runnable = () -> intRef[0] = 42;
        runnable.run();
        return intRef[0]++;
    }

    public static int doNotRefactorDecrementalRead() {
        int[] intRef= new int[1];
        Runnable runnable = () -> intRef[0] = 42;
        runnable.run();
        return intRef[0]--;
    }

    public static int doNotRefactorPreincrementalRead() {
        int[] intRef= new int[1];
        Runnable runnable = () -> intRef[0] = 42;
        runnable.run();
        return ++intRef[0];
    }

    public static int doNotRefactorPredecrementalRead() {
        int[] intRef= new int[1];
        Runnable runnable = () -> intRef[0] = 42;
        runnable.run();
        return --intRef[0];
    }
}
