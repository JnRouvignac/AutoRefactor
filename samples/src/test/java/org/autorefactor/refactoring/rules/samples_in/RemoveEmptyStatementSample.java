/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class RemoveEmptyStatementSample {

    {
        ;
    }

    public void removeEmptyStatement(boolean b, int i, String[] args) {
        ;
        if (b);
        if (b);
        else;
        if (b) System.out.println(b);
        else;
        try {
            ;
        } catch (Exception e) {
            e.printStackTrace();
        }
        for (String arg : args);
        for (int j = 0; j < 10; j++);
        while (i < 10);
        do ; while (i < 10);
    }

    public void removeEmptyBlock(boolean b, int i, String[] args) {
        {}
        if (b) {}
        if (b) {}
        else {}
        if (b) System.out.println(b);
        else {}
        try {
            {}
        } catch (Exception e) {
            e.printStackTrace();
        }
        for (String arg : args) {}
        for (int j = 0; j < 10; j++) {}
        while (i < 10) {}
        do {} while (i < 10);
    }

    public void doNotRemoveFilledBlock(boolean b, int i, String[] args) {
        {
            System.out.println("foo");
        }
        if (b) {
            System.out.println("foo");
        }
        if (b) {
            System.out.println("foo");
        }
        else {
            System.out.println("foo");
        }
        if (b) System.out.println(b);
        else {
            System.out.println("foo");
        }
        try {
            {
                System.out.println("foo");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        for (String arg : args) {
            System.out.println("foo");
        }
        for (int j = 0; j < 10; j++) {
            System.out.println("foo");
        }
        while (i < 10) {
            System.out.println("foo");
        }
        do {
            System.out.println("foo");
        } while (i < 10);
    }

    public void doNotRemoveEmptyStatement(boolean b) {
        if (b);
        else System.out.println(b);
        if (b) {}
        else System.out.println(b);
    }

    public void doNotRemoveWithMethodCall(List<String> filledList) {
        if (filledList.remove("foo"));
        if (filledList.remove("foo")) {}
    }

    public boolean doNotRemoveWithAssignment(boolean b1, boolean b2) {
        if (b1 = true);
        if (b2 = true) {}
        return b1;
    }

    public int doNotRemoveWithIncrement(int i) {
        if (i-- == 0);
        if (--i == 0) {}
        return i;
    }

    public int doNotRemoveWhileWithIncrement(int i) {
        while (i++ == 100);
        while (++i == 100) {}
        return i;
    }

    public void doNotRemoveInfiniteWhile() {
        while (true);
    }

    public int doNotRemoveDoWhileWithIncrement(int i) {
        do; while (i++ == 100);
        do {} while (++i == 100);
        return i;
    }

    public void doNotRemoveInfiniteDoWhile() {
        do; while (true);
    }

    public void doNotRemoveInfiniteForLoop() {
        for (;;);
    }

    public void doNotRemoveAnotherInfiniteForLoop() {
        for (int i = 0;;i++) {}
    }

    public void doNotRemoveForLoopWithEternalCondition() {
        for (;true;);
    }

    public int doNotRemoveForWithExternalVar(int myValue) {
        for (myValue = 0; myValue < 1000; myValue = myValue * myValue);
        for (myValue = 0; myValue < 1000; myValue = myValue * myValue) {}
        return myValue;
    }

    public int doNotRemoveForWithDecrement(List<String> filledList, int init) {
        for (String aString : filledList.toArray(new String[init--]));
        for (String aString : filledList.toArray(new String[init--])) {}
        return init;
    }

    public class ActiveIteratorList<E> extends ArrayList<E> {

        private int readCount = 0;

        @Override
        public Iterator<E> iterator() {
            readCount++;
            return super.iterator();
        }

        public int getReadCount() {
            return readCount;
        }
    }

    public int doNotRemoveForWithActiveIterator(ActiveIteratorList<String> activeList) {
        for (String aString : activeList);
        for (String aString : activeList) {}

        return activeList.getReadCount();
    }
}
