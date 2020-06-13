/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Vector;
import java.util.List;
import java.util.Observable;

public class ArrayListRatherThanVectorSample {
    public void replaceVectorInstanceCreation() {
        // Keep this comment
        Object[] stringArray = new ArrayList<String>().toArray();
        // Keep this comment too
        int size = new ArrayList<String>(10).size();
    }

    public void replaceRawVector() {
        // Keep this comment
        Object[] objectArray = new ArrayList().toArray();
        // Keep this comment too
        int size = new ArrayList(10).size();
    }

    public void replaceFullyQualifiedVector() {
        // Keep this comment
        Object[] dateArray = new ArrayList<Date>().toArray();
        // Keep this comment too
        int size = new ArrayList(10).size();
    }

    public void replaceVectorVariableDeclaration() {
        // Keep this comment
        ArrayList<String> list = new ArrayList<String>();
    }

    public void replaceCollectionVariableDeclaration() {
        // Keep this comment
        Collection<String> list = new ArrayList<String>();
    }

    public void doNotReplaceInterface() {
        // Keep this comment
        List<String> vector = new Vector<String>();
    }

    public void replaceVectorVariableUse() {
        // Keep this comment
        ArrayList<String> list = new ArrayList<String>();
        // Keep this comment too
        list.add("bar");
    }

    public void replaceCollectionVariableUse() {
        // Keep this comment
        Collection<String> list = new ArrayList<String>();
        // Keep this comment too
        list.add("bar");
    }

    public void refactorWithListMethod() {
        // Keep this comment
        ArrayList<Observable> list = new ArrayList<Observable>();
        // Keep this comment too
        list.toArray(new Observable[0]);
    }

    public void refactorWithCollectionMethod() {
        // Keep this comment
        Collection<Observable> list = new ArrayList<Observable>();
        // Keep this comment too
        list.toArray();
    }

    public String replaceVectorWithLoop(List<Date> dates) {
        // Keep this comment
        ArrayList<Date> list = new ArrayList<Date>();
        for (Date date : dates) {
            list.add(date);
        }

        // Keep this comment too
        ArrayList<Date> secondList = new ArrayList<Date>();
        for (; list.isEmpty(); list.remove(0)) {
            secondList.add(list.get(0));
        }

        return secondList.toString();
    }

    public Observable[] replaceCollectionWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Date> list = new ArrayList<Date>();
        for (Date date : dates) {
            list.add(date);
        }

        // Keep this comment too
        Collection<String> secondList = new ArrayList<String>();
        for (; list.isEmpty(); list.remove(0)) {
            secondList.add(list.toString());
        }

        return secondList.toArray(new Observable[0]);
    }

    public void replaceVectorWithModifier() {
        // Keep this comment
        final ArrayList<String> list = new ArrayList<String>();
        list.add("bar");
    }

    public void replaceCollectionWithModifier() {
        // Keep this comment
        final Collection<String> list = new ArrayList<String>();
        list.add("bar");
    }

    public void replaceVectorWithParameter() {
        // Keep this comment
        ArrayList<String> list = new ArrayList<String>(10);
        list.add("bar");
    }

    public void replaceCollectionWithParameter() {
        // Keep this comment
        Collection<String> list = new ArrayList<String>(10);
        list.add("bar");
    }

    public Object[] replaceReassignedVector() {
        // Keep this comment
        ArrayList<String> list1 = new ArrayList<String>();
        list1.add("FOO");

        // Keep this comment too
        ArrayList<String> list2 = list1;
        list2.add("BAR");

        return list2.toArray();
    }

    public Object[] replaceReassignedCollection() {
        // Keep this comment
        Collection<String> list1 = new ArrayList<String>();
        list1.add("FOO");

        // Keep this comment too
        Collection<String> list2 = list1;
        list2.add("BAR");

        return list2.toArray();
    }

    public void doNotReplaceVectorParameter(Vector<String> aVector) {
        Vector<String> vector = aVector;
        vector.add("bar");
    }

    public void doNotReplaceVectorPassedToAMethod() {
        String text = String.valueOf(new Vector<String>());
    }

    public Vector<Date> doNotReplaceReturnedVector() {
        return new Vector<Date>();
    }

    public void doNotReplaceReassignedVariable() {
        Vector<String> vector = new Vector<String>();
        vector = new Vector<String>();
    }

    public void replaceOldMethod() {
        // Keep this comment
        ArrayList<Integer> list = new ArrayList<Integer>();
        // Keep this comment too
        list.add(42);
        list.get(0);
        list.toArray(new Object[10]);
        list.toArray(new Integer[10]);
        list.remove(123);
        list.remove(1);
        list.clear();
        list.add(2, 456);
        list.set(3, 789);
    }

    public void doNotReplaceSpecificInstantiation() {
        Vector<String> list = new Vector<String>(10, 10);
        list.add("foo");
    }

    public String doNotReplaceSpecificMethod() {
        Vector<String> list = new Vector<String>();
        list.add("foo");
        return list.firstElement();
    }

    public void replaceVectorWithRunnable() {
        // Keep this comment
        final ArrayList<String> list = new ArrayList<String>();
        new Runnable() {

            @Override
            public void run() {
                final ArrayList<String> localList = new ArrayList<String>();
                localList.add("Local, it's safe.");
            }
        };
    }

    public void replaceCollectionWithRunnable() {
        // Keep this comment
        final Collection<String> list = new ArrayList<String>();
        new Runnable() {

            @Override
            public void run() {
                final Collection<String> localList = new ArrayList<String>();
                localList.add("Local, it's safe.");
            }
        };
    }

    public void doNotReplaceThreadSharedVector() {
        final Vector<String> vector = new Vector<String>();
        new Runnable() {

            @Override
            public void run() {
                vector.add("No conflict please");
            }
        };
    }

    public void doNotReplaceAnonymousVector() {
        Vector<String> list = new Vector<String>() {
            private static final long serialVersionUID= 2598952380459765926L;

            @Override
            public String firstElement() {
                return null;
            }
        };
        list.add("bar");
    }
}
