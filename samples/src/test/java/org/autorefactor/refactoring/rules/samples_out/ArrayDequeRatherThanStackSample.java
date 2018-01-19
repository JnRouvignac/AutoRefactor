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
package org.autorefactor.refactoring.rules.samples_out;

import java.util.Collection;
import java.util.Date;
import java.util.Stack;
import java.util.Vector;
import java.util.List;

public class ArrayDequeRatherThanStackSample {

    public void replaceStackInstanceCreation() {
        // Keep this comment
        Object[] stringArray = new java.util.ArrayDeque<String>().toArray(new Integer[10]);
        // Keep this comment too
        int size = new java.util.ArrayDeque<String>().size();
    }

    public void replaceRawStack() {
        // Keep this comment
        Object[] objectArray = new java.util.ArrayDeque().toArray();
        // Keep this comment too
        int size = new java.util.ArrayDeque().size();
    }

    public void replaceFullyQualifiedStack() {
        // Keep this comment
        Object[] dateArray = new java.util.ArrayDeque<Date>().toArray();
        // Keep this comment too
        int size = new java.util.ArrayDeque().size();
    }

    public void replaceStackVariableDeclaration() {
        // Keep this comment
        java.util.ArrayDeque<String> queue = new java.util.ArrayDeque<String>();
    }

    public void replaceVectorVariableDeclaration() {
        // Keep this comment
        java.util.Collection<String> queue = new java.util.ArrayDeque<String>();
    }

    public void replaceCollectionVariableDeclaration() {
        // Keep this comment
        Collection<String> queue = new java.util.ArrayDeque<String>();
    }

    public void doNotReplaceInterface() {
        // Keep this comment
        List<String> queue = new Stack<String>();
    }

    public void replaceStackVariableUse() {
        // Keep this comment
        java.util.ArrayDeque<String> queue = new java.util.ArrayDeque<String>();
        // Keep this comment too
        queue.add("bar");
    }

    public void replaceVectorVariableUse() {
        // Keep this comment
        java.util.Collection<String> queue = new java.util.ArrayDeque<String>();
        // Keep this comment too
        queue.add("bar");
    }

    public void replaceCollectionVariableUse() {
        // Keep this comment
        Collection<String> queue = new java.util.ArrayDeque<String>();
        // Keep this comment too
        queue.add("bar");
    }

    public void refactorMethod() {
        // Keep this comment
        java.util.ArrayDeque<String> queue = new java.util.ArrayDeque<String>();
        // Keep this comment too
        queue.toArray();
    }

    public void refactorVectorMethod() {
        // Keep this comment
        java.util.Collection<String> queue = new java.util.ArrayDeque<String>();
        // Keep this comment too
        queue.toArray();
    }

    public void refactorCollectionMethod() {
        // Keep this comment
        Collection<String> queue = new java.util.ArrayDeque<String>();
        // Keep this comment too
        queue.toArray();
    }

    public String replaceStackWithLoop(List<Date> dates) {
        // Keep this comment
        java.util.ArrayDeque<Date> queue = new java.util.ArrayDeque<Date>();
        for (Date date : dates) {
            queue.add(date);
        }

        return queue.toString();
    }

    public String replaceVectorWithLoop(List<Date> dates) {
        // Keep this comment
        java.util.Collection<Date> queue = new java.util.ArrayDeque<Date>();
        for (Date date : dates) {
            queue.add(date);
        }

        return queue.toString();
    }

    public String replaceCollectionWithLoop(List<Date> dates) {
        // Keep this comment
        Collection<Date> queue = new java.util.ArrayDeque<Date>();
        for (Date date : dates) {
            queue.add(date);
        }

        return queue.toString();
    }

    public void replaceStackWithModifier() {
        // Keep this comment
        final java.util.ArrayDeque<String> queue = new java.util.ArrayDeque<String>();
        queue.add("bar");
    }

    public void replaceVectorWithModifier() {
        // Keep this comment
        final java.util.Collection<String> queue = new java.util.ArrayDeque<String>();
        queue.add("bar");
    }

    public void replaceCollectionWithModifier() {
        // Keep this comment
        final Collection<String> queue = new java.util.ArrayDeque<String>();
        queue.add("bar");
    }

    public void replaceStackWithParameter() {
        // Keep this comment
        java.util.ArrayDeque<String> queue = new java.util.ArrayDeque<String>();
        queue.add("bar");
    }

    public void replaceVectorWithParameter() {
        // Keep this comment
        java.util.Collection<String> queue = new java.util.ArrayDeque<String>();
        queue.add("bar");
    }

    public void replaceCollectionWithParameter() {
        // Keep this comment
        Collection<String> queue = new java.util.ArrayDeque<String>();
        queue.add("bar");
    }

    public Object[] replaceReassignedStack() {
        // Keep this comment
        java.util.ArrayDeque<String> queue1 = new java.util.ArrayDeque<String>();
        queue1.add("FOO");

        // Keep this comment too
        java.util.ArrayDeque<String> queue2 = queue1;
        queue2.add("BAR");

        return queue2.toArray();
    }

    public Object[] replaceReassignedVector() {
        // Keep this comment
        java.util.ArrayDeque<String> queue1 = new java.util.ArrayDeque<String>();
        queue1.add("FOO");

        // Keep this comment too
        java.util.Collection<String> queue2 = queue1;
        queue2.add("BAR");

        return queue2.toArray();
    }

    public Object[] replaceReassignedCollection() {
        // Keep this comment
        java.util.ArrayDeque<String> queue1 = new java.util.ArrayDeque<String>();
        queue1.add("FOO");

        // Keep this comment too
        Collection<String> queue2 = queue1;
        queue2.add("BAR");

        return queue2.toArray();
    }

    public void doNotReplaceStackParameter(Stack<String> aStack) {
        Stack<String> stack = aStack;
        stack.add("bar");
    }

    public void doNotReplaceStackPassedToAMethod() {
        String p3 = String.valueOf(new Stack<String>());
    }

    public Stack<Date> doNotReplaceReturnedStack() {
        return new Stack<Date>();
    }

    public void doNotReplaceReassignedVariable() {
        Stack<String> stack = new Stack<String>();
        stack = new Stack<String>();
    }

    public void replaceOldMethod() {
        java.util.ArrayDeque<Integer> queue = new java.util.ArrayDeque<Integer>();
        // Keep this comment
        queue.add(42);
        queue.toArray(new Object[10]);
        queue.remove(123);
        queue.clear();
        queue.getFirst();
        queue.getLast();
        queue.isEmpty();
    }

    public String doNotReplaceSpecificMethod() {
        Stack<String> stack = new Stack<String>();
        stack.removeElementAt(1);
        return stack.elementAt(0);
    }

    public void replaceStackWithRunnable() {
        // Keep this comment
        final java.util.ArrayDeque<String> queue = new java.util.ArrayDeque<String>();
        new Runnable() {

            @Override
            public void run() {
                final java.util.ArrayDeque<String> localQueue = new java.util.ArrayDeque<String>();
                localQueue.add("Local, it's safe.");
            }
        };
    }

    public void replaceVectorWithRunnable() {
        // Keep this comment
        final java.util.Collection<String> queue = new java.util.ArrayDeque<String>();
        new Runnable() {

            @Override
            public void run() {
                final java.util.Collection<String> localQueue = new java.util.ArrayDeque<String>();
                localQueue.add("Local, it's safe.");
            }
        };
    }

    public void replaceCollectionWithRunnable() {
        // Keep this comment
        final Collection<String> queue = new java.util.ArrayDeque<String>();
        new Runnable() {

            @Override
            public void run() {
                final Collection<String> localQueue = new java.util.ArrayDeque<String>();
                localQueue.add("Local, it's safe.");
            }
        };
    }

    public void doNotReplaceThreadSharedStack() {
        final Stack<String> stack = new Stack<String>();
        new Runnable() {

            @Override
            public void run() {
                stack.add("No conflict please");
            }
        };
    }
}
