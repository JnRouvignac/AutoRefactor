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

import java.io.FileInputStream;
import java.io.IOException;
import java.util.AbstractList;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class DeadCodeEliminationSample {

    public int removeEmptyElseClause(boolean b) {
        int i = 0;
        if (b) {
            i++;
        }
        return i;
    }

    public int removeEmptyThenClause(boolean b) {
        int i = 0;
        if (!b) {
            i++;
        }
        return i;
    }

    public int removeEmptyIfStatement(boolean b) {
        int i = 0;
        return i;
    }

    public int removeImpossibleIfClauses() {
        int i = 0;
        int j = 0;
        // Keep this comment
        i++;

        // Keep this comment
        i++;

        // Keep this comment
        j++;

        // Keep this comment
        j++;

        return i + j;
    }

    public int doNotRefactorWithVariableConflict() {
        if (true) {
            int j = 0;
        }
        int j = 1;
        return j;
    }

    public int removeConditionWithCompatibleVariables(int i) {
        if (i == 0) {
            int j = 0;
        }
        // Keep this comment
        // Keep this comment too
        int j = 1;
        return 1;
    }

    public int removeDeadCodeAfterIfTrueWithReturn(int i) {
        System.out.println(i);
        return 1;
    }

    public int removeDeadCodeAfterEmbeddedIfTrueWithThrow(int i) {
        System.out.println(i);
        throw new RuntimeException();
    }

    public int removeDeadCodeAfterIfFalseWithThrow(int i) {
        System.out.println(i);
        throw new RuntimeException();
    }

    public int doNotRemoveDeadCodeAfterEmbeddedIfTrueNoThrowOrReturn(int i) {
        System.out.println(i);
        return 2;
    }

    public int doNotRemoveAfterIfFalseNoThrowOrReturn(int i) {
        System.out.println(i);
        return 2;
    }

    public int removeDeadCodeAfterEmbeddedIfThrowOrReturn(boolean b, int i) {
        // Keep this comment
        if (b) {
            toString();
            return 1;
        } else {
            System.out.println(i);
            throw new RuntimeException();
        }
    }

    public int removeOnlyConstantConditionWithEmbeddedIf(boolean b, int i) {
        // Keep this comment
        if (b) {
            toString();
        } else {
            System.out.println(i);
        }
        return 2;
    }

    public int removeOnlyConstantConditionWithEmbeddedIfReturn(boolean b) {
        // Keep this comment
        if (b) {
            toString();
            return 1;
        }
        return 2;
    }

// FIXME compilation error.
// Following code:
//    private int removeWhileWithoutIterations() {
//        int i = 0;
//        while (false) {
//            i++;
//        }
//        while (false)
//            i++;
//        return i;
//    }
// Should become
//    private int removeWhileWithoutIterations() {
//        int i = 0;
//        return i;
//    }

    public int removeEmptyTryEmptyFinally() {
        int i = 0;
        return i;
    }

    public int removeEmptyTryNonEmptyFinally() {
        int i = 0;
        // Keep this comment
        i++;
        return i;
    }

    public int doNotRemoveEmptyTryWithVariableConflict() {
        try {
        } finally {
            int i = 0;
            i++;
        }
        int i = 0;
        return i;
    }

    public void doNotRemoveTryWithResources() throws IOException {
        try (FileInputStream f = new FileInputStream("file.txt")) {
        }
    }

    public void doNotRemoveTryWithResourcesAndFinally() throws IOException {
        int i = 0;
        try (FileInputStream f = new FileInputStream("file.txt")) {
        } finally {
            i++;
        }
    }

    public interface MethodDeclarationWithoutBody {
        void aMethod();
    }

    public void doNotRemovePackageAccessedMethodOverride() {
        MyAbstractList<String> l = new MyAbstractList<>();
        l.removeRange(0, l.size());
    }

    public static class MyAbstractList<E> extends AbstractList<E> {
        @Override
        public E get(int index) {
            return null;
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        protected void removeRange(int fromIndex, int toIndex) {
            super.removeRange(fromIndex, toIndex);
        }
    }

    public void removeIfButKeepConditionsWithSideEffects(AtomicBoolean b, AtomicInteger i, Collection<Object> col) {
        b.getAndSet(true);
        col.add(1);
        i.getAndIncrement();
        i.getAndIncrement();
        b.getAndSet(false);
        col.add(2);
    }
}
