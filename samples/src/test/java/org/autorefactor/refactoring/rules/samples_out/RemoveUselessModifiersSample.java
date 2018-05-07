/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2018 Andrei Paikin - Remove protected modifier for final class not inherited members.
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

import java.io.Closeable;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

public interface RemoveUselessModifiersSample {

    String MY_FIELD = "";

    void myMethod(String myArg);

    public static abstract class FixModifierOrderSampleClass implements Closeable {

        public static final String MY_FIELD = "";

        public static final void myMethod(final String myArg) {
        }

        void removeFinalModifierInTryWithResource() throws IOException {
            try (InputStream is = new FileInputStream("out.txt")) {
                is.read();
            }
        }

        @Deprecated
        protected final void reorderModifiers() {
        }

        @Deprecated
        protected final void reorderModifiersAndAnnotations() {
        }

        @Deprecated
        @Override
        public synchronized void close() {
        }
    }

    public static abstract class AbstractSampleClass {
    }

    class Sample {
       private static void refactorIt(){};
       private void refactorIt2(){};

       final void doNotRefactor(){};
       private void doNotRefactor2(){};
   }

    public enum RemoveStaticSampleEnum {
        VALUE1, VALUE2
    }

    public interface RemoveStaticSampleInterface {
    }

    public static @interface FixModifierOrderSampleAnnotation {
        int fixModifierOrder();
    }

    final class Class {
        String refactorThisField = "someStr";

        Class(){}

        void doIt(){}

        void doItThisWay(){}
    }

    final class InheritedClass<E> extends javax.swing.JComboBox<E> {
        String refactorThisField = "someStr";

        InheritedClass(boolean isActive) {}

        @Override
        protected void fireActionEvent() {}
    }

    class DoNotRefactorNonFinalClass {
        protected String doNotRefactorThisField = "someStr";

        protected DoNotRefactorNonFinalClass(){}

        protected final void doIt(){}
    }
}
