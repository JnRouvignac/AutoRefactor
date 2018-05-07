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
package org.autorefactor.refactoring.rules.samples_in;

import java.io.Closeable;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

public interface RemoveUselessModifiersSample {

    public static final String MY_FIELD = "";

    public abstract void myMethod(final String myArg);

    abstract static public class FixModifierOrderSampleClass implements Closeable {

        final public static String MY_FIELD = "";

        final public static void myMethod(final String myArg) {
        }

        void removeFinalModifierInTryWithResource() throws IOException {
            try (final InputStream is = new FileInputStream("out.txt")) {
                is.read();
            }
        }

        @Deprecated
        final protected void reorderModifiers() {
        }

        final @Deprecated protected void reorderModifiersAndAnnotations() {
        }

        @Deprecated
        @Override
        synchronized public void close() {
        }
    }

    static public abstract class AbstractSampleClass {
    }

    class Sample {
        private static final void refactorIt(){};
        private final void refactorIt2(){};

        final void doNotRefactor(){};
        private void doNotRefactor2(){};
    }

    static public enum RemoveStaticSampleEnum {
        VALUE1, VALUE2
    }

    static public interface RemoveStaticSampleInterface {
    }

    static public @interface FixModifierOrderSampleAnnotation {
        abstract public int fixModifierOrder();
    }

    final class Class {
        protected String refactorThisField = "someStr";

        protected Class(){}

        protected void doIt(){}

        protected final void doItThisWay(){}
    }

    final class InheritedClass<E> extends javax.swing.JComboBox<E> {
        protected String refactorThisField = "someStr";

        protected InheritedClass(boolean isActive) {}

        @Override
        protected void fireActionEvent() {}
    }

    class DoNotRefactorNonFinalClass {
        protected String doNotRefactorThisField = "someStr";

        protected DoNotRefactorNonFinalClass(){}

        protected final void doIt(){}
    }
}
