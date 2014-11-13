/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.samples_out;

import java.io.Closeable;

public interface RemoveUselessModifiersSample {

    String MY_FIELD = "";

    void myMethod(String myArg);

    public static abstract class FixModifierOrderSampleClass implements Closeable {

        public static final String MY_FIELD = "";

        public static void myMethod(final String myArg) {
        }

        // FIXME JDT bug? (still failing with juno) uncomment next line
        // @Override
        public synchronized void close() {
        }
    }

    public static abstract class AbstractSampleClass {
    }

    public static enum FixModifierOrderSampleEnum {
        VALUE1, VALUE2
    }

    public static @interface FixModifierOrderSampleAnnotation {
        int fixModifierOrder();
    }

}
