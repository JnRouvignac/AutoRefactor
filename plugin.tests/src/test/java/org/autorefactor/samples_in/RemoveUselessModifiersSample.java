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
package org.autorefactor.samples_in;

import java.io.Closeable;

public interface RemoveUselessModifiersSample {

    public static final String MY_FIELD = "";

    public abstract void myMethod(final String myArg);

    abstract static public class FixModifierOrderSampleClass implements Closeable {

        final public static String MY_FIELD = "";

        final public static void myMethod(final String myArg) {
        }

        // FIXME JDT bug? uncomment next line
        // @Override
        synchronized public void close() {
        }
    }

    static public abstract class AbstractSampleClass {
    }

    static public enum FixModifierOrderSampleEnum {
        VALUE1, VALUE2
    }

    static public @interface FixModifierOrderSampleAnnotation {
        abstract public int fixModifierOrder();
    }

}
