/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
import java.util.Iterator;
import java.util.function.Predicate;

public class RemoveFieldsDefaultValuesSample {

    private interface Constants {
        String NULL = null;
    }

    private static final int MY_CONSTANT = 0;
    private int keepInitializer = MY_CONSTANT;

    private final boolean final_bo = false;

    private Object doNotRemoveInitializer = new Object();

    private Object obj;
    private String st;
    private byte by1, by2;
    private boolean bo;
    private char c1, c2;
    private short sh;
    private int i;
    private long l1, l2, l3;
    private float f1, f2, f3, f4;
    private double d1, d2;
    private Predicate<String> doNotRemoveLambdaExpr = x -> "foo".equals(x);

    public Iterable<String> getIterable() {
        return new Iterable<String>() {
            private Collection<String> aField;

            @Override
            public Iterator<String> iterator() {
                return aField.iterator();
            }
        };
    }

    private enum MyEnum {
        ONE, TWO;

        String aField;
    }

    private @interface MyAnnotation {
        String aField = null;
    }
}
