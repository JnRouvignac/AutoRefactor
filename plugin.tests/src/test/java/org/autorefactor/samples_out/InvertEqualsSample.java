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

public class InvertEqualsSample {

    public static interface Itf {
        String constant = "fkjfkjf";
        String nullConstant = null;
        MyEnum enumConstant = MyEnum.NOT_NULL;
        MyEnum enumNullConstant = null;
    }

    private static enum MyEnum {
        NOT_NULL
    }

    public boolean invertEquals(Object obj) {
        return "".equals(obj)
                && Itf.constant.equals(obj)
                && ("" + Itf.constant).equals(obj)
                && MyEnum.NOT_NULL.equals(obj);
                // && obj.equals(Itf.enumConstant);
                // should become:
                // && Itf.enumConstant.equals(obj);
    }

    public boolean doNotInvertEquals(Object obj) {
        return obj.equals(Itf.nullConstant) && obj.equals(Itf.enumNullConstant);
    }

    public boolean invertEqualsIgnoreCase(String s) {
        return "".equalsIgnoreCase(s)
                && Itf.constant.equalsIgnoreCase(s)
                && ("" + Itf.constant).equalsIgnoreCase(s);
    }

    public boolean doNotInvertEqualsIgnoreCase(String s) {
        return s.equalsIgnoreCase(Itf.nullConstant);
    }

}
