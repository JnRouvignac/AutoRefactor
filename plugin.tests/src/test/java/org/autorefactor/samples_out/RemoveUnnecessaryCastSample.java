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
package org.autorefactor.samples_out;

public class RemoveUnnecessaryCastSample {

    public Object removeCastToSameType(Integer i) {
        return i;
    }

    public Object removeCastToIntegerWrapper(int i) {
        return i;
    }

    public Integer doNotRemoveNarrowingCast1(Object o) {
        return (Integer) o;
    }

    public int doNotRemoveNarrowingCast2(Object o) {
        return (Integer) o;
    }

    public long removeWideningPrimitiveCast(int i) {
        return i;
    }

    public int doNotRemovePrimitiveNarrowingCast(long l) {
        return (int) l;
    }

    public void removeAssignmentCasts(Integer oi, int pi, Object o) {
        Object o1;
        long l;
        int i;
        o1 = oi;
        o1 = pi;
        l = pi;
        i = pi;
    }

    public void doNotRemoveAssignmentCasts(Object o, long l) {
        Integer oi;
        int i;
        oi = (Integer) o;
        i = (Integer) o;
        i = (int) l;
    }

    public void removeVariableDeclarationFragmentCasts(Integer oi, int pi, Object o) {
        Object o1 = oi;
        Object o2 = pi;
        long l = pi;
    }

    public void doNotRemoveVariableDeclarationFragmentCasts(Object o, long l) {
        Integer oi1 = (Integer) o;
        int pi2 = (Integer) o;
        int pi3 = (int) l;
    }

    public boolean removeCasts(Integer oi, int pi, long l) {
        return oi != pi
            && pi != oi
            && oi != pi
            && pi != oi
            && l != pi
            && pi != l;
    }

    public boolean doNotRemoveCasts(Integer oi, int pi, long l, Object o) {
        return (int) l != pi
            && pi != (int) l
            && (Integer) o != pi // FIXME cast to (int) after moving to Java 7 builds
            && pi != (Integer) o // FIXME cast to (int) after moving to Java 7 builds
            && (Integer) o != oi // FIXME cast to (int) after moving to Java 7 builds
            && oi != (Integer) o; // FIXME cast to (int) after moving to Java 7 builds
    }

    public boolean doNotRemovePrimitiveNarrowingCastsWithComparison(int i) {
        return (byte) i == 0
            && 0 == (byte) i
            && (char) i == 0
            && 0 == (char) i
            && (short) i == 0
            && 0 == (short) i;
    }

    public long removeWideningCast(int i, int j) {
        return i + j;
    }

    public long removeWideningCastsWithExtendedOperands(int i, int j) {
        return i + j + i;
    }

    public int doNotRemovePrimitiveNarrowingCasts(int i, int j) {
        return (byte) i + j;
    }

    public int doNotRemovePrimitiveNarrowingCastsWithExtendedOperands(int i, int j) {
        return (byte) i + j + (byte) i;
    }
}
