/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix.samples_in;

import static org.assertj.core.api.Assertions.*;

import java.util.Date;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.Fail;

public class AssertJSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
        // Keep this comment
        Assertions.assertThat(i1 == i2).isTrue();
        Assertions.assertThat(i1 == i2).as("Failure message to keep").isTrue();
        Assertions.assertThat(i1 == i2).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(i1 != i2).isFalse();
        Assertions.assertThat(i1 != i2).as("Failure message to keep").isFalse();
        Assertions.assertThat(i1 != i2).describedAs("Failure message to keep").isFalse();

        assertThat(i1 == i2).isTrue();
        assertThat(i1 == i2).as("Failure message to keep").isTrue();
        assertThat(i1 == i2).describedAs("Failure message to keep").isTrue();
        assertThat(i1 != i2).isFalse();
        assertThat(i1 != i2).as("Failure message to keep").isFalse();
        assertThat(i1 != i2).describedAs("Failure message to keep").isFalse();
    }

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assertions.assertThat(d1 == d2).isTrue();
        Assertions.assertThat(d1 == d2).as("Failure message to keep").isTrue();
        Assertions.assertThat(d1 == d2).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(d1 != d2).isFalse();
        Assertions.assertThat(d1 != d2).as("Failure message to keep").isFalse();
        Assertions.assertThat(d1 != d2).describedAs("Failure message to keep").isFalse();

        assertThat(d1 == d2).isTrue();
        assertThat(d1 == d2).as("Failure message to keep").isTrue();
        assertThat(d1 == d2).describedAs("Failure message to keep").isTrue();
        assertThat(d1 != d2).isFalse();
        assertThat(d1 != d2).as("Failure message to keep").isFalse();
        assertThat(d1 != d2).describedAs("Failure message to keep").isFalse();
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assertions.assertThat(f1 == f2).isTrue();
        Assertions.assertThat(f1 == f2).as("Failure message to keep").isTrue();
        Assertions.assertThat(f1 == f2).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(f1 != f2).isFalse();
        Assertions.assertThat(f1 != f2).as("Failure message to keep").isFalse();
        Assertions.assertThat(f1 != f2).describedAs("Failure message to keep").isFalse();

        assertThat(f1 == f2).isTrue();
        assertThat(f1 == f2).as("Failure message to keep").isTrue();
        assertThat(f1 == f2).describedAs("Failure message to keep").isTrue();
        assertThat(f1 != f2).isFalse();
        assertThat(f1 != f2).as("Failure message to keep").isFalse();
        assertThat(f1 != f2).describedAs("Failure message to keep").isFalse();
    }

    public void removeParenthesis(int i1, int i2, int i3, int i4) {
        // Keep this comment
        Assertions.assertThat((i1 + i2) == (i3 + i4)).isTrue();
        Assertions.assertThat((i1 + i2) == (i3 + i4)).as("Failure message to keep").isTrue();
        Assertions.assertThat((i1 + i2) == (i3 + i4)).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat((i1 + i2) != (i3 + i4)).isFalse();
        Assertions.assertThat((i1 + i2) != (i3 + i4)).as("Failure message to keep").isFalse();
        Assertions.assertThat((i1 + i2) != (i3 + i4)).describedAs("Failure message to keep").isFalse();

        assertThat((i1 + i2) == (i3 + i4)).isTrue();
        assertThat((i1 + i2) == (i3 + i4)).as("Failure message to keep").isTrue();
        assertThat((i1 + i2) == (i3 + i4)).describedAs("Failure message to keep").isTrue();
        assertThat((i1 + i2) != (i3 + i4)).isFalse();
        assertThat((i1 + i2) != (i3 + i4)).as("Failure message to keep").isFalse();
        assertThat((i1 + i2) != (i3 + i4)).describedAs("Failure message to keep").isFalse();
    }

    public void refactorFailures() {
        // Keep this comment
        Assertions.assertThat(false).isTrue();
        Assertions.assertThat(false).as("Failure message to keep").isTrue();
        Assertions.assertThat(false).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(true).isFalse();
        Assertions.assertThat(true).as("Failure message to keep").isFalse();
        Assertions.assertThat(true).describedAs("Failure message to keep").isFalse();

        assertThat(false).isTrue();
        assertThat(false).as("Failure message to keep").isTrue();
        assertThat(false).describedAs("Failure message to keep").isTrue();
        assertThat(true).isFalse();
        assertThat(true).as("Failure message to keep").isFalse();
        assertThat(true).describedAs("Failure message to keep").isFalse();
    }

    public void removeDeadChecks() {
        Assertions.assertThat(true).isTrue();
        Assertions.assertThat(true).as("Useless message").isTrue();
        Assertions.assertThat(true).describedAs("Useless message").isTrue();
        Assertions.assertThat(false).isFalse();
        Assertions.assertThat(false).as("Useless message").isFalse();
        Assertions.assertThat(false).describedAs("Useless message").isFalse();

        assertThat(true).isTrue();
        assertThat(true).as("Useless message").isTrue();
        assertThat(true).describedAs("Useless message").isTrue();
        assertThat(false).isFalse();
        assertThat(false).as("Useless message").isFalse();
        assertThat(false).describedAs("Useless message").isFalse();
    }

    public void replaceAssertByBlocks(boolean b) {
        if (b)
            Assertions.assertThat(true).isTrue();
        if (b)
            Assertions.assertThat(true).as("Useless message").isTrue();
        Assertions.assertThat(true).describedAs("Useless message").isTrue();
        if (b)
            Assertions.assertThat(false).isFalse();
        if (b)
            Assertions.assertThat(false).as("Useless message").isFalse();
        Assertions.assertThat(false).describedAs("Useless message").isFalse();

        if (b)
            assertThat(true).isTrue();
        if (b)
            assertThat(true).as("Useless message").isTrue();
        assertThat(true).describedAs("Useless message").isTrue();
        if (b)
            assertThat(false).isFalse();
        if (b)
            assertThat(false).as("Useless message").isFalse();
        assertThat(false).describedAs("Useless message").isFalse();
    }

    public void refactorNegatedConditions(boolean b) {
        // Keep this comment
        Assertions.assertThat(!b).isTrue();
        Assertions.assertThat(!b).as("Failure message to keep").isTrue();
        Assertions.assertThat(!b).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(!b).isFalse();
        Assertions.assertThat(!b).as("Failure message to keep").isFalse();
        Assertions.assertThat(!b).describedAs("Failure message to keep").isFalse();

        assertThat(!b).isTrue();
        assertThat(!b).as("Failure message to keep").isTrue();
        assertThat(!b).describedAs("Failure message to keep").isTrue();
        assertThat(!b).isFalse();
        assertThat(!b).as("Failure message to keep").isFalse();
        assertThat(!b).describedAs("Failure message to keep").isFalse();
    }

    public void refactorWithObjectReferences(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertThat(o1 == o2).isTrue();
        Assertions.assertThat(o1 == o2).as("Failure message to keep").isTrue();
        Assertions.assertThat(o1 == o2).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(o1 != o2).isTrue();
        Assertions.assertThat(o1 != o2).as("Failure message to keep").isTrue();
        Assertions.assertThat(o1 != o2).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(o1 != o2).isFalse();
        Assertions.assertThat(o1 != o2).as("Failure message to keep").isFalse();
        Assertions.assertThat(o1 != o2).describedAs("Failure message to keep").isFalse();
        Assertions.assertThat(o1 == o2).isFalse();
        Assertions.assertThat(o1 == o2).as("Failure message to keep").isFalse();
        Assertions.assertThat(o1 == o2).describedAs("Failure message to keep").isFalse();

        assertThat(o1 == o2).isTrue();
        assertThat(o1 == o2).as("Failure message to keep").isTrue();
        assertThat(o1 == o2).describedAs("Failure message to keep").isTrue();
        assertThat(o1 != o2).isTrue();
        assertThat(o1 != o2).as("Failure message to keep").isTrue();
        assertThat(o1 != o2).describedAs("Failure message to keep").isTrue();
        assertThat(o1 != o2).isFalse();
        assertThat(o1 != o2).as("Failure message to keep").isFalse();
        assertThat(o1 != o2).describedAs("Failure message to keep").isFalse();
        assertThat(o1 == o2).isFalse();
        assertThat(o1 == o2).as("Failure message to keep").isFalse();
        assertThat(o1 == o2).describedAs("Failure message to keep").isFalse();
    }

    public void refactorWithObjects(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertThat(o1.equals(o2)).isTrue();
        Assertions.assertThat(o1.equals(o2)).as("Failure message to keep").isTrue();
        Assertions.assertThat(o1.equals(o2)).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(!(o1.equals(o2))).isFalse();
        Assertions.assertThat(!(o1.equals(o2))).as("Failure message to keep").isFalse();
        Assertions.assertThat(!(o1.equals(o2))).describedAs("Failure message to keep").isFalse();

        assertThat(o1.equals(o2)).isTrue();
        assertThat(o1.equals(o2)).as("Failure message to keep").isTrue();
        assertThat(o1.equals(o2)).describedAs("Failure message to keep").isTrue();
        assertThat(!(o1.equals(o2))).isFalse();
        assertThat(!(o1.equals(o2))).as("Failure message to keep").isFalse();
        assertThat(!(o1.equals(o2))).describedAs("Failure message to keep").isFalse();
    }

    public void refactorNullCheckFirstArg(Object o) {
        // Keep this comment
        Assertions.assertThat(null == o).isTrue();
        Assertions.assertThat(null == o).as("Failure message to keep").isTrue();
        Assertions.assertThat(null == o).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(null != o).isTrue();
        Assertions.assertThat(null != o).as("Failure message to keep").isTrue();
        Assertions.assertThat(null != o).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(null != o).isFalse();
        Assertions.assertThat(null != o).as("Failure message to keep").isFalse();
        Assertions.assertThat(null != o).describedAs("Failure message to keep").isFalse();
        Assertions.assertThat(null == o).isFalse();
        Assertions.assertThat(null == o).as("Failure message to keep").isFalse();
        Assertions.assertThat(null == o).describedAs("Failure message to keep").isFalse();

        assertThat(null == o).isTrue();
        assertThat(null == o).as("Failure message to keep").isTrue();
        assertThat(null == o).describedAs("Failure message to keep").isTrue();
        assertThat(null != o).isTrue();
        assertThat(null != o).as("Failure message to keep").isTrue();
        assertThat(null != o).describedAs("Failure message to keep").isTrue();
        assertThat(null != o).isFalse();
        assertThat(null != o).as("Failure message to keep").isFalse();
        assertThat(null != o).describedAs("Failure message to keep").isFalse();
        assertThat(null == o).isFalse();
        assertThat(null == o).as("Failure message to keep").isFalse();
        assertThat(null == o).describedAs("Failure message to keep").isFalse();
    }

    public void refactorNullCheckSecondArg(Object o) {
        // Keep this comment
        Assertions.assertThat(o == null).isTrue();
        Assertions.assertThat(o == null).as("Failure message to keep").isTrue();
        Assertions.assertThat(o == null).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(o != null).isTrue();
        Assertions.assertThat(o != null).as("Failure message to keep").isTrue();
        Assertions.assertThat(o != null).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(o != null).isFalse();
        Assertions.assertThat(o != null).as("Failure message to keep").isFalse();
        Assertions.assertThat(o != null).describedAs("Failure message to keep").isFalse();
        Assertions.assertThat(o == null).isFalse();
        Assertions.assertThat(o == null).as("Failure message to keep").isFalse();
        Assertions.assertThat(o == null).describedAs("Failure message to keep").isFalse();

        assertThat(o == null).isTrue();
        assertThat(o == null).as("Failure message to keep").isTrue();
        assertThat(o == null).describedAs("Failure message to keep").isTrue();
        assertThat(o != null).isTrue();
        assertThat(o != null).as("Failure message to keep").isTrue();
        assertThat(o != null).describedAs("Failure message to keep").isTrue();
        assertThat(o != null).isFalse();
        assertThat(o != null).as("Failure message to keep").isFalse();
        assertThat(o != null).describedAs("Failure message to keep").isFalse();
        assertThat(o == null).isFalse();
        assertThat(o == null).as("Failure message to keep").isFalse();
        assertThat(o == null).describedAs("Failure message to keep").isFalse();
    }

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertThat(o).equals(null);
        Assertions.assertThat(o).as("Failure message to keep").equals(null);
        Assertions.assertThat(o).describedAs("Failure message to keep").equals(null);
        Assertions.assertThat(o).isEqualTo(null);
        Assertions.assertThat(o).as("Failure message to keep").isEqualTo(null);
        Assertions.assertThat(o).describedAs("Failure message to keep").isEqualTo(null);
        Assertions.assertThat(o).isNotEqualTo(null);
        Assertions.assertThat(o).as("Failure message to keep").isNotEqualTo(null);
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotEqualTo(null);

        assertThat(o).equals(null);
        assertThat(o).as("Failure message to keep").equals(null);
        assertThat(o).describedAs("Failure message to keep").equals(null);
        assertThat(o).isEqualTo(null);
        assertThat(o).as("Failure message to keep").isEqualTo(null);
        assertThat(o).describedAs("Failure message to keep").isEqualTo(null);
        assertThat(o).isNotEqualTo(null);
        assertThat(o).as("Failure message to keep").isNotEqualTo(null);
        assertThat(o).describedAs("Failure message to keep").isNotEqualTo(null);
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertThat(42).isEqualTo(o);
        Assertions.assertThat(42).as("Failure message to keep").isEqualTo(o);
        Assertions.assertThat(42).describedAs("Failure message to keep").isEqualTo(o);
        Assertions.assertThat(42).isNotEqualTo(o);
        Assertions.assertThat(42).as("Failure message to keep").isNotEqualTo(o);
        Assertions.assertThat(42).describedAs("Failure message to keep").isNotEqualTo(o);

        assertThat(42).isEqualTo(o);
        assertThat(42).as("Failure message to keep").isEqualTo(o);
        assertThat(42).describedAs("Failure message to keep").isEqualTo(o);
        assertThat(42).isNotEqualTo(o);
        assertThat(42).as("Failure message to keep").isNotEqualTo(o);
        assertThat(42).describedAs("Failure message to keep").isNotEqualTo(o);
    }

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assertions.assertThat(42L).isEqualTo(l);
        Assertions.assertThat(42L).as("Failure message to keep").isEqualTo(l);
        Assertions.assertThat(42L).describedAs("Failure message to keep").isEqualTo(l);
        Assertions.assertThat(42L).isNotEqualTo(l);
        Assertions.assertThat(42L).as("Failure message to keep").isNotEqualTo(l);
        Assertions.assertThat(42L).describedAs("Failure message to keep").isNotEqualTo(l);

        assertThat(42L).isEqualTo(l);
        assertThat(42L).as("Failure message to keep").isEqualTo(l);
        assertThat(42L).describedAs("Failure message to keep").isEqualTo(l);
        assertThat(42L).isNotEqualTo(l);
        assertThat(42L).as("Failure message to keep").isNotEqualTo(l);
        assertThat(42L).describedAs("Failure message to keep").isNotEqualTo(l);
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assertions.assertThat(42).isEqualTo(i);
        Assertions.assertThat(42).as("Failure message to keep").isEqualTo(i);
        Assertions.assertThat(42).describedAs("Failure message to keep").isEqualTo(i);
        Assertions.assertThat(42).isNotEqualTo(i);
        Assertions.assertThat(42).as("Failure message to keep").isNotEqualTo(i);
        Assertions.assertThat(42).describedAs("Failure message to keep").isNotEqualTo(i);

        assertThat(42).isEqualTo(i);
        assertThat(42).as("Failure message to keep").isEqualTo(i);
        assertThat(42).describedAs("Failure message to keep").isEqualTo(i);
        assertThat(42).isNotEqualTo(i);
        assertThat(42).as("Failure message to keep").isNotEqualTo(i);
        assertThat(42).describedAs("Failure message to keep").isNotEqualTo(i);
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assertions.assertThat('a').isEqualTo(c);
        Assertions.assertThat('a').as("Failure message to keep").isEqualTo(c);
        Assertions.assertThat('a').describedAs("Failure message to keep").isEqualTo(c);
        Assertions.assertThat('a').isNotEqualTo(c);
        Assertions.assertThat('a').as("Failure message to keep").isNotEqualTo(c);
        Assertions.assertThat('a').describedAs("Failure message to keep").isNotEqualTo(c);

        assertThat('a').isEqualTo(c);
        assertThat('a').as("Failure message to keep").isEqualTo(c);
        assertThat('a').describedAs("Failure message to keep").isEqualTo(c);
        assertThat('a').isNotEqualTo(c);
        assertThat('a').as("Failure message to keep").isNotEqualTo(c);
        assertThat('a').describedAs("Failure message to keep").isNotEqualTo(c);
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assertions.assertThat(1 + 2 + 3).isEqualTo(i);
        Assertions.assertThat(1 + 2 + 3).as("Failure message to keep").isEqualTo(i);
        Assertions.assertThat(1 + 2 + 3).describedAs("Failure message to keep").isEqualTo(i);
        Assertions.assertThat(1 + 2 + 3).isNotEqualTo(i);
        Assertions.assertThat(1 + 2 + 3).as("Failure message to keep").isNotEqualTo(i);
        Assertions.assertThat(1 + 2 + 3).describedAs("Failure message to keep").isNotEqualTo(i);

        assertThat(1 + 2 + 3).isEqualTo(i);
        assertThat(1 + 2 + 3).as("Failure message to keep").isEqualTo(i);
        assertThat(1 + 2 + 3).describedAs("Failure message to keep").isEqualTo(i);
        assertThat(1 + 2 + 3).isNotEqualTo(i);
        assertThat(1 + 2 + 3).as("Failure message to keep").isNotEqualTo(i);
        assertThat(1 + 2 + 3).describedAs("Failure message to keep").isNotEqualTo(i);
    }

    public void doNotRefactorLiteralAsExpectedArgInWithEquals(Object o) {
        Assertions.assertThat(o).isEqualTo(42);
        Assertions.assertThat(o).as("Failure message to keep").isEqualTo(42);
        Assertions.assertThat(o).describedAs("Failure message to keep").isEqualTo(42);
        Assertions.assertThat(o).isNotEqualTo(42);
        Assertions.assertThat(o).as("Failure message to keep").isNotEqualTo(42);
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotEqualTo(42);

        assertThat(o).isEqualTo(42);
        assertThat(o).as("Failure message to keep").isEqualTo(42);
        assertThat(o).describedAs("Failure message to keep").isEqualTo(42);
        assertThat(o).isNotEqualTo(42);
        assertThat(o).as("Failure message to keep").isNotEqualTo(42);
        assertThat(o).describedAs("Failure message to keep").isNotEqualTo(42);
    }

    public void refactorTrueCheckSecondArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(true).isEqualTo(b);
        Assertions.assertThat(true).as("Failure message to keep").isEqualTo(b);
        Assertions.assertThat(true).describedAs("Failure message to keep").isEqualTo(b);
        Assertions.assertThat(true).isNotEqualTo(b);
        Assertions.assertThat(true).as("Failure message to keep").isNotEqualTo(b);
        Assertions.assertThat(true).describedAs("Failure message to keep").isNotEqualTo(b);

        assertThat(true).isEqualTo(b);
        assertThat(true).as("Failure message to keep").isEqualTo(b);
        assertThat(true).describedAs("Failure message to keep").isEqualTo(b);
        assertThat(true).isNotEqualTo(b);
        assertThat(true).as("Failure message to keep").isNotEqualTo(b);
        assertThat(true).describedAs("Failure message to keep").isNotEqualTo(b);
    }

    public void refactorTrueCheckFirstArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(b).isEqualTo(true);
        Assertions.assertThat(b).as("Failure message to keep").isEqualTo(true);
        Assertions.assertThat(b).describedAs("Failure message to keep").isEqualTo(true);
        Assertions.assertThat(b).isNotEqualTo(true);
        Assertions.assertThat(b).as("Failure message to keep").isNotEqualTo(true);
        Assertions.assertThat(b).describedAs("Failure message to keep").isNotEqualTo(true);

        assertThat(b).isEqualTo(true);
        assertThat(b).as("Failure message to keep").isEqualTo(true);
        assertThat(b).describedAs("Failure message to keep").isEqualTo(true);
        assertThat(b).isNotEqualTo(true);
        assertThat(b).as("Failure message to keep").isNotEqualTo(true);
        assertThat(b).describedAs("Failure message to keep").isNotEqualTo(true);
    }

    public void refactorFalseCheckSecondArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(false).isEqualTo(b);
        Assertions.assertThat(false).as("Failure message to keep").isEqualTo(b);
        Assertions.assertThat(false).describedAs("Failure message to keep").isEqualTo(b);
        Assertions.assertThat(false).isNotEqualTo(b);
        Assertions.assertThat(false).as("Failure message to keep").isNotEqualTo(b);
        Assertions.assertThat(false).describedAs("Failure message to keep").isNotEqualTo(b);

        assertThat(false).isEqualTo(b);
        assertThat(false).as("Failure message to keep").isEqualTo(b);
        assertThat(false).describedAs("Failure message to keep").isEqualTo(b);
        assertThat(false).isNotEqualTo(b);
        assertThat(false).as("Failure message to keep").isNotEqualTo(b);
        assertThat(false).describedAs("Failure message to keep").isNotEqualTo(b);
    }

    public void refactorFalseCheckFirstArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(b).isEqualTo(false);
        Assertions.assertThat(b).as("Failure message to keep").isEqualTo(false);
        Assertions.assertThat(b).describedAs("Failure message to keep").isEqualTo(false);
        Assertions.assertThat(b).isNotEqualTo(false);
        Assertions.assertThat(b).as("Failure message to keep").isNotEqualTo(false);
        Assertions.assertThat(b).describedAs("Failure message to keep").isNotEqualTo(false);

        assertThat(b).isEqualTo(false);
        assertThat(b).as("Failure message to keep").isEqualTo(false);
        assertThat(b).describedAs("Failure message to keep").isEqualTo(false);
        assertThat(b).isNotEqualTo(false);
        assertThat(b).as("Failure message to keep").isNotEqualTo(false);
        assertThat(b).describedAs("Failure message to keep").isNotEqualTo(false);
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
        Assertions.assertThat(FOURTYTWO).isEqualTo(o);
        Assertions.assertThat(FOURTYTWO).as("Failure message to keep").isEqualTo(o);
        Assertions.assertThat(FOURTYTWO).describedAs("Failure message to keep").isEqualTo(o);
        Assertions.assertThat(FOURTYTWO).isNotEqualTo(o);
        Assertions.assertThat(FOURTYTWO).as("Failure message to keep").isNotEqualTo(o);
        Assertions.assertThat(FOURTYTWO).describedAs("Failure message to keep").isNotEqualTo(o);

        assertThat(FOURTYTWO).isEqualTo(o);
        assertThat(FOURTYTWO).as("Failure message to keep").isEqualTo(o);
        assertThat(FOURTYTWO).describedAs("Failure message to keep").isEqualTo(o);
        assertThat(FOURTYTWO).isNotEqualTo(o);
        assertThat(FOURTYTWO).as("Failure message to keep").isNotEqualTo(o);
        assertThat(FOURTYTWO).describedAs("Failure message to keep").isNotEqualTo(o);
    }

    public void doNotRefactorConstantAsExpectedArgInWithEquals(Object o) {
        Assertions.assertThat(o).isEqualTo(FOURTYTWO);
        Assertions.assertThat(o).as("Failure message to keep").isEqualTo(FOURTYTWO);
        Assertions.assertThat(o).describedAs("Failure message to keep").isEqualTo(FOURTYTWO);
        Assertions.assertThat(o).isNotEqualTo(FOURTYTWO);
        Assertions.assertThat(o).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotEqualTo(FOURTYTWO);

        assertThat(o).isEqualTo(FOURTYTWO);
        assertThat(o).as("Failure message to keep").isEqualTo(FOURTYTWO);
        assertThat(o).describedAs("Failure message to keep").isEqualTo(FOURTYTWO);
        assertThat(o).isNotEqualTo(FOURTYTWO);
        assertThat(o).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        assertThat(o).describedAs("Failure message to keep").isNotEqualTo(FOURTYTWO);
    }

    public void moveExpectedObjectAsExpectedArgWithEquals(Object o, int expected) {
        // Keep this comment
        Assertions.assertThat(expected).isEqualTo(o);
        Assertions.assertThat(expected).as("Failure message to keep").isEqualTo(o);
        Assertions.assertThat(expected).describedAs("Failure message to keep").isEqualTo(o);
        Assertions.assertThat(expected).isNotEqualTo(o);
        Assertions.assertThat(expected).as("Failure message to keep").isNotEqualTo(o);
        Assertions.assertThat(expected).describedAs("Failure message to keep").isNotEqualTo(o);

        assertThat(expected).isEqualTo(o);
        assertThat(expected).as("Failure message to keep").isEqualTo(o);
        assertThat(expected).describedAs("Failure message to keep").isEqualTo(o);
        assertThat(expected).isNotEqualTo(o);
        assertThat(expected).as("Failure message to keep").isNotEqualTo(o);
        assertThat(expected).describedAs("Failure message to keep").isNotEqualTo(o);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        Assertions.assertThat(expceted).isEqualTo(o);
        Assertions.assertThat(expceted).isNotEqualTo(o);
    }

    public void doNotRefactorExpectedObjectAsExpectedArgWithEquals(Object o, int expected) {
        Assertions.assertThat(o).isEqualTo(expected);
        Assertions.assertThat(o).as("Failure message to keep").isEqualTo(expected);
        Assertions.assertThat(o).describedAs("Failure message to keep").isEqualTo(expected);
        Assertions.assertThat(o).isNotEqualTo(expected);
        Assertions.assertThat(o).as("Failure message to keep").isNotEqualTo(expected);
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotEqualTo(expected);

        assertThat(o).isEqualTo(expected);
        assertThat(o).as("Failure message to keep").isEqualTo(expected);
        assertThat(o).describedAs("Failure message to keep").isEqualTo(expected);
        assertThat(o).isNotEqualTo(expected);
        assertThat(o).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(o).describedAs("Failure message to keep").isNotEqualTo(expected);

        int expceted = 0;
        Assertions.assertThat(o).isEqualTo(expceted);
        Assertions.assertThat(o).isNotEqualTo(expceted);
    }

    public void moveExpectedLongAsExpectedArgWithEquals(long l, long expected) {
        // Keep this comment
        Assertions.assertThat(expected).isEqualTo(l);
        Assertions.assertThat(expected).as("Failure message to keep").isEqualTo(l);
        Assertions.assertThat(expected).describedAs("Failure message to keep").isEqualTo(l);
        Assertions.assertThat(expected).isNotEqualTo(l);
        Assertions.assertThat(expected).as("Failure message to keep").isNotEqualTo(l);
        Assertions.assertThat(expected).describedAs("Failure message to keep").isNotEqualTo(l);

        assertThat(expected).isEqualTo(l);
        assertThat(expected).as("Failure message to keep").isEqualTo(l);
        assertThat(expected).describedAs("Failure message to keep").isEqualTo(l);
        assertThat(expected).isNotEqualTo(l);
        assertThat(expected).as("Failure message to keep").isNotEqualTo(l);
        assertThat(expected).describedAs("Failure message to keep").isNotEqualTo(l);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        Assertions.assertThat(expceted).isEqualTo(l);
        Assertions.assertThat(expceted).isNotEqualTo(l);
    }

    public void moveExpectedDoubleAsExpectedArgWithEquals(double d, double expected) {
        // Keep this comment
        Assertions.assertThat(expected).isEqualTo(d);
        Assertions.assertThat(expected).as("Failure message to keep").isEqualTo(d);
        Assertions.assertThat(expected).describedAs("Failure message to keep").isEqualTo(d);
        Assertions.assertThat(expected).isNotEqualTo(d);
        Assertions.assertThat(expected).as("Failure message to keep").isNotEqualTo(d);
        Assertions.assertThat(expected).describedAs("Failure message to keep").isNotEqualTo(d);

        assertThat(expected).isEqualTo(d);
        assertThat(expected).as("Failure message to keep").isEqualTo(d);
        assertThat(expected).describedAs("Failure message to keep").isEqualTo(d);
        assertThat(expected).isNotEqualTo(d);
        assertThat(expected).as("Failure message to keep").isNotEqualTo(d);
        assertThat(expected).describedAs("Failure message to keep").isNotEqualTo(d);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        Assertions.assertThat(expceted).isEqualTo(d);
        Assertions.assertThat(expceted).isNotEqualTo(d);
    }

    public void refactorIfOnBoolean(boolean b) {
        // Keep this comment
        if (b) {
            Fail.fail(null);
        }
        if (b) {
            Fail.fail("Failure message to keep");
        }

        if (!b) {
            Fail.fail(null);
        }
        if (!b) {
            Fail.fail("Failure message to keep");
        }
    }

    public void doNotRefactorUsedObject(Date nullDate) {
        if (nullDate != null) {
            Fail.fail("The date should be null: " + nullDate.getTime());
        }

        if (nullDate != null) {
            fail("The date should be null: " + nullDate.getTime());
        }
    }

    public void refactorNotUsedObject(Date nullDate, Date notNullDate) {
        // Keep this comment
        if (nullDate != null) {
            Fail.fail("The date should be null, not like: " + notNullDate.getTime());
        }

        if (nullDate != null) {
            fail("The date should be null, not like: " + notNullDate.getTime());
        }
    }

    public void refactorObjectInParameter(Date nullDate) {
        // Keep this comment
        if (nullDate != null) {
            Fail.fail("The date should be null, not like: {}", nullDate);
        }

        if (nullDate != null) {
            fail("The date should be null, not like: {}", nullDate);
        }
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
        // Keep this comment
        if (i1 != i2) {
            Fail.fail(null);
        }
        if (i1 != i2) {
            Fail.fail("Failure message to keep");
        }

        if (i1 != i2) {
            Fail.fail(null);
        }
        if (i1 != i2) {
            Fail.fail("Failure message to keep");
        }
    }

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        if (o1 == o2) {
            Fail.fail(null);
        }
        if (o1 == o2) {
            Fail.fail("Failure message to keep");
        }
        if (o1 != o2) {
            Fail.fail(null);
        }
        if (o1 != o2) {
            Fail.fail("Failure message to keep");
        }

        if (o1 == o2) {
            Assertions.fail(null);
        }
        if (o1 == o2) {
            Assertions.fail("Failure message to keep");
        }
        if (o1 != o2) {
            Assertions.fail(null);
        }
        if (o1 != o2) {
            Assertions.fail("Failure message to keep");
        }

        if (o1 == o2) {
            fail(null);
        }
        if (o1 == o2) {
            fail("Failure message to keep");
        }
        if (o1 != o2) {
            fail(null);
        }
        if (o1 != o2) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfNullThenFail(Object o1) {
        // Keep this comment
        if (o1 == null) {
            Fail.fail(null);
        }
        if (o1 == null) {
            Fail.fail("Failure message to keep");
        }
        if (o1 != null) {
            Fail.fail(null);
        }
        if (o1 != null) {
            Fail.fail("Failure message to keep");
        }
        if (null == o1) {
            Fail.fail(null);
        }
        if (null == o1) {
            Fail.fail("Failure message to keep");
        }
        if (null != o1) {
            Fail.fail(null);
        }
        if (null != o1) {
            Fail.fail("Failure message to keep");
        }

        if (o1 == null) {
            Assertions.fail(null);
        }
        if (o1 == null) {
            Assertions.fail("Failure message to keep");
        }
        if (o1 != null) {
            Assertions.fail(null);
        }
        if (o1 != null) {
            Assertions.fail("Failure message to keep");
        }
        if (null == o1) {
            Assertions.fail(null);
        }
        if (null == o1) {
            Assertions.fail("Failure message to keep");
        }
        if (null != o1) {
            Assertions.fail(null);
        }
        if (null != o1) {
            Assertions.fail("Failure message to keep");
        }

        if (o1 == null) {
            fail(null);
        }
        if (o1 == null) {
            fail("Failure message to keep");
        }
        if (o1 != null) {
            fail(null);
        }
        if (o1 != null) {
            fail("Failure message to keep");
        }
        if (null == o1) {
            fail(null);
        }
        if (null == o1) {
            fail("Failure message to keep");
        }
        if (null != o1) {
            fail(null);
        }
        if (null != o1) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        if (!o1.equals(o2)) {
            Fail.fail(null);
        }
        if (!o1.equals(o2)) {
            Fail.fail("Failure message to keep");
        }

        if (!o1.equals(o2)) {
            Assertions.fail(null);
        }
        if (!o1.equals(o2)) {
            Assertions.fail("Failure message to keep");
        }

        if (!o1.equals(o2)) {
            fail(null);
        }
        if (!o1.equals(o2)) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfLiteralThenFail(int i) {
        // Keep this comment
        if (i == 42) {
            Fail.fail(null);
        }
        if (i == 42) {
            Fail.fail("Failure message to keep");
        }
        if (i != 42) {
            Fail.fail(null);
        }
        if (i != 42) {
            Fail.fail("Failure message to keep");
        }
        if (42 == i) {
            Fail.fail(null);
        }
        if (42 == i) {
            Fail.fail("Failure message to keep");
        }
        if (42 != i) {
            Fail.fail(null);
        }
        if (42 != i) {
            Fail.fail("Failure message to keep");
        }

        if (i == 42) {
            Assertions.fail(null);
        }
        if (i == 42) {
            Assertions.fail("Failure message to keep");
        }
        if (i != 42) {
            Assertions.fail(null);
        }
        if (i != 42) {
            Assertions.fail("Failure message to keep");
        }
        if (42 == i) {
            Assertions.fail(null);
        }
        if (42 == i) {
            Assertions.fail("Failure message to keep");
        }
        if (42 != i) {
            Assertions.fail(null);
        }
        if (42 != i) {
            Assertions.fail("Failure message to keep");
        }

        if (i == 42) {
            fail(null);
        }
        if (i == 42) {
            fail("Failure message to keep");
        }
        if (i != 42) {
            fail(null);
        }
        if (i != 42) {
            fail("Failure message to keep");
        }
        if (42 == i) {
            fail(null);
        }
        if (42 == i) {
            fail("Failure message to keep");
        }
        if (42 != i) {
            fail(null);
        }
        if (42 != i) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfConstantThenFail(int i) {
        // Keep this comment
        if (i == FOURTYTWO) {
            Fail.fail(null);
        }
        if (i == FOURTYTWO) {
            Fail.fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            Fail.fail(null);
        }
        if (i != FOURTYTWO) {
            Fail.fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            Fail.fail(null);
        }
        if (FOURTYTWO == i) {
            Fail.fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            Fail.fail(null);
        }
        if (FOURTYTWO != i) {
            Fail.fail("Failure message to keep");
        }

        if (i == FOURTYTWO) {
            Assertions.fail(null);
        }
        if (i == FOURTYTWO) {
            Assertions.fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            Assertions.fail(null);
        }
        if (i != FOURTYTWO) {
            Assertions.fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            Assertions.fail(null);
        }
        if (FOURTYTWO == i) {
            Assertions.fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            Assertions.fail(null);
        }
        if (FOURTYTWO != i) {
            Assertions.fail("Failure message to keep");
        }

        if (i == FOURTYTWO) {
            fail(null);
        }
        if (i == FOURTYTWO) {
            fail("Failure message to keep");
        }
        if (i != FOURTYTWO) {
            fail(null);
        }
        if (i != FOURTYTWO) {
            fail("Failure message to keep");
        }
        if (FOURTYTWO == i) {
            fail(null);
        }
        if (FOURTYTWO == i) {
            fail("Failure message to keep");
        }
        if (FOURTYTWO != i) {
            fail(null);
        }
        if (FOURTYTWO != i) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfExpectedThenFail(int i, int expected) {
        // Keep this comment
        if (i == expected) {
            Fail.fail(null);
        }
        if (i == expected) {
            Fail.fail("Failure message to keep");
        }
        if (i != expected) {
            Fail.fail(null);
        }
        if (i != expected) {
            Fail.fail("Failure message to keep");
        }
        if (expected == i) {
            Fail.fail(null);
        }
        if (expected == i) {
            Fail.fail("Failure message to keep");
        }
        if (expected != i) {
            Fail.fail(null);
        }
        if (expected != i) {
            Fail.fail("Failure message to keep");
        }

        if (i == expected) {
            Assertions.fail(null);
        }
        if (i == expected) {
            Assertions.fail("Failure message to keep");
        }
        if (i != expected) {
            Assertions.fail(null);
        }
        if (i != expected) {
            Assertions.fail("Failure message to keep");
        }
        if (expected == i) {
            Assertions.fail(null);
        }
        if (expected == i) {
            Assertions.fail("Failure message to keep");
        }
        if (expected != i) {
            Assertions.fail(null);
        }
        if (expected != i) {
            Assertions.fail("Failure message to keep");
        }

        if (i == expected) {
            fail(null);
        }
        if (i == expected) {
            fail("Failure message to keep");
        }
        if (i != expected) {
            fail(null);
        }
        if (i != expected) {
            fail("Failure message to keep");
        }
        if (expected == i) {
            fail(null);
        }
        if (expected == i) {
            fail("Failure message to keep");
        }
        if (expected != i) {
            fail(null);
        }
        if (expected != i) {
            fail("Failure message to keep");
        }
    }

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
        // Keep this comment
        if (i == expectedI) {
            Fail.fail(null);
        }
        if (i == expectedI) {
            Fail.fail("Failure message to keep");
        }
        if (i != expectedI) {
            Fail.fail(null);
        }
        if (i != expectedI) {
            Fail.fail("Failure message to keep");
        }
        if (expectedI == i) {
            Fail.fail(null);
        }
        if (expectedI == i) {
            Fail.fail("Failure message to keep");
        }
        if (expectedI != i) {
            Fail.fail(null);
        }
        if (expectedI != i) {
            Fail.fail("Failure message to keep");
        }

        if (i == expectedI) {
            Assertions.fail(null);
        }
        if (i == expectedI) {
            Assertions.fail("Failure message to keep");
        }
        if (i != expectedI) {
            Assertions.fail(null);
        }
        if (i != expectedI) {
            Assertions.fail("Failure message to keep");
        }
        if (expectedI == i) {
            Assertions.fail(null);
        }
        if (expectedI == i) {
            Assertions.fail("Failure message to keep");
        }
        if (expectedI != i) {
            Assertions.fail(null);
        }
        if (expectedI != i) {
            Assertions.fail("Failure message to keep");
        }

        if (i == expectedI) {
            fail(null);
        }
        if (i == expectedI) {
            fail("Failure message to keep");
        }
        if (i != expectedI) {
            fail(null);
        }
        if (i != expectedI) {
            fail("Failure message to keep");
        }
        if (expectedI == i) {
            fail(null);
        }
        if (expectedI == i) {
            fail("Failure message to keep");
        }
        if (expectedI != i) {
            fail(null);
        }
        if (expectedI != i) {
            fail("Failure message to keep");
        }
    }

    public void doNotRefactorBecauseOfElseStatement(int i1, int i2, Object o1) {
        if (i1 == i2) {
            Fail.fail(null);
        } else {
            System.out.println("keep me!");
        }
        if (o1 == null) {
            Fail.fail(null);
        } else {
            System.out.println("keep me!");
        }
        Object o2 = i2;
        if (o1 == o2) {
            Fail.fail(null);
        } else {
            System.out.println("keep me!");
        }
        if (o1.equals(o2)) {
            Fail.fail(null);
        } else {
            System.out.println("keep me!");
        }
    }
}
