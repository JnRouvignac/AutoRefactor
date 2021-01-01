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
package org.autorefactor.refactoring.rules.samples_out;

import static org.assertj.core.api.Assertions.*;

import java.util.Date;

import org.assertj.core.api.Assertions;
import org.assertj.core.api.Fail;
import org.assertj.core.data.Offset;

public class AssertJSample {
    private static final int FOURTYTWO = 42;

    public void refactorWithPrimitives(int i1, int i2) {
        // Keep this comment
        Assertions.assertThat(i2).isEqualTo(i1);
        Assertions.assertThat(i2).as("Failure message to keep").isEqualTo(i1);
        Assertions.assertThat(i2).describedAs("Failure message to keep").isEqualTo(i1);
        Assertions.assertThat(i2).isEqualTo(i1);
        Assertions.assertThat(i2).as("Failure message to keep").isEqualTo(i1);
        Assertions.assertThat(i2).describedAs("Failure message to keep").isEqualTo(i1);

        assertThat(i2).isEqualTo(i1);
        assertThat(i2).as("Failure message to keep").isEqualTo(i1);
        assertThat(i2).describedAs("Failure message to keep").isEqualTo(i1);
        assertThat(i2).isEqualTo(i1);
        assertThat(i2).as("Failure message to keep").isEqualTo(i1);
        assertThat(i2).describedAs("Failure message to keep").isEqualTo(i1);
    }

    public void refactorWithDoubles(double d1, double d2) {
        // Keep this comment
        Assertions.assertThat(d2).isEqualTo(d1, Offset.offset(.0));
        Assertions.assertThat(d2).as("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
        Assertions.assertThat(d2).describedAs("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
        Assertions.assertThat(d2).isEqualTo(d1, Offset.offset(.0));
        Assertions.assertThat(d2).as("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
        Assertions.assertThat(d2).describedAs("Failure message to keep").isEqualTo(d1, Offset.offset(.0));

        assertThat(d2).isEqualTo(d1, Offset.offset(.0));
        assertThat(d2).as("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
        assertThat(d2).describedAs("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
        assertThat(d2).isEqualTo(d1, Offset.offset(.0));
        assertThat(d2).as("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
        assertThat(d2).describedAs("Failure message to keep").isEqualTo(d1, Offset.offset(.0));
    }

    public void refactorWithFloats(float f1, float f2) {
        // Keep this comment
        Assertions.assertThat(f2).isEqualTo(f1, Offset.offset(.0F));
        Assertions.assertThat(f2).as("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
        Assertions.assertThat(f2).describedAs("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
        Assertions.assertThat(f2).isEqualTo(f1, Offset.offset(.0F));
        Assertions.assertThat(f2).as("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
        Assertions.assertThat(f2).describedAs("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));

        assertThat(f2).isEqualTo(f1, Offset.offset(.0F));
        assertThat(f2).as("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
        assertThat(f2).describedAs("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
        assertThat(f2).isEqualTo(f1, Offset.offset(.0F));
        assertThat(f2).as("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
        assertThat(f2).describedAs("Failure message to keep").isEqualTo(f1, Offset.offset(.0F));
    }

    public void removeParenthesis(int i1, int i2, int i3, int i4) {
        // Keep this comment
        Assertions.assertThat(i3 + i4).isEqualTo(i1 + i2);
        Assertions.assertThat(i3 + i4).as("Failure message to keep").isEqualTo(i1 + i2);
        Assertions.assertThat(i3 + i4).describedAs("Failure message to keep").isEqualTo(i1 + i2);
        Assertions.assertThat(i3 + i4).isEqualTo(i1 + i2);
        Assertions.assertThat(i3 + i4).as("Failure message to keep").isEqualTo(i1 + i2);
        Assertions.assertThat(i3 + i4).describedAs("Failure message to keep").isEqualTo(i1 + i2);

        assertThat(i3 + i4).isEqualTo(i1 + i2);
        assertThat(i3 + i4).as("Failure message to keep").isEqualTo(i1 + i2);
        assertThat(i3 + i4).describedAs("Failure message to keep").isEqualTo(i1 + i2);
        assertThat(i3 + i4).isEqualTo(i1 + i2);
        assertThat(i3 + i4).as("Failure message to keep").isEqualTo(i1 + i2);
        assertThat(i3 + i4).describedAs("Failure message to keep").isEqualTo(i1 + i2);
    }

    public void refactorFailures() {
        // Keep this comment
        Assertions.fail(null);
        Assertions.fail("Failure message to keep");
        Assertions.fail("Failure message to keep");
        Assertions.fail(null);
        Assertions.fail("Failure message to keep");
        Assertions.fail("Failure message to keep");

        fail(null);
        fail("Failure message to keep");
        fail("Failure message to keep");
        fail(null);
        fail("Failure message to keep");
        fail("Failure message to keep");
    }

    public void removeDeadChecks() {
    }

    public void replaceAssertByBlocks(boolean b) {
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
        if (b) {
        }
    }

    public void refactorNegatedConditions(boolean b) {
        // Keep this comment
        Assertions.assertThat(b).isFalse();
        Assertions.assertThat(b).as("Failure message to keep").isFalse();
        Assertions.assertThat(b).describedAs("Failure message to keep").isFalse();
        Assertions.assertThat(b).isTrue();
        Assertions.assertThat(b).as("Failure message to keep").isTrue();
        Assertions.assertThat(b).describedAs("Failure message to keep").isTrue();

        assertThat(b).isFalse();
        assertThat(b).as("Failure message to keep").isFalse();
        assertThat(b).describedAs("Failure message to keep").isFalse();
        assertThat(b).isTrue();
        assertThat(b).as("Failure message to keep").isTrue();
        assertThat(b).describedAs("Failure message to keep").isTrue();
    }

    public void refactorWithObjectReferences(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertThat(o2).isSameAs(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isSameAs(o1);
        Assertions.assertThat(o2).describedAs("Failure message to keep").isSameAs(o1);
        Assertions.assertThat(o2).isNotSameAs(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        Assertions.assertThat(o2).describedAs("Failure message to keep").isNotSameAs(o1);
        Assertions.assertThat(o2).isSameAs(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isSameAs(o1);
        Assertions.assertThat(o2).describedAs("Failure message to keep").isSameAs(o1);
        Assertions.assertThat(o2).isNotSameAs(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        Assertions.assertThat(o2).describedAs("Failure message to keep").isNotSameAs(o1);

        assertThat(o2).isSameAs(o1);
        assertThat(o2).as("Failure message to keep").isSameAs(o1);
        assertThat(o2).describedAs("Failure message to keep").isSameAs(o1);
        assertThat(o2).isNotSameAs(o1);
        assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        assertThat(o2).describedAs("Failure message to keep").isNotSameAs(o1);
        assertThat(o2).isSameAs(o1);
        assertThat(o2).as("Failure message to keep").isSameAs(o1);
        assertThat(o2).describedAs("Failure message to keep").isSameAs(o1);
        assertThat(o2).isNotSameAs(o1);
        assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        assertThat(o2).describedAs("Failure message to keep").isNotSameAs(o1);
    }

    public void refactorWithObjects(Object o1, Object o2) {
        // Keep this comment
        Assertions.assertThat(o2).isEqualTo(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isEqualTo(o1);
        Assertions.assertThat(o2).describedAs("Failure message to keep").isEqualTo(o1);
        Assertions.assertThat(o2).isEqualTo(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isEqualTo(o1);
        Assertions.assertThat(o2).describedAs("Failure message to keep").isEqualTo(o1);

        assertThat(o2).isEqualTo(o1);
        assertThat(o2).as("Failure message to keep").isEqualTo(o1);
        assertThat(o2).describedAs("Failure message to keep").isEqualTo(o1);
        assertThat(o2).isEqualTo(o1);
        assertThat(o2).as("Failure message to keep").isEqualTo(o1);
        assertThat(o2).describedAs("Failure message to keep").isEqualTo(o1);
    }

    public void refactorNullCheckFirstArg(Object o) {
        // Keep this comment
        Assertions.assertThat(o).isNull();
        Assertions.assertThat(o).as("Failure message to keep").isNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNull();
        Assertions.assertThat(o).isNotNull();
        Assertions.assertThat(o).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotNull();
        Assertions.assertThat(o).isNull();
        Assertions.assertThat(o).as("Failure message to keep").isNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNull();
        Assertions.assertThat(o).isNotNull();
        Assertions.assertThat(o).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotNull();

        assertThat(o).isNull();
        assertThat(o).as("Failure message to keep").isNull();
        assertThat(o).describedAs("Failure message to keep").isNull();
        assertThat(o).isNotNull();
        assertThat(o).as("Failure message to keep").isNotNull();
        assertThat(o).describedAs("Failure message to keep").isNotNull();
        assertThat(o).isNull();
        assertThat(o).as("Failure message to keep").isNull();
        assertThat(o).describedAs("Failure message to keep").isNull();
        assertThat(o).isNotNull();
        assertThat(o).as("Failure message to keep").isNotNull();
        assertThat(o).describedAs("Failure message to keep").isNotNull();
    }

    public void refactorNullCheckSecondArg(Object o) {
        // Keep this comment
        Assertions.assertThat(o).isNull();
        Assertions.assertThat(o).as("Failure message to keep").isNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNull();
        Assertions.assertThat(o).isNotNull();
        Assertions.assertThat(o).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotNull();
        Assertions.assertThat(o).isNull();
        Assertions.assertThat(o).as("Failure message to keep").isNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNull();
        Assertions.assertThat(o).isNotNull();
        Assertions.assertThat(o).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotNull();

        assertThat(o).isNull();
        assertThat(o).as("Failure message to keep").isNull();
        assertThat(o).describedAs("Failure message to keep").isNull();
        assertThat(o).isNotNull();
        assertThat(o).as("Failure message to keep").isNotNull();
        assertThat(o).describedAs("Failure message to keep").isNotNull();
        assertThat(o).isNull();
        assertThat(o).as("Failure message to keep").isNull();
        assertThat(o).describedAs("Failure message to keep").isNull();
        assertThat(o).isNotNull();
        assertThat(o).as("Failure message to keep").isNotNull();
        assertThat(o).describedAs("Failure message to keep").isNotNull();
    }

    public void refactorNullCheckFirstArgWithEquals(Object o) {
        // Keep this comment
        Assertions.assertThat(o).isNull();
        Assertions.assertThat(o).as("Failure message to keep").isNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNull();
        Assertions.assertThat(o).isNull();
        Assertions.assertThat(o).as("Failure message to keep").isNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNull();
        Assertions.assertThat(o).isNotNull();
        Assertions.assertThat(o).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o).describedAs("Failure message to keep").isNotNull();

        assertThat(o).isNull();
        assertThat(o).as("Failure message to keep").isNull();
        assertThat(o).describedAs("Failure message to keep").isNull();
        assertThat(o).isNull();
        assertThat(o).as("Failure message to keep").isNull();
        assertThat(o).describedAs("Failure message to keep").isNull();
        assertThat(o).isNotNull();
        assertThat(o).as("Failure message to keep").isNotNull();
        assertThat(o).describedAs("Failure message to keep").isNotNull();
    }

    public void moveLiteralAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
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

    public void moveLongLiteral(long l) {
        // Keep this comment
        Assertions.assertThat(l).isEqualTo(42L);
        Assertions.assertThat(l).as("Failure message to keep").isEqualTo(42L);
        Assertions.assertThat(l).describedAs("Failure message to keep").isEqualTo(42L);
        Assertions.assertThat(l).isNotEqualTo(42L);
        Assertions.assertThat(l).as("Failure message to keep").isNotEqualTo(42L);
        Assertions.assertThat(l).describedAs("Failure message to keep").isNotEqualTo(42L);

        assertThat(l).isEqualTo(42L);
        assertThat(l).as("Failure message to keep").isEqualTo(42L);
        assertThat(l).describedAs("Failure message to keep").isEqualTo(42L);
        assertThat(l).isNotEqualTo(42L);
        assertThat(l).as("Failure message to keep").isNotEqualTo(42L);
        assertThat(l).describedAs("Failure message to keep").isNotEqualTo(42L);
    }

    public void moveIntegerLiteral(int i) {
        // Keep this comment
        Assertions.assertThat(i).isEqualTo(42);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(42);
        Assertions.assertThat(i).describedAs("Failure message to keep").isEqualTo(42);
        Assertions.assertThat(i).isNotEqualTo(42);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        Assertions.assertThat(i).describedAs("Failure message to keep").isNotEqualTo(42);

        assertThat(i).isEqualTo(42);
        assertThat(i).as("Failure message to keep").isEqualTo(42);
        assertThat(i).describedAs("Failure message to keep").isEqualTo(42);
        assertThat(i).isNotEqualTo(42);
        assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        assertThat(i).describedAs("Failure message to keep").isNotEqualTo(42);
    }

    public void moveCharacterLiteral(char c) {
        // Keep this comment
        Assertions.assertThat(c).isEqualTo('a');
        Assertions.assertThat(c).as("Failure message to keep").isEqualTo('a');
        Assertions.assertThat(c).describedAs("Failure message to keep").isEqualTo('a');
        Assertions.assertThat(c).isNotEqualTo('a');
        Assertions.assertThat(c).as("Failure message to keep").isNotEqualTo('a');
        Assertions.assertThat(c).describedAs("Failure message to keep").isNotEqualTo('a');

        assertThat(c).isEqualTo('a');
        assertThat(c).as("Failure message to keep").isEqualTo('a');
        assertThat(c).describedAs("Failure message to keep").isEqualTo('a');
        assertThat(c).isNotEqualTo('a');
        assertThat(c).as("Failure message to keep").isNotEqualTo('a');
        assertThat(c).describedAs("Failure message to keep").isNotEqualTo('a');
    }

    public void moveExpression(int i) {
        // Keep this comment
        Assertions.assertThat(i).isEqualTo(1 + 2 + 3);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(1 + 2 + 3);
        Assertions.assertThat(i).describedAs("Failure message to keep").isEqualTo(1 + 2 + 3);
        Assertions.assertThat(i).isNotEqualTo(1 + 2 + 3);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(1 + 2 + 3);
        Assertions.assertThat(i).describedAs("Failure message to keep").isNotEqualTo(1 + 2 + 3);

        assertThat(i).isEqualTo(1 + 2 + 3);
        assertThat(i).as("Failure message to keep").isEqualTo(1 + 2 + 3);
        assertThat(i).describedAs("Failure message to keep").isEqualTo(1 + 2 + 3);
        assertThat(i).isNotEqualTo(1 + 2 + 3);
        assertThat(i).as("Failure message to keep").isNotEqualTo(1 + 2 + 3);
        assertThat(i).describedAs("Failure message to keep").isNotEqualTo(1 + 2 + 3);
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
        Assertions.assertThat(b).isTrue();
        Assertions.assertThat(b).as("Failure message to keep").isTrue();
        Assertions.assertThat(b).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(b).isFalse();
        Assertions.assertThat(b).as("Failure message to keep").isFalse();
        Assertions.assertThat(b).describedAs("Failure message to keep").isFalse();

        assertThat(b).isTrue();
        assertThat(b).as("Failure message to keep").isTrue();
        assertThat(b).describedAs("Failure message to keep").isTrue();
        assertThat(b).isFalse();
        assertThat(b).as("Failure message to keep").isFalse();
        assertThat(b).describedAs("Failure message to keep").isFalse();
    }

    public void refactorTrueCheckFirstArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(b).isTrue();
        Assertions.assertThat(b).as("Failure message to keep").isTrue();
        Assertions.assertThat(b).describedAs("Failure message to keep").isTrue();
        Assertions.assertThat(b).isFalse();
        Assertions.assertThat(b).as("Failure message to keep").isFalse();
        Assertions.assertThat(b).describedAs("Failure message to keep").isFalse();

        assertThat(b).isTrue();
        assertThat(b).as("Failure message to keep").isTrue();
        assertThat(b).describedAs("Failure message to keep").isTrue();
        assertThat(b).isFalse();
        assertThat(b).as("Failure message to keep").isFalse();
        assertThat(b).describedAs("Failure message to keep").isFalse();
    }

    public void refactorFalseCheckSecondArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(b).isFalse();
        Assertions.assertThat(b).as("Failure message to keep").isFalse();
        Assertions.assertThat(b).describedAs("Failure message to keep").isFalse();
        Assertions.assertThat(b).isTrue();
        Assertions.assertThat(b).as("Failure message to keep").isTrue();
        Assertions.assertThat(b).describedAs("Failure message to keep").isTrue();

        assertThat(b).isFalse();
        assertThat(b).as("Failure message to keep").isFalse();
        assertThat(b).describedAs("Failure message to keep").isFalse();
        assertThat(b).isTrue();
        assertThat(b).as("Failure message to keep").isTrue();
        assertThat(b).describedAs("Failure message to keep").isTrue();
    }

    public void refactorFalseCheckFirstArgWithEquals(boolean b) {
        // Keep this comment
        Assertions.assertThat(b).isFalse();
        Assertions.assertThat(b).as("Failure message to keep").isFalse();
        Assertions.assertThat(b).describedAs("Failure message to keep").isFalse();
        Assertions.assertThat(b).isTrue();
        Assertions.assertThat(b).as("Failure message to keep").isTrue();
        Assertions.assertThat(b).describedAs("Failure message to keep").isTrue();

        assertThat(b).isFalse();
        assertThat(b).as("Failure message to keep").isFalse();
        assertThat(b).describedAs("Failure message to keep").isFalse();
        assertThat(b).isTrue();
        assertThat(b).as("Failure message to keep").isTrue();
        assertThat(b).describedAs("Failure message to keep").isTrue();
    }

    public void moveConstantAsExpectedArgInWithEquals(Object o) {
        // Keep this comment
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

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        Assertions.assertThat(o).isEqualTo(expceted);
        Assertions.assertThat(o).isNotEqualTo(expceted);
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
        Assertions.assertThat(l).isEqualTo(expected);
        Assertions.assertThat(l).as("Failure message to keep").isEqualTo(expected);
        Assertions.assertThat(l).describedAs("Failure message to keep").isEqualTo(expected);
        Assertions.assertThat(l).isNotEqualTo(expected);
        Assertions.assertThat(l).as("Failure message to keep").isNotEqualTo(expected);
        Assertions.assertThat(l).describedAs("Failure message to keep").isNotEqualTo(expected);

        assertThat(l).isEqualTo(expected);
        assertThat(l).as("Failure message to keep").isEqualTo(expected);
        assertThat(l).describedAs("Failure message to keep").isEqualTo(expected);
        assertThat(l).isNotEqualTo(expected);
        assertThat(l).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(l).describedAs("Failure message to keep").isNotEqualTo(expected);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        Assertions.assertThat(l).isEqualTo(expceted);
        Assertions.assertThat(l).isNotEqualTo(expceted);
    }

    public void moveExpectedDoubleAsExpectedArgWithEquals(double d, double expected) {
        // Keep this comment
        Assertions.assertThat(d).isEqualTo(expected, Offset.offset(.0));
        Assertions.assertThat(d).as("Failure message to keep").isEqualTo(expected, Offset.offset(.0));
        Assertions.assertThat(d).describedAs("Failure message to keep").isEqualTo(expected, Offset.offset(.0));
        Assertions.assertThat(d).isNotEqualTo(expected);
        Assertions.assertThat(d).as("Failure message to keep").isNotEqualTo(expected);
        Assertions.assertThat(d).describedAs("Failure message to keep").isNotEqualTo(expected);

        assertThat(d).isEqualTo(expected, Offset.offset(.0));
        assertThat(d).as("Failure message to keep").isEqualTo(expected, Offset.offset(.0));
        assertThat(d).describedAs("Failure message to keep").isEqualTo(expected, Offset.offset(.0));
        assertThat(d).isNotEqualTo(expected);
        assertThat(d).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(d).describedAs("Failure message to keep").isNotEqualTo(expected);

        // Tests that this works according to levenshtein distance
        int expceted = 0;
        Assertions.assertThat(d).isEqualTo(expceted);
        Assertions.assertThat(d).isNotEqualTo(expceted);
    }

    public void refactorIfOnBoolean(boolean b) {
        // Keep this comment
        assertThat(b).isFalse();
        assertThat(b).as("Failure message to keep").isFalse();

        assertThat(b).isTrue();
        assertThat(b).as("Failure message to keep").isTrue();
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
        assertThat(nullDate).as("The date should be null, not like: " + notNullDate.getTime()).isNull();

        assertThat(nullDate).as("The date should be null, not like: " + notNullDate.getTime()).isNull();
    }

    public void refactorObjectInParameter(Date nullDate) {
        // Keep this comment
        assertThat(nullDate).as("The date should be null, not like: {}", nullDate).isNull();

        assertThat(nullDate).as("The date should be null, not like: {}", nullDate).isNull();
    }

    public void refactorIfPrimitiveThenFail(int i1, int i2) {
        // Keep this comment
        assertThat(i2).isEqualTo(i1);
        assertThat(i2).as("Failure message to keep").isEqualTo(i1);

        assertThat(i2).isEqualTo(i1);
        assertThat(i2).as("Failure message to keep").isEqualTo(i1);
    }

    public void refactorIfSameObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        assertThat(o2).isNotSameAs(o1);
        assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        assertThat(o2).isSameAs(o1);
        assertThat(o2).as("Failure message to keep").isSameAs(o1);

        Assertions.assertThat(o2).isNotSameAs(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        Assertions.assertThat(o2).isSameAs(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isSameAs(o1);

        assertThat(o2).isNotSameAs(o1);
        assertThat(o2).as("Failure message to keep").isNotSameAs(o1);
        assertThat(o2).isSameAs(o1);
        assertThat(o2).as("Failure message to keep").isSameAs(o1);
    }

    public void refactorIfNullThenFail(Object o1) {
        // Keep this comment
        assertThat(o1).isNotNull();
        assertThat(o1).as("Failure message to keep").isNotNull();
        assertThat(o1).isNull();
        assertThat(o1).as("Failure message to keep").isNull();
        assertThat(o1).isNotNull();
        assertThat(o1).as("Failure message to keep").isNotNull();
        assertThat(o1).isNull();
        assertThat(o1).as("Failure message to keep").isNull();

        Assertions.assertThat(o1).isNotNull();
        Assertions.assertThat(o1).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o1).isNull();
        Assertions.assertThat(o1).as("Failure message to keep").isNull();
        Assertions.assertThat(o1).isNotNull();
        Assertions.assertThat(o1).as("Failure message to keep").isNotNull();
        Assertions.assertThat(o1).isNull();
        Assertions.assertThat(o1).as("Failure message to keep").isNull();

        assertThat(o1).isNotNull();
        assertThat(o1).as("Failure message to keep").isNotNull();
        assertThat(o1).isNull();
        assertThat(o1).as("Failure message to keep").isNull();
        assertThat(o1).isNotNull();
        assertThat(o1).as("Failure message to keep").isNotNull();
        assertThat(o1).isNull();
        assertThat(o1).as("Failure message to keep").isNull();
    }

    public void refactorIfObjectThenFail(Object o1, Object o2) {
        // Keep this comment
        assertThat(o2).isEqualTo(o1);
        assertThat(o2).as("Failure message to keep").isEqualTo(o1);

        Assertions.assertThat(o2).isEqualTo(o1);
        Assertions.assertThat(o2).as("Failure message to keep").isEqualTo(o1);

        assertThat(o2).isEqualTo(o1);
        assertThat(o2).as("Failure message to keep").isEqualTo(o1);
    }

    public void refactorIfLiteralThenFail(int i) {
        // Keep this comment
        assertThat(i).isNotEqualTo(42);
        assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        assertThat(i).isEqualTo(42);
        assertThat(i).as("Failure message to keep").isEqualTo(42);
        assertThat(i).isNotEqualTo(42);
        assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        assertThat(i).isEqualTo(42);
        assertThat(i).as("Failure message to keep").isEqualTo(42);

        Assertions.assertThat(i).isNotEqualTo(42);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        Assertions.assertThat(i).isEqualTo(42);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(42);
        Assertions.assertThat(i).isNotEqualTo(42);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        Assertions.assertThat(i).isEqualTo(42);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(42);

        assertThat(i).isNotEqualTo(42);
        assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        assertThat(i).isEqualTo(42);
        assertThat(i).as("Failure message to keep").isEqualTo(42);
        assertThat(i).isNotEqualTo(42);
        assertThat(i).as("Failure message to keep").isNotEqualTo(42);
        assertThat(i).isEqualTo(42);
        assertThat(i).as("Failure message to keep").isEqualTo(42);
    }

    public void refactorIfConstantThenFail(int i) {
        // Keep this comment
        assertThat(i).isNotEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        assertThat(i).isEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isEqualTo(FOURTYTWO);
        assertThat(i).isNotEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        assertThat(i).isEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isEqualTo(FOURTYTWO);

        Assertions.assertThat(i).isNotEqualTo(FOURTYTWO);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        Assertions.assertThat(i).isEqualTo(FOURTYTWO);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(FOURTYTWO);
        Assertions.assertThat(i).isNotEqualTo(FOURTYTWO);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        Assertions.assertThat(i).isEqualTo(FOURTYTWO);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(FOURTYTWO);

        assertThat(i).isNotEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        assertThat(i).isEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isEqualTo(FOURTYTWO);
        assertThat(i).isNotEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isNotEqualTo(FOURTYTWO);
        assertThat(i).isEqualTo(FOURTYTWO);
        assertThat(i).as("Failure message to keep").isEqualTo(FOURTYTWO);
    }

    public void refactorIfExpectedThenFail(int i, int expected) {
        // Keep this comment
        assertThat(i).isNotEqualTo(expected);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(i).isEqualTo(expected);
        assertThat(i).as("Failure message to keep").isEqualTo(expected);
        assertThat(i).isNotEqualTo(expected);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(i).isEqualTo(expected);
        assertThat(i).as("Failure message to keep").isEqualTo(expected);

        Assertions.assertThat(i).isNotEqualTo(expected);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(expected);
        Assertions.assertThat(i).isEqualTo(expected);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(expected);
        Assertions.assertThat(i).isNotEqualTo(expected);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(expected);
        Assertions.assertThat(i).isEqualTo(expected);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(expected);

        assertThat(i).isNotEqualTo(expected);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(i).isEqualTo(expected);
        assertThat(i).as("Failure message to keep").isEqualTo(expected);
        assertThat(i).isNotEqualTo(expected);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expected);
        assertThat(i).isEqualTo(expected);
        assertThat(i).as("Failure message to keep").isEqualTo(expected);
    }

    public void refactorIfNearlyExpectedThenFail(int i, int expectedI) {
        // Keep this comment
        assertThat(i).isNotEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expectedI);
        assertThat(i).isEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isEqualTo(expectedI);
        assertThat(i).isNotEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expectedI);
        assertThat(i).isEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isEqualTo(expectedI);

        Assertions.assertThat(i).isNotEqualTo(expectedI);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(expectedI);
        Assertions.assertThat(i).isEqualTo(expectedI);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(expectedI);
        Assertions.assertThat(i).isNotEqualTo(expectedI);
        Assertions.assertThat(i).as("Failure message to keep").isNotEqualTo(expectedI);
        Assertions.assertThat(i).isEqualTo(expectedI);
        Assertions.assertThat(i).as("Failure message to keep").isEqualTo(expectedI);

        assertThat(i).isNotEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expectedI);
        assertThat(i).isEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isEqualTo(expectedI);
        assertThat(i).isNotEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isNotEqualTo(expectedI);
        assertThat(i).isEqualTo(expectedI);
        assertThat(i).as("Failure message to keep").isEqualTo(expectedI);
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
