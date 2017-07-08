/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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

public class PushNegationDownSample {

    public boolean replaceDoubleNegation(boolean b) {
        return !!b;
    }

    public boolean reduceLiteralExpression() {
        boolean b = !Boolean.TRUE;
        return !Boolean.FALSE;
    }

    public boolean reduceConstantExpression() {
        boolean b = !true;
        return !false;
    }

    public boolean replaceDoubleNegationWithParentheses(boolean b) {
        return !(!(b /* another refactoring removes the parentheses */));
    }

    public boolean replaceNegationWithInfixAndOperator(boolean b1, boolean b2, boolean b3) {
        return !(b1 && b2 && b3); // another refactoring removes the parentheses
    }

    public boolean replaceNegationWithInfixOrOperator(boolean b1, boolean b2, boolean b3) {
        return !(b1 || b2 || b3); // another refactoring removes the parentheses
    }

    public boolean replaceNegationWithEqualOperator(boolean b1, boolean b2) {
        return !(b1 == b2); // another refactoring removes the parentheses
    }

    public boolean replaceNegationWithNotEqualOperator(boolean b1, boolean b2) {
        return !(b1 != b2); // another refactoring removes the parentheses
    }

    public boolean replaceNegationRevertInnerExpressions(boolean b1, boolean b2) {
        return !(!b1 && !b2 /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationLeaveParentheses(boolean b1, boolean b2) {
        return !(!(b1 && b2 /* another refactoring removes the parentheses */));
    }

    public boolean replaceNegationRemoveParentheses(boolean b1, boolean b2) {
        return !((!b1) && (!b2));
    }

    public boolean doNotNegateNonBooleanExprs(Object o) {
        return !(o != null /* another refactoring removes the parentheses */);
    }

    public boolean doNotNegateNonBooleanPrimitiveExprs(Boolean b) {
        return !(b != null /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationAndLessOperator(int i1, int i2) {
        return !(i1 < i2 /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationAndLessEqualOperator(int i1, int i2) {
        return !(i1 <= i2 /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationAndGreaterOperator(int i1, int i2) {
        return !(i1 > i2 /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationAndGreaterEqualOperator(int i1, int i2) {
        return !(i1 >= i2 /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationAndEqualOperator(int i1, int i2) {
        return !(i1 == i2 /* another refactoring removes the parentheses */);
    }

    public boolean replaceNegationAndNotEqualOperator(int i1, int i2) {
        return !(i1 != i2 /* another refactoring removes the parentheses */);
    }
}
