/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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

public class BooleanSample {

    public boolean f;
    public Boolean g;

    public void useBooleanConstants() {
        // Keep this comment
        Boolean b1 = Boolean.TRUE;
        Boolean b2 = Boolean.FALSE;
    }

    // TODO JNR handle mix and match of boolean primitives and boolean objects

    public boolean returnIfConditionBooleanPrimitive(boolean b) {
        // Keep this comment
        return b;
    }

    public boolean returnIfConditionBooleanPrimitive2(boolean b) {
        // Keep this comment
        return !b;
    }

    public boolean returnIfConditionWithInfixExpressionBooleanPrimitive(int i) {
        // Keep this comment
        return !(0 < i && i < 12);
    }

    public boolean returnIfConditionWithInstanceofExpressionBooleanPrimitive(Object o) {
        // Keep this comment
        return !(o instanceof String);
    }

    public boolean returnIfConditionAddCurlyBraces(Object o) {
        // Keep this comment
        return (o instanceof Integer) || (!(o instanceof String));
    }

    public boolean returnIfConditionThatRevertsInstanceofExpressionBooleanPrimitive(Object o) {
        // Keep this comment
        return /* do not lose me */o instanceof String;
    }

    public Boolean returnIfConditionBooleanObject(boolean b) {
        // Keep this comment
        return Boolean.valueOf(b);
    }

    public Boolean returnIfConditionBooleanObject2(boolean b) {
        // Keep this comment
        return Boolean.valueOf(!b);
    }

    public boolean returnIfConditionMixedBoolean1(boolean b) {
        // Keep this comment
        return Boolean.valueOf(b);
    }

    public boolean returnIfConditionMixedBoolean2(boolean b) {
        // Keep this comment
        return b;
    }

    public boolean returnIfConditionBooleanPrimitive3(boolean b) {
        // Keep this comment
        return b;
    }

    public boolean returnIfConditionBooleanPrimitive4(boolean b) {
        // Keep this comment
        return !b;
    }

    public boolean returnIfConditionBooleanObject3(boolean b) {
        // Keep this comment
        return b;
    }

    public boolean returnIfConditionBooleanObject4(boolean b) {
        // Keep this comment
        return !b;
    }

    public boolean removeUselessTernaryOperatorWithBooleanPrimitive1(boolean bo) {
        // Keep this comment
        boolean b = bo;
        return b;
    }

    public boolean removeUselessTernaryOperatorWithBooleanPrimitive2(boolean bo) {
        // Keep this comment
        boolean b = !bo;
        return b;
    }

    public Boolean removeUselessTernaryOperatorWithBooleanObject1(boolean bo) {
        // Keep this comment
        Boolean b = Boolean.valueOf(bo);
        return b;
    }

    public Boolean removeUselessTernaryOperatorWithBooleanObject2(boolean bo) {
        // Keep this comment
        Boolean b = Boolean.valueOf(!bo);
        return b;
    }

    public boolean replaceTernaryOperatorByAndOperator(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean b = bo1 && bo2;
        return b;
    }

    public boolean replaceTernaryOperatorByAndOperator2(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean b = !bo1 && bo2;
        return b;
    }

    public boolean replaceTernaryOperatorByOrOperator(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean b = bo1 || bo2;
        return b;
    }

    public boolean replaceTernaryOperatorByOrOperator2(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean b = !bo1 || bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObjectConstant(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean b = bo1 && bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObjectConstant2(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean b = !bo1 && bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObjectConstant(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean b = bo1 || bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObjectConstant2(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean b = !bo1 || bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObject(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean b = bo1 && bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObject2(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean b = !bo1 && bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObject(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean b = bo1 || bo2;
        return b;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObject2(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean b = !bo1 || bo2;
        return b;
    }

    public void doNotReplacePossibleNullObject(Boolean bo1, Boolean bo2) {
        Boolean b = bo1 ? bo2 : Boolean.FALSE;
        b = bo1 ? Boolean.FALSE : bo2;
        b = bo1 ? Boolean.TRUE : bo2;
        b = bo1 ? bo2 : Boolean.TRUE;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithExpression(int i1, int i2) {
        // Keep this comment
        Boolean b = (i1 == 1) && (i2 == 2);
        return b;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithExpression2(int i1, int i2) {
        // Keep this comment
        Boolean b = !(i1 == 1) && (i2 == 2);
        return b;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithExpression(int i1, int i2) {
        // Keep this comment
        Boolean b = (i1 == 1) || (i2 == 2);
        return b;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithExpression2(int i1, int i2) {
        // Keep this comment
        Boolean b = !(i1 == 1) || (i2 == 2);
        return b;
    }

    public void doNotRemoveIfInBooleanPrimitiveAssignment1(boolean bo) {
        boolean b = true;
        if (bo) {
            b = false;
        } else {
            System.out.println();
        }
    }

    public void removeUselessIfInBooleanPrimitiveAssignment1(boolean bo) {
        // Keep this comment
        boolean b = !bo;
    }

    public void removeUselessIfInBooleanPrimitiveAssignment2(boolean bo) {
        // Keep this comment
        boolean b = bo;
    }

    public void removeUselessIfInBooleanObjectAssignment1(boolean bo) {
        // Keep this comment
        Boolean b = Boolean.valueOf(!bo);
    }

    public void removeUselessIfInBooleanObjectAssignment2(boolean bo) {
        // Keep this comment
        boolean b = bo;
    }

    public void removeUselessIfInBooleanPrimitiveAssignment3(boolean bo,
            boolean b) {
        // Keep this comment
        b = !bo;
    }

    public void removeUselessIfInBooleanPrimitiveAssignment4(boolean bo,
            boolean b) {
        // Keep this comment
        b = bo;
    }

    public void removeUselessIfInBooleanPrimitiveAssignmentSearchFurtherAwayForPreviousSibling(
            boolean bo, boolean b) {
        b = false;
        char c = 'a';
        byte by = 0;
        double d = 0.0;
        if (bo) {
            b = true;
        }
    }

    public void removeUselessIfInBooleanObjectAssignment3(boolean bo, Boolean b) {
        // Keep this comment
        b = Boolean.valueOf(!bo);
    }

    public void removeUselessIfInBooleanObjectAssignment4(boolean bo, Boolean b) {
        // Keep this comment
        b = Boolean.valueOf(bo);
    }

    public void removeUselessIfInBooleanPrimitiveAssignment5(boolean bo) {
        // Keep this comment
        this.f = bo;
    }

    public void removeUselessIfInBooleanObjectAssignment5(boolean bo) {
        // Keep this comment
        this.g = Boolean.valueOf(bo);
    }

    public void removeUselessIfInBooleanPrimitiveAssignment6(boolean bo) {
        // Keep this comment
        f = bo;
    }

    public void removeUselessIfInBooleanObjectAssignment6(boolean bo) {
        // Keep this comment
        g = Boolean.valueOf(bo);
    }

    public void removeUselessIfInBooleanObjectAssignment7(boolean bo) {
        // Keep this comment
        BooleanSample.this.g = Boolean.valueOf(bo);
    }

    // TODO redo the next 4 with Boolean object

    public boolean removeUselessIfInBooleanPrimitiveAssignment7(boolean bo) {
        // Keep this comment
        return (bo) && (aMethodThatReturnsBoolean());
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment8(boolean bo) {
        // Keep this comment
        return !(bo) || (aMethodThatReturnsBoolean());
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment9(boolean bo) {
        // Keep this comment
        return !(bo) && (aMethodThatReturnsBoolean());
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment10(boolean bo) {
        // Keep this comment
        return (bo) || (aMethodThatReturnsBoolean());
    }

    public void removeUselessTernaryOperatorWithBooleanPrimitive(boolean bo) {
        aMethodThatAcceptsABoolean(bo);
        aMethodThatAcceptsABoolean(!bo);
        aMethodThatAcceptsABoolean(Boolean.valueOf(bo));
        aMethodThatAcceptsABoolean(Boolean.valueOf(!bo));
    }

    public void removeUselessIfInBooleanPrimitiveExpression10(boolean bo) {
        // Keep this comment
        aMethodThatAcceptsABoolean(bo);
    }

    public void removeUselessIfInBooleanPrimitiveExpression11(boolean bo) {
        // Keep this comment
        aMethodThatAcceptsABoolean(!bo);
    }

    public void removeUselessIfInBooleanPrimitiveExpression12(boolean bo) {
        if (bo) {
            aMethodThatAcceptsABoolean(true);
        } else {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(bo || aMethodThatReturnsBoolean());
    }

    public void removeUselessIfInBooleanPrimitiveExpression13(boolean bo) {
        if (bo) {
            aMethodThatAcceptsABoolean(false);
        } else {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(!bo && aMethodThatReturnsBoolean());
    }

    public void removeUselessIfInBooleanPrimitiveExpression14(boolean bo) {
        if (bo) {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        } else {
            aMethodThatAcceptsABoolean(true);
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(!bo || aMethodThatReturnsBoolean());
    }

    public void removeUselessIfInBooleanPrimitiveExpression15(boolean bo) {
        if (bo) {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        } else {
            aMethodThatAcceptsABoolean(false);
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(bo && aMethodThatReturnsBoolean());
    }

    public boolean invertConditionalExpression(int i, boolean res1, boolean res2) {
        // Keep this comment
        return !(i == 0 ? res1 : res2);
    }

    public boolean invertAssignment(boolean b1, boolean b2) {
        // Keep this comment
        return !(b1 = b2);
    }

    public boolean invertCast(Object o) {
        // Keep this comment
        return !(Boolean) o;
    }

    public boolean doNotRefactor(Object o) {
        if (o instanceof Double) {
            return ((Double) o).doubleValue() != 0;
        } else if (o instanceof Float) {
            return ((Float) o).floatValue() != 0;
        }
        return false;
    }

    public Boolean doNotThrowAnyException(boolean bo) {
        class ClassWithBooleanField {
            Boolean b;
        }
        ClassWithBooleanField objWithBooleanField = new ClassWithBooleanField();
        return bo ? objWithBooleanField.b : Boolean.TRUE;
    }

    protected boolean aMethodThatReturnsBoolean() {
        return false;
    }

    protected void aMethodThatAcceptsABoolean(boolean b) {
    }
}
