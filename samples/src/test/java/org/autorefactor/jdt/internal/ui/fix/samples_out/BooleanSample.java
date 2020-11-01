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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.util.List;

public class BooleanSample {
    public boolean booleanPrimitive;
    public Boolean booleanWrapper;

    public void replaceIfByAssignment(boolean isValueValid) {
        boolean varToAssign;
        // Keep this comment
        varToAssign = isValueValid;
    }

    public void replaceIfByOppositeAssignment(boolean isValueValid) {
        boolean varToAssign;
        // Keep this comment
        varToAssign = !isValueValid;
    }

    public boolean returnIfConditionBooleanPrimitive(boolean isValueValid) {
        // Keep this comment
        return isValueValid;
    }

    public boolean returnIfConditionBooleanPrimitive2(boolean isValueValid) {
        // Keep this comment
        return !isValueValid;
    }

    public boolean returnIfConditionWithInfixExpressionBooleanPrimitive(int i) {
        // Keep this comment
        return 0 >= i || i >= 12;
    }

    public boolean returnIfConditionWithInstanceofExpressionBooleanPrimitive(Object o) {
        // Keep this comment
        return !(o instanceof String);
    }

    public boolean returnIfConditionAddCurlyBraces(Object o) {
        // Keep this comment
        return (o instanceof Integer) || !(o instanceof String);
    }

    public boolean returnIfConditionThatRevertsInstanceofExpressionBooleanPrimitive(Object o) {
        // Keep this comment
        return (/* do not lose me */o instanceof String);
    }

    public Boolean returnIfConditionBooleanObject(boolean isValueValid) {
        // Keep this comment
        return Boolean.valueOf(isValueValid);
    }

    public Boolean returnIfConditionBooleanObject2(boolean isValueValid) {
        // Keep this comment
        return Boolean.valueOf(!isValueValid);
    }

    public boolean returnIfConditionMixedBoolean1(boolean isValueValid) {
        // Keep this comment
        return Boolean.valueOf(isValueValid);
    }

    public boolean returnIfConditionMixedBoolean2(boolean isValueValid) {
        // Keep this comment
        return isValueValid;
    }

    public boolean returnIfConditionBooleanPrimitive3(boolean isValueValid) {
        // Keep this comment
        return isValueValid;
    }

    public boolean returnIfConditionBooleanPrimitive4(boolean isValueValid) {
        // Keep this comment
        return !isValueValid;
    }

    public boolean returnIfConditionBooleanObject3(boolean isValueValid) {
        // Keep this comment
        return isValueValid;
    }

    public boolean returnIfConditionBooleanObject4(boolean isValueValid) {
        // Keep this comment
        return !isValueValid;
    }

    public boolean removeUselessTernaryOperatorWithBooleanPrimitive1(boolean isValid) {
        // Keep this comment
        boolean isValueValid = isValid;
        return isValueValid;
    }

    public boolean removeUselessTernaryOperatorWithBooleanPrimitive2(boolean isValid) {
        // Keep this comment
        boolean isValueValid = !isValid;
        return isValueValid;
    }

    public Boolean removeUselessTernaryOperatorWithBooleanObject1(boolean isValid) {
        // Keep this comment
        Boolean isValueValid = Boolean.valueOf(isValid);
        return isValueValid;
    }

    public Boolean removeUselessTernaryOperatorWithBooleanObject2(boolean isValid) {
        // Keep this comment
        Boolean isValueValid = Boolean.valueOf(!isValid);
        return isValueValid;
    }

    public boolean replaceTernaryOperatorByAndOperator(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean isValueValid = bo1 && bo2;
        return isValueValid;
    }

    public boolean replaceTernaryOperatorByAndOperator2(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean isValueValid = !bo1 && bo2;
        return isValueValid;
    }

    public boolean replaceTernaryOperatorByOrOperator(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean isValueValid = bo1 || bo2;
        return isValueValid;
    }

    public boolean replaceTernaryOperatorByOrOperator2(boolean bo1, boolean bo2) {
        // Keep this comment
        boolean isValueValid = !bo1 || bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObjectConstant(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean isValueValid = bo1 && bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObjectConstant2(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean isValueValid = !bo1 && bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObjectConstant(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean isValueValid = bo1 || bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObjectConstant2(
            boolean bo1, boolean bo2) {
        // Keep this comment
        Boolean isValueValid = !bo1 || bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObject(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean isValueValid = bo1 && bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithObject2(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean isValueValid = !bo1 && bo2;
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithObject(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Boolean isValueValid = bo1 || bo2;
        return isValueValid;
    }

    public Object replaceTernaryOperatorByOrOperatorWithObject2(
            Boolean bo1, Boolean bo2) {
        // Keep this comment
        Object isValueValid = !bo1 || bo2;
        return isValueValid;
    }

    public Object doNotReplaceNonBooleanExpression(boolean isValid) {
        Object anything = isValid ? true : "false";
        return anything;
    }

    public void doNotReplacePossibleNullObject(Boolean bo1, Boolean bo2) {
        Boolean isValueValid = bo1 ? bo2 : Boolean.FALSE;
        isValueValid = bo1 ? Boolean.FALSE : bo2;
        isValueValid = bo1 ? Boolean.TRUE : bo2;
        isValueValid = bo1 ? bo2 : Boolean.TRUE;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithExpression(int number1, int number2) {
        // Keep this comment
        Boolean isValueValid = (number1 == 1) && (number2 == 2);
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByAndOperatorWithExpression2(int number1, int number2) {
        // Keep this comment
        Boolean isValueValid = number1 != 1 && (number2 == 2);
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithExpression(int number1, int number2) {
        // Keep this comment
        Boolean isValueValid = (number1 == 1) || (number2 == 2);
        return isValueValid;
    }

    public Boolean replaceTernaryOperatorByOrOperatorWithExpression2(int number1, int number2) {
        // Keep this comment
        Boolean isValueValid = number1 != 1 || (number2 == 2);
        return isValueValid;
    }

    public void doNotRemoveIfInBooleanPrimitiveAssignment1(boolean isValid) {
        boolean isValueValid = true;
        if (isValid) {
            isValueValid = false;
        } else {
            System.out.println();
        }
    }

    public void removeUselessIfInBooleanPrimitiveAssignment1(boolean isValid) {
        // Keep this comment
        boolean isValueValid = !isValid;
    }

    public void removeUselessIfInBooleanPrimitiveAssignment2(boolean isValid) {
        // Keep this comment
        boolean isValueValid = isValid;
    }

    public void removeUselessIfInBooleanObjectAssignment1(boolean isValid) {
        // Keep this comment
        Boolean isValueValid = Boolean.valueOf(!isValid);
    }

    public void removeUselessIfInBooleanObjectAssignment2(boolean isValid) {
        // Keep this comment
        boolean isValueValid = isValid;
    }

    public void removeUselessIfInBooleanPrimitiveAssignment3(boolean isValid,
            boolean isValueValid) {
        // Keep this comment
        isValueValid = !isValid;
    }

    public void removeUselessIfInBooleanPrimitiveAssignment4(boolean isValid,
            boolean isValueValid) {
        // Keep this comment
        isValueValid = isValid;
    }

    public void doNotInlineAlreadyUsedVariable(boolean isValid) {
        boolean isValueValid = false;
        if (isValid || isValueValid) {
            isValueValid = true;
        }
    }

    public void removeUselessIfInBooleanPrimitiveAssignmentSearchFurtherAwayForPreviousSibling(
            boolean isValid, boolean isValueValid) {
        isValueValid = false;
        char c = 'a';
        byte by = 0;
        double d = 0.0;
        if (isValid) {
            isValueValid = true;
        }
    }

    public void removeUselessIfInBooleanObjectAssignment3(boolean isValid, Boolean isValueValid) {
        // Keep this comment
        isValueValid = Boolean.valueOf(!isValid);
    }

    public void removeUselessIfInBooleanObjectAssignment4(boolean isValid, Boolean isValueValid) {
        // Keep this comment
        isValueValid = Boolean.valueOf(isValid);
    }

    public void removeUselessIfInBooleanPrimitiveAssignment5(boolean isValid) {
        // Keep this comment
        this.booleanPrimitive = isValid;
    }

    public void removeUselessIfInBooleanObjectAssignment5(boolean isValid) {
        // Keep this comment
        this.booleanWrapper = Boolean.valueOf(isValid);
    }

    public void removeUselessIfInBooleanPrimitiveAssignment6(boolean isValid) {
        // Keep this comment
        booleanPrimitive = isValid;
    }

    public void removeUselessIfInBooleanObjectAssignment6(boolean isValid) {
        // Keep this comment
        booleanWrapper = Boolean.valueOf(isValid);
    }

    public void removeUselessIfInBooleanObjectAssignment7(boolean isValid) {
        // Keep this comment
        BooleanSample.this.booleanWrapper = Boolean.valueOf(isValid);
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment7(boolean isValid) {
        // Keep this comment
        return isValid && aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment8(boolean isValid) {
        // Keep this comment
        return !isValid || aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment9(boolean isValid) {
        // Keep this comment
        return !isValid && aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfInBooleanPrimitiveAssignment10(boolean isValid) {
        // Keep this comment
        return isValid || aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfWithBooleanObjectParameter1(Boolean isValid) {
        // Keep this comment
        return isValid && aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfWithBooleanObjectParameter2(Boolean isValid) {
        // Keep this comment
        return !isValid || aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfWithBooleanObjectParameter3(Boolean isValid) {
        // Keep this comment
        return !isValid && aMethodThatReturnsBoolean();
    }

    public boolean removeUselessIfWithBooleanObjectParameter4(Boolean isValid) {
        // Keep this comment
        return isValid || aMethodThatReturnsBoolean();
    }

    public void removeUselessTernaryOperatorWithBooleanPrimitive(boolean isValid) {
        aMethodThatAcceptsABoolean(isValid);
        aMethodThatAcceptsABoolean(!isValid);
        aMethodThatAcceptsABoolean(Boolean.valueOf(isValid));
        aMethodThatAcceptsABoolean(Boolean.valueOf(!isValid));
    }

    public void directlyPassBooleanPrimitiveAsParameter(boolean isValid) {
        // Keep this comment
        aMethodThatAcceptsABoolean(isValid);
    }

    public void directlyPassBooleanExpressionAsParameter(int i) {
        // Keep this comment
        aMethodThatAcceptsABoolean(i > 0);
    }

    public void mergeCodeInIfStatement(int i) {
        if (i % 2 == 0) {
            // Keep this comment
            aMethodThatAcceptsABoolean(i > 0);
        }
    }

    public void mergeCodeInElseStatement(int i) {
        // Keep this comment
        if (i % 2 == 0) {
            System.out.println("i is even");
        } else {
            aMethodThatAcceptsABoolean(i > 0);
        }
    }

    public void doNotDuplicateExpression(int i) {
        if (i > 0) {
            aMethodThatAcceptsABoolean(true);
            aMethodThatAcceptsABoolean(true);
        } else {
            aMethodThatAcceptsABoolean(false);
            aMethodThatAcceptsABoolean(false);
        }
    }

    public void directlyPassInvertedBooleanPrimitiveAsParameter(boolean isValid) {
        // Keep this comment
        aMethodThatAcceptsABoolean(!isValid);
    }

    public int directlyPassBooleanAmongOtherCode(boolean isValid) {
        // Keep this comment
        aMethodThatAcceptsABoolean(isValid);
        if (aMethodThatReturnsBoolean()) {
            return 0;
        } else {
            return 10;
        }
    }

    public int directlyPassBooleanAmongOtherBoolean(boolean isValid, int i) {
        // Keep this comment
        aMethodThatAcceptsABoolean(true);
        aMethodThatAcceptsABoolean(isValid);
        aMethodThatAcceptsABoolean(true);
        i++;
        return i;
    }

    public int directlyPassOppositeBoolean(boolean isValid) {
        // Keep this comment
        aMethodThatAcceptsABoolean(isValid);
        aMethodThatAcceptsABoolean(!isValid);
        if (aMethodThatReturnsBoolean()) {
            return 0;
        } else
            return 10;
    }

    public int directlyPassOppositeBoolean() {
        // Keep this comment
        aMethodThatAcceptsABoolean(this.booleanPrimitive);
        aMethodThatAcceptsABoolean(!this.booleanPrimitive);
        if (aMethodThatReturnsBoolean()) {
            return 0;
        } else
            return 10;
    }

    public int refactorWithFollowingCode(boolean isValid) {
        // Keep this comment
        aMethodThatAcceptsABoolean(!isValid);
        return 42;
    }

    public int doNotRefactorWithNameConflict(boolean isValid) {
        if (isValid) {
            boolean isValueValid = true;
            aMethodThatAcceptsABoolean(isValueValid);
        } else {
            boolean isValueValid = false;
            aMethodThatAcceptsABoolean(isValueValid);
        }

        int isValueValid = 42;
        return isValueValid;
    }

    public void doNotMoveActiveExpression(List<Integer> modifiableList) {
        if (modifiableList.add(1)) {
            aMethodThatAcceptsABoolean(modifiableList.contains(1));
            aMethodThatAcceptsABoolean(true);
        } else {
            aMethodThatAcceptsABoolean(modifiableList.contains(1));
            aMethodThatAcceptsABoolean(false);
        }
    }

    public void removeUselessIfInBooleanPrimitiveExpression12(boolean isValid) {
        if (isValid) {
            aMethodThatAcceptsABoolean(true);
        } else {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(isValid || aMethodThatReturnsBoolean());
    }

    public void removeUselessIfInBooleanPrimitiveExpression13(boolean isValid) {
        if (isValid) {
            aMethodThatAcceptsABoolean(false);
        } else {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(!isValid && aMethodThatReturnsBoolean());
    }

    public void removeUselessIfInBooleanPrimitiveExpression14(boolean isValid) {
        if (isValid) {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        } else {
            aMethodThatAcceptsABoolean(true);
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(!isValid || aMethodThatReturnsBoolean());
    }

    public void removeUselessIfInBooleanPrimitiveExpression15(boolean isValid) {
        if (isValid) {
            aMethodThatAcceptsABoolean(aMethodThatReturnsBoolean());
        } else {
            aMethodThatAcceptsABoolean(false);
        }
        // FIXME This should be converted to aMethodThatAcceptsABoolean(isValid && aMethodThatReturnsBoolean());
    }

    public boolean invertConditionalExpression(int i, boolean res1, boolean res2) {
        // Keep this comment
        return i == 0 ? !res1 : !res2;
    }

    public boolean invertAssignment(boolean b1, boolean b2) {
        // Keep this comment
        return !(b1 = b2);
    }

    public boolean invertCast(Object o) {
        // Keep this comment
        return !((Boolean) o);
    }

    public boolean doNotRefactor(Object o) {
        if (o instanceof Double) {
            return ((Double) o).doubleValue() != 0;
        } else if (o instanceof Float) {
            return ((Float) o).floatValue() != 0;
        }
        return false;
    }

    public Boolean doNotThrowAnyException(boolean isValid) {
        class ClassWithBooleanField {
            Boolean isValueValid;
        }
        ClassWithBooleanField objWithBooleanField = new ClassWithBooleanField();
        return isValid ? objWithBooleanField.isValueValid : Boolean.TRUE;
    }

    protected boolean aMethodThatReturnsBoolean() {
        return false;
    }

    protected void aMethodThatAcceptsABoolean(boolean isValueValid) {
    }
}
