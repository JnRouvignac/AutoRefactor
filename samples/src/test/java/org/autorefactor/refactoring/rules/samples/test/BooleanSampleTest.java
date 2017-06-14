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
package org.autorefactor.refactoring.rules.samples.test;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import static org.junit.Assert.*;

@SuppressWarnings("javadoc")
@RunWith(value = Parameterized.class)
public class BooleanSampleTest {

    private boolean samplesReturn;
    private boolean sampleInResult;
    private org.autorefactor.refactoring.rules.samples_in.BooleanSample sampleIn;
    private boolean sampleOutResult;
    private org.autorefactor.refactoring.rules.samples_out.BooleanSample sampleOut;
    private final String methodName;
    private final Object[] args;

    public BooleanSampleTest(String methodName, Object... args) {
        this.methodName = methodName;
        this.args = args;
    }

    @Before
    public void setUp() {
        sampleIn = new org.autorefactor.refactoring.rules.samples_in.BooleanSample() {
            @Override
            protected void aMethodThatAcceptsABoolean(boolean b) {
                sampleInResult = b;
            }
            @Override
            protected boolean aMethodThatReturnsBoolean() {
                return samplesReturn;
            }
        };
        sampleOut = new org.autorefactor.refactoring.rules.samples_out.BooleanSample() {
            @Override
            protected void aMethodThatAcceptsABoolean(boolean b) {
                sampleOutResult = b;
            }
            @Override
            protected boolean aMethodThatReturnsBoolean() {
                return samplesReturn;
            }
        };
    }

    @Parameters(name = "{0}({1})")
    public static Collection<Object[]> data() {
        return Arrays.asList(new Object[][] {
                { "returnIfConditionBooleanPrimitive", new Object[] { true } },
                { "returnIfConditionBooleanPrimitive", new Object[] { false } },
                { "returnIfConditionBooleanPrimitive2", new Object[] { true } },
                { "returnIfConditionBooleanPrimitive2", new Object[] { false } },
                { "returnIfConditionWithInfixExpressionBooleanPrimitive", new Object[] { 0 } },
                { "returnIfConditionWithInfixExpressionBooleanPrimitive", new Object[] { 1 } },
                { "returnIfConditionWithInstanceofExpressionBooleanPrimitive", new Object[] { "" } },
                { "returnIfConditionWithInstanceofExpressionBooleanPrimitive", new Object[] { new Object() } },
                { "returnIfConditionAddCurlyBraces", new Object[] { 0 } },
                { "returnIfConditionAddCurlyBraces", new Object[] { "" } },
                { "returnIfConditionAddCurlyBraces", new Object[] { new Object() } },
                { "returnIfConditionThatRevertsInstanceofExpressionBooleanPrimitive", new Object[] { "" } },
                { "returnIfConditionThatRevertsInstanceofExpressionBooleanPrimitive", new Object[] { new Object() } },
                { "returnIfConditionBooleanObject", new Object[] { true } },
                { "returnIfConditionBooleanObject", new Object[] { false } },
                { "returnIfConditionBooleanObject2", new Object[] { true } },
                { "returnIfConditionBooleanObject2", new Object[] { false } },
                { "returnIfConditionMixedBoolean1", new Object[] { true } },
                { "returnIfConditionMixedBoolean1", new Object[] { false } },
                { "returnIfConditionMixedBoolean2", new Object[] { true } },
                { "returnIfConditionMixedBoolean2", new Object[] { false } },
                { "returnIfConditionBooleanPrimitive3", new Object[] { true } },
                { "returnIfConditionBooleanPrimitive3", new Object[] { false } },
                { "returnIfConditionBooleanPrimitive4", new Object[] { true } },
                { "returnIfConditionBooleanPrimitive4", new Object[] { false } },
                { "returnIfConditionBooleanObject3", new Object[] { true } },
                { "returnIfConditionBooleanObject3", new Object[] { false } },
                { "returnIfConditionBooleanObject4", new Object[] { true } },
                { "returnIfConditionBooleanObject4", new Object[] { false } },
                { "removeUselessTernaryOperatorWithBooleanPrimitive1", new Object[] { true } },
                { "removeUselessTernaryOperatorWithBooleanPrimitive1", new Object[] { false } },
                { "removeUselessTernaryOperatorWithBooleanPrimitive2", new Object[] { true } },
                { "removeUselessTernaryOperatorWithBooleanPrimitive2", new Object[] { false } },
                { "removeUselessTernaryOperatorWithBooleanObject1", new Object[] { true } },
                { "removeUselessTernaryOperatorWithBooleanObject1", new Object[] { false } },
                { "removeUselessTernaryOperatorWithBooleanObject2", new Object[] { true } },
                { "removeUselessTernaryOperatorWithBooleanObject2", new Object[] { false } },

                { "replaceTernaryOperatorByAndOperator", new Object[] { true, true } },
                { "replaceTernaryOperatorByAndOperator", new Object[] { true, false } },
                { "replaceTernaryOperatorByAndOperator", new Object[] { false, true } },
                { "replaceTernaryOperatorByAndOperator", new Object[] { false, false } },
                { "replaceTernaryOperatorByAndOperator2", new Object[] { true, true } },
                { "replaceTernaryOperatorByAndOperator2", new Object[] { true, false } },
                { "replaceTernaryOperatorByAndOperator2", new Object[] { false, true } },
                { "replaceTernaryOperatorByAndOperator2", new Object[] { false, false } },
                { "replaceTernaryOperatorByOrOperator", new Object[] { true, true } },
                { "replaceTernaryOperatorByOrOperator", new Object[] { true, false } },
                { "replaceTernaryOperatorByOrOperator", new Object[] { false, true } },
                { "replaceTernaryOperatorByOrOperator", new Object[] { false, false } },
                { "replaceTernaryOperatorByOrOperator2", new Object[] { true, true } },
                { "replaceTernaryOperatorByOrOperator2", new Object[] { true, false } },
                { "replaceTernaryOperatorByOrOperator2", new Object[] { false, true } },
                { "replaceTernaryOperatorByOrOperator2", new Object[] { false, false } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant", new Object[] { true, true } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant", new Object[] { true, false } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant", new Object[] { false, true } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant", new Object[] { false, false } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant2", new Object[] { true, true } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant2", new Object[] { true, false } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant2", new Object[] { false, true } },
                { "replaceTernaryOperatorByAndOperatorWithObjectConstant2", new Object[] { false, false } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant", new Object[] { true, true } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant", new Object[] { true, false } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant", new Object[] { false, true } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant", new Object[] { false, false } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant2", new Object[] { true, true } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant2", new Object[] { true, false } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant2", new Object[] { false, true } },
                { "replaceTernaryOperatorByOrOperatorWithObjectConstant2", new Object[] { false, false } },
                // FIXME JNR with null?
                { "replaceTernaryOperatorByAndOperatorWithObject", new Object[] { true, true } },
                { "replaceTernaryOperatorByAndOperatorWithObject", new Object[] { true, false } },
                { "replaceTernaryOperatorByAndOperatorWithObject", new Object[] { false, true } },
                { "replaceTernaryOperatorByAndOperatorWithObject", new Object[] { false, false } },
                { "replaceTernaryOperatorByAndOperatorWithObject2", new Object[] { true, true } },
                { "replaceTernaryOperatorByAndOperatorWithObject2", new Object[] { true, false } },
                { "replaceTernaryOperatorByAndOperatorWithObject2", new Object[] { false, true } },
                { "replaceTernaryOperatorByAndOperatorWithObject2", new Object[] { false, false } },
                { "replaceTernaryOperatorByOrOperatorWithObject", new Object[] { true, true } },
                { "replaceTernaryOperatorByOrOperatorWithObject", new Object[] { true, false } },
                { "replaceTernaryOperatorByOrOperatorWithObject", new Object[] { false, true } },
                { "replaceTernaryOperatorByOrOperatorWithObject", new Object[] { false, false } },
                { "replaceTernaryOperatorByOrOperatorWithObject2", new Object[] { true, true } },
                { "replaceTernaryOperatorByOrOperatorWithObject2", new Object[] { true, false } },
                { "replaceTernaryOperatorByOrOperatorWithObject2", new Object[] { false, true } },
                { "replaceTernaryOperatorByOrOperatorWithObject2", new Object[] { false, false } },
                { "doNotReplacePossibleNullObject", new Object[] { true, true } },
                { "doNotReplacePossibleNullObject", new Object[] { true, false } },
                { "doNotReplacePossibleNullObject", new Object[] { false, true } },
                { "doNotReplacePossibleNullObject", new Object[] { false, false } },
                { "replaceTernaryOperatorByAndOperatorWithExpression", new Object[] { 1, 1 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression", new Object[] { 1, 2 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression", new Object[] { 2, 1 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression", new Object[] { 2, 2 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression2", new Object[] { 1, 1 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression2", new Object[] { 1, 2 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression2", new Object[] { 2, 1 } },
                { "replaceTernaryOperatorByAndOperatorWithExpression2", new Object[] { 2, 2 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression", new Object[] { 1, 1 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression", new Object[] { 1, 2 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression", new Object[] { 2, 1 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression", new Object[] { 2, 2 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression2", new Object[] { 1, 1 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression2", new Object[] { 1, 2 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression2", new Object[] { 2, 1 } },
                { "replaceTernaryOperatorByOrOperatorWithExpression2", new Object[] { 2, 2 } },

                { "doNotRemoveIfInBooleanPrimitiveAssignment1", new Object[] { true } },
                { "doNotRemoveIfInBooleanPrimitiveAssignment1", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment1", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment1", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment2", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment2", new Object[] { false } },
                { "removeUselessIfInBooleanObjectAssignment1", new Object[] { true } },
                { "removeUselessIfInBooleanObjectAssignment1", new Object[] { false } },
                { "removeUselessIfInBooleanObjectAssignment2", new Object[] { true } },
                { "removeUselessIfInBooleanObjectAssignment2", new Object[] { false } },
//                { "removeUselessIfInBooleanPrimitiveAssignment3", new Object[] { true } },
//                { "removeUselessIfInBooleanPrimitiveAssignment3", new Object[] { false } },
//                { "removeUselessIfInBooleanPrimitiveAssignment4", new Object[] { true } },
//                { "removeUselessIfInBooleanPrimitiveAssignment4", new Object[] { false } },
//                { "removeUselessIfInBooleanPrimitiveAssignmentSearchFurtherAwayForPreviousSibling", new Object[] { false } },
//                { "removeUselessIfInBooleanPrimitiveAssignmentSearchFurtherAwayForPreviousSibling", new Object[] { false } },
//                { "removeUselessIfInBooleanObjectAssignment3", new Object[] { true } },
//                { "removeUselessIfInBooleanObjectAssignment3", new Object[] { false } },
//                { "removeUselessIfInBooleanObjectAssignment4", new Object[] { true } },
//                { "removeUselessIfInBooleanObjectAssignment4", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment5", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment5", new Object[] { false } },
                { "removeUselessIfInBooleanObjectAssignment5", new Object[] { true } },
                { "removeUselessIfInBooleanObjectAssignment5", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment6", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment6", new Object[] { false } },
                { "removeUselessIfInBooleanObjectAssignment6", new Object[] { true } },
                { "removeUselessIfInBooleanObjectAssignment6", new Object[] { false } },
                { "removeUselessIfInBooleanObjectAssignment7", new Object[] { true } },
                { "removeUselessIfInBooleanObjectAssignment7", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment7", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment7", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment8", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment8", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment9", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment9", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveAssignment10", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveAssignment10", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveExpression10", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveExpression10", new Object[] { false } },
                { "removeUselessIfInBooleanPrimitiveExpression11", new Object[] { true } },
                { "removeUselessIfInBooleanPrimitiveExpression11", new Object[] { false } }
        });
    }

    @Test
    public void assertInvokeEquals() throws Exception {
        assertInvokeEquals(true);
        assertInvokeEquals(false);
    }

    private void assertInvokeEquals(boolean sampleReturn) throws Exception {
        samplesReturn = sampleReturn;
        final Boolean resIn = invoke(sampleIn, methodName, args);
        final Boolean resOut = invoke(sampleOut, methodName, args);
        assertEquals(resIn, resOut);
        assertEquals(sampleIn.booleanPrimitive, sampleOut.booleanPrimitive);
        assertEquals(sampleIn.booleanWrapper, sampleOut.booleanWrapper);
        assertEquals(sampleInResult, sampleOutResult);
    }

    private Boolean invoke(Object obj, String methodName, Object... args) throws Exception {
        try {
            return invoke(obj, methodName, args, getClasses(args));
        } catch (NoSuchMethodException e) {
            // try with primitive types
        }

        try {
            final Class<?>[] type = getPrimitiveTypes(args);
            return invoke(obj, methodName, args, type);
        } catch (NoSuchMethodException e) {
            // try with java.lang.Object
        }
        return invoke(obj, methodName, args, objectsArray(args));
    }

    private Class<?>[] getClasses(Object... args) {
        final Class<?>[] results = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++) {
            results[i] = args[i].getClass();
        }
        return results;
    }

    private Class<?>[] getPrimitiveTypes(Object[] args) {
        final Class<?>[] results = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++) {
            results[i] = getPrimitiveType(args[i].getClass());
        }
        return results;
    }

    private Class<?>[] objectsArray(Object[] args) {
        final Class<?>[] results = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++) {
            results[i] = Object.class;
        }
        return results;
    }

    private Boolean invoke(Object obj, String methodName, Object[] arg, Class<?>[] argType) throws Exception {
        final Method m = obj.getClass().getMethod(methodName, argType);
        return (Boolean) m.invoke(obj, arg);
    }

    private Class<?> getPrimitiveType(Class<?> clazz) {
        if (Boolean.class.isAssignableFrom(clazz)) {
            return Boolean.TYPE;
        } else if (Byte.class.isAssignableFrom(clazz)) {
            return Byte.TYPE;
        } else if (Character.class.isAssignableFrom(clazz)) {
            return Character.TYPE;
        } else if (Short.class.isAssignableFrom(clazz)) {
            return Short.TYPE;
        } else if (Integer.class.isAssignableFrom(clazz)) {
            return Integer.TYPE;
        } else if (Long.class.isAssignableFrom(clazz)) {
            return Long.TYPE;
        } else if (Float.class.isAssignableFrom(clazz)) {
            return Float.TYPE;
        } else if (Double.class.isAssignableFrom(clazz)) {
            return Double.TYPE;
        }
        return null;
    }

}
