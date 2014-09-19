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
package org.autorefactor.samples.test;

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
    private org.autorefactor.samples_in.BooleanSample sampleIn;
    private boolean sampleOutResult;
    private org.autorefactor.samples_out.BooleanSample sampleOut;
    private final String methodName;
    private final Object arg;

    public BooleanSampleTest(String methodName, Object arg) {
        this.methodName = methodName;
        this.arg = arg;
    }

    @Before
    public void setUp() {
        sampleIn = new org.autorefactor.samples_in.BooleanSample() {
            @Override
            protected void aMethodThatAcceptsABoolean(boolean b) {
                sampleInResult = b;
            }
            @Override
            protected boolean aMethodThatReturnsBoolean() {
                return samplesReturn;
            }
        };
        sampleOut = new org.autorefactor.samples_out.BooleanSample() {
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

    @Parameters(name = "{0}Refactoring")
    public static Collection<Object[]> data() {
        return Arrays.asList(
                new Object[] { "returnIfConditionBooleanPrimitive", true },
                new Object[] { "returnIfConditionBooleanPrimitive", false },
                new Object[] { "returnIfConditionBooleanPrimitive2", true },
                new Object[] { "returnIfConditionBooleanPrimitive2", false },
                new Object[] { "returnIfConditionWithInfixExpressionBooleanPrimitive", 0 },
                new Object[] { "returnIfConditionWithInfixExpressionBooleanPrimitive", 1 },
                new Object[] { "returnIfConditionWithInstanceofExpressionBooleanPrimitive", "" },
                new Object[] { "returnIfConditionWithInstanceofExpressionBooleanPrimitive", new Object() },
                new Object[] { "returnIfConditionThatRevertsInstanceofExpressionBooleanPrimitive", "" },
                new Object[] { "returnIfConditionThatRevertsInstanceofExpressionBooleanPrimitive", new Object() },
                new Object[] { "returnIfConditionBooleanObject", true },
                new Object[] { "returnIfConditionBooleanObject", false },
                new Object[] { "returnIfConditionBooleanObject2", true },
                new Object[] { "returnIfConditionBooleanObject2", false },
                new Object[] { "returnIfConditionMixedBoolean1", true },
                new Object[] { "returnIfConditionMixedBoolean1", false },
                new Object[] { "returnIfConditionMixedBoolean2", true },
                new Object[] { "returnIfConditionMixedBoolean2", false },
                new Object[] { "returnIfConditionBooleanPrimitive3", true },
                new Object[] { "returnIfConditionBooleanPrimitive3", false },
                new Object[] { "returnIfConditionBooleanPrimitive4", true },
                new Object[] { "returnIfConditionBooleanPrimitive4", false },
                new Object[] { "returnIfConditionBooleanObject3", true },
                new Object[] { "returnIfConditionBooleanObject3", false },
                new Object[] { "returnIfConditionBooleanObject4", true },
                new Object[] { "returnIfConditionBooleanObject4", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment5", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment5", false },
                new Object[] { "removeUselessIfInBooleanObjectAssignment5", true },
                new Object[] { "removeUselessIfInBooleanObjectAssignment5", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment6", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment6", false },
                new Object[] { "removeUselessIfInBooleanObjectAssignment6", true },
                new Object[] { "removeUselessIfInBooleanObjectAssignment6", false },
                new Object[] { "removeUselessIfInBooleanObjectAssignment7", true },
                new Object[] { "removeUselessIfInBooleanObjectAssignment7", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment7", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment7", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment8", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment8", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment9", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment9", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment10", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveAssignment10", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveExpression10", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveExpression10", false },
                new Object[] { "removeUselessIfInBooleanPrimitiveExpression11", true },
                new Object[] { "removeUselessIfInBooleanPrimitiveExpression11", false }
        );
    }

    @Test
    public void assertInvokeEquals() throws Exception {
        assertInvokeEquals(true);
        assertInvokeEquals(false);
    }

    private void assertInvokeEquals(boolean sampleReturn) throws Exception {
        samplesReturn = sampleReturn;
        final Boolean resIn = invoke(sampleIn, methodName, arg);
        final Boolean resOut = invoke(sampleOut, methodName, arg);
        assertEquals(resIn, resOut);
        assertEquals(sampleIn.f, sampleOut.f);
        assertEquals(sampleIn.g, sampleOut.g);
        assertEquals(sampleInResult, sampleOutResult);
    }

    private Boolean invoke(Object obj, String methodName, Object arg) throws Exception {
        try {
            return invoke(obj, methodName, arg, arg.getClass());
        } catch (NoSuchMethodException e) {}
        final Class<?> type = getPrimitiveType(arg.getClass());
        if (type != null) {
            try {
                return invoke(obj, methodName, arg, type);
            } catch (NoSuchMethodException e) {}
        }
        return invoke(obj, methodName, arg, Object.class);
    }

    private Boolean invoke(Object obj, String methodName, Object arg,
            Class<?> argType) throws Exception {
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
