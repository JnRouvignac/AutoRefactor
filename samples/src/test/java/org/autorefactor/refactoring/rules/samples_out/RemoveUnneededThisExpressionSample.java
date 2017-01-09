/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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

public class RemoveUnneededThisExpressionSample {

    public void removeThisExpression() {
        // Keep this comment
        testRemoveThisExpression(42);
        testRemoveThisExpression(42);
        hashCode();
    }

    public void removeThisExpressionForAnonymousClass() {
        new Object() {
            public void testRemoveThisExpression(int i) {
                // Keep this comment
                testRemoveThisExpression(i);
            }
        }.testRemoveThisExpression(42);
    }

    public void doNotRemoveThisExpressionForAnonymousClass() {
        new Object() {
            public void testRemoveThisExpression(int i) {
                RemoveUnneededThisExpressionSample.this.testRemoveThisExpression(i);
            }
        }.testRemoveThisExpression(42);
    }

    public void doNotRemoveThisExpressionForAnonymousClassWithPrimitiveOverloadedMethod() {
        new Object() {
            public void doNotRemoveThisExpression(int i) {
                RemoveUnneededThisExpressionSample.this.testRemoveThisExpression(i);
            }

            public void testRemoveThisExpression(long l) {
                System.out.println(l);
            }
        }.doNotRemoveThisExpression(42);
    }

    public void doNotRemoveThisExpressionForAnonymousClassWithObjectOverloadedMethod() {
        new Object() {
            public void doNotRemoveThisExpression(int i) {
                RemoveUnneededThisExpressionSample.this.testRemoveThisExpression(i);
            }

            public void testRemoveThisExpression(Integer i) {
                System.out.println(i);
            }
        }.doNotRemoveThisExpression(42);
    }

    public class InnerClass {

        public void removeThisExpression() {
            // Keep this comment
            testRemoveThisExpression(42);
            testRemoveThisExpression(42);
            testRemoveThisExpression(42);
        }

        public void doNotRemoveThisExpression() {
            RemoveUnneededThisExpressionSample.this.testRemoveThisExpression(42);
        }

        public void testRemoveThisExpression(int i) {
        }
    }

    private void testRemoveThisExpression(int i) {
    }

    public String doNotRemoveThis() {
        return this.<String>aGenericMethod();
    }

    public String removeThisAndUselessTypeParameter() {
        return this.<Class> notGenericMethod();
    }

    private <T> T aGenericMethod() {
        return null;
    }

    private String notGenericMethod() {
        return null;
    }
}
