/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.samples_in;

import java.util.Collection;
import java.util.List;

public class SimplifyExpressionSample {

    public void removeUselessNullCheck(String[] args) {
        {
            // Remove redundant null checks
            boolean b1 = args[0] != null && "".equals(args[0]);
            boolean b2 = args[0] != null && args[0] instanceof String;
        }
        {
            // Remove redundant null checks
            boolean b1 = null != args[0] && "".equals(args[0]);
            boolean b2 = null != args[0] && args[0] instanceof String;
        }
        {
            // Remove redundant left / write hand side operands
            boolean b3 = true && args[0] != null;
            boolean b4 = false && args[0] != null;
            boolean b5 = true || args[0] != null;
            boolean b6 = false || args[0] != null;
        }
        {
            // Right-hand-side left unchanged because left-hand-side can have
            // side effects
            boolean b3 = args[0] != null && true;
            boolean b4 = args[0] != null && false;
            boolean b5 = args[0] != null || true;
            boolean b6 = args[0] != null || false;
        }
    }

    public void fixCompareToUsage() {
        boolean b;
        final String s = "";

        // valid, do no change these ones
        b = s.compareTo("") < 0;
        b = s.compareTo("") <= 0;
        b = s.compareTo("") == 0;
        b = s.compareTo("") != 0;
        b = s.compareTo("") >= 0;
        b = s.compareTo("") > 0;
        b = s.compareToIgnoreCase("") == 0;

        // invalid, refactor them
        b = s.compareTo("") == -1;
        b = s.compareTo("") != -1;
        b = s.compareTo("") != 1;
        b = s.compareTo("") == 1;
        b = s.compareToIgnoreCase("") == 1;
    }

    public void borderLineParenthezisedExpressions(Integer i) throws Exception {
        // Do not replace any because they are in a String concatenation
        String s1 = ((Number) i).doubleValue() + "";
        String s2 = (i instanceof Number) + "";
        String s3 = (i + 0) + "";
        String s4 = (i == null ? null : "i")  + "";

        // replace
        boolean b1 = ((Number) i).doubleValue() == 0;
        // replace
        boolean b2 = (i instanceof Number);
        // do not replace
        boolean b3 = (i + 0) == 0;
        // do not replace
        Collection<?> c = null;
        Object obj = ((List<?>) c).get(0);
        // do not replace
        boolean b4 = !(i instanceof Number);
    }

    public boolean removeUselessParentheses() throws Exception {
        boolean b = (true);
        int i;
        Collection<?> col = (null);
        i = (0);
        int[] ar = new int[(i)];
        ar = new int[] { (i) };
        ar[(i)] = (i);
        if ((b)) {
            throw (new Exception());
        }
        do {
        } while ((b));
        while ((b)) {
        }
        for (Object obj : (col)) {
        }
        for (i = 0; (b); i++) {
        }
        synchronized ((col)) {
        }
        switch ((i)) {
        case (0):
        }
        if ((col) instanceof Collection) {
        }
        return ((b));
    }

	public int removeUselessParenthesesInStatements(int i) {
		int j = (i);
		j = (i);
		if ((j == 0)) {
			removeUselessParenthesesInStatements((i));
		}
		do {
			i++;
		} while ((i == 0));
		while ((i == 0)) {
			i++;
		}
		return (i);
	}

    public void removeUselessParenthesesWithAssociativeOperators(boolean b1,
            boolean b2, boolean b3) {
        System.out.println(b1 && (b2 && b3));
        System.out.println(b1 || (b2 || b3));
        int i1 = 0;
        int i2 = 0;
        int i3 = 0;
        System.out.println(i1 * (i2 * i3));
        System.out.println(i1 + (i2 + i3));
        System.out.println(i1 & (i2 & i3));
        System.out.println(i1 | (i2 | i3));
        System.out.println(i1 ^ (i2 ^ i3));
    }

    public void doNotRemoveParenthesesWithNonAssociativeOperators(int i1,
            int i2, int i3) {
        System.out.println(i1 - (i2 - i3));
        System.out.println(i1 / (i2 / i3));
    }

    public void doNotRemoveParenthesesDueToOperatorsPriority(int i1,
            int i2, int i3) {
        System.out.println((i1 + i2) / i3);
    }

    public void removeThisExpression() {
        this.simplifyPrimitiveBooleanExpression(false);
        SimplifyExpressionSample.this.simplifyPrimitiveBooleanExpression(false);
    }

    public class InnerClass {

        public void removeThisExpression() {
            this.simplifyBooleanExpression(false);
            InnerClass.this.simplifyBooleanExpression(false);
            SimplifyExpressionSample.InnerClass.this
                    .simplifyBooleanExpression(false);
        }

        public void doNotRemoveThisExpression() {
            SimplifyExpressionSample.this.simplifyPrimitiveBooleanExpression(false);
        }

        public void simplifyBooleanExpression(boolean b) {
        }
    }

    public void simplifyPrimitiveBooleanExpression(boolean b) {
        if (b == true) {
            int i = 0;
        }
        if (b != false) {
            int i = 0;
        }
        if (b == false) {
            int i = 0;
        }
        if (b != true) {
            int i = 0;
        }
        if (b == Boolean.TRUE) {
            int i = 0;
        }
        if (b != Boolean.FALSE) {
            int i = 0;
        }
        if (b == Boolean.FALSE) {
            int i = 0;
        }
        if (b != Boolean.TRUE) {
            int i = 0;
        }
    }

    public void simplifyBooleanWrapperExpression(Boolean b) {
        if (b == true) {
            int i = 0;
        }
        if (b != false) {
            int i = 0;
        }
        if (b == false) {
            int i = 0;
        }
        if (b != true) {
            int i = 0;
        }
    }

    public void doNotSimplifyBooleanWrapperExpression(Boolean b) {
        if (b == Boolean.TRUE) {
            int i = 0;
        }
        if (b != Boolean.FALSE) {
            int i = 0;
        }
        if (b == Boolean.FALSE) {
            int i = 0;
        }
        if (b != Boolean.TRUE) {
            int i = 0;
        }
    }

}
