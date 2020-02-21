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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class IncrementStatementRatherThanIncrementExpressionSample extends ArrayList<String> {
	private static final long serialVersionUID = -5909621993540999616L;

	private int field= 0;

    public IncrementStatementRatherThanIncrementExpressionSample(int i) {
		super(i++);
	}

    public IncrementStatementRatherThanIncrementExpressionSample(int doNotRefactor, boolean isEnabled) {
		super(++doNotRefactor);
	}

    public IncrementStatementRatherThanIncrementExpressionSample(int i, int j) {
		this(i++);
	}

	public String moveIncrementBeforeIf(int i) {
        // Keep this comment
        if (++i > 0) {
            return "Positive";
        } else {
        	return "Negative";
        }
    }

    public String moveDecrementBeforeIf(int i) {
        // Keep this comment
        if (--i > 0) {
            return "Positive";
        } else {
        	return "Negative";
        }
    }

    public String doNotMoveIncrementAfterIf(int i) {
    	String result= null;

        if (i++ > 0) {
        	result= "Positive";
        } else {
        	result= "Negative";
        }

    	return result;
    }

    public int doNotMoveDecrementAfterReturn(int i) {
        return i--;
    }

    public int moveDecrementBeforeThrow(int i) {
        // Keep this comment
    	throw new NullPointerException("++i " + ++i);
    }

    public int doNotMoveDecrementAfterThrow(int i) {
    	throw new NullPointerException("i++ " + i++);
    }

    public int doNotMoveIncrementAfterFallThrough(boolean isEnabled, int i) {
        if (i-- > 0) {
            return i++;
        } else {
        	throw new NullPointerException("i++ " + i++);
        }
    }

    public int moveIncrementOutsideStatement(int i, int z, Object[] obj, IncrementStatementRatherThanIncrementExpressionSample[] theClass) throws InterruptedException {
        // Keep this comment
        String[] texts= new String[++i];
        texts.wait(++i);
        int j= i++, k= ++z;
        j= i-- + 123;
        boolean isString= obj[++i] instanceof String;
        List<Date> dates= new ArrayList<>(--i);
        long l= (long)i++;
        int m= (i++);
        boolean isEqual= !(i++ == 10);
        theClass[i++].field--;
        int[] integers= {i++, 1, 2, 3};
        return ++i;
    }

    public boolean moveIncrementOutsideInfix(int i, boolean isEnabled) {
        // Keep this comment
    	boolean isEqual= (i++ == 10) && isEnabled;
        return isEqual;
    }

    public boolean doNotMoveIncrementOutsideConditionalInfix(int i, boolean isEnabled) {
    	boolean isEqual= isEnabled && (i++ == 10);
        return isEqual;
    }

    public boolean moveIncrementOutsideEagerInfix(int i, boolean isEnabled) {
        // Keep this comment
    	boolean isEqual= isEnabled & (i++ == 10);
        return isEqual;
    }

    public int moveIncrementOutsideTernaryExpression(int i) {
        // Keep this comment
        int j= (i++ == 10) ? 10 : 20;
        return j * 2;
    }

    public int doNotMoveIncrementOutsideTernaryExpression(int i) {
        int j= (i == 10) ? i++ : 20;
        return j * 2;
    }

    public int doNotMoveIncrementOnReadVariable(int i) {
        int j= i++ + i++;
        int k= i++ + i;
        int l= i + i++;
        int m= (i = 0) + i++;
        return j + k + l + m;
    }

    public void doNotRefactorIncrementStatement(int i) {
        i++;
    }

    public void doNotMoveIncrementOutsideWhile(int i) {
        while (i-- > 0) {
            System.out.println("Must decrement on each loop");
        }
    }

    public void doNotMoveIncrementOutsideDoWhile(int i) {
        do {
            System.out.println("Must decrement on each loop");
        } while (i-- > 0);
    }

    public void doNotMoveIncrementOutsideFor() {
        for (int i = 0; i < 10; i++) {
            System.out.println("Must increment on each loop");
		}
    }

    public void doNotMoveIncrementOutsideElseIf(int i) {
    	if (i == 0) {
    		System.out.println("I equals zero");
    	} else if (i++ == 10) {
    		System.out.println("I has equaled ten");
    	}
    }

    public int moveIncrementInIf(int i, boolean isEnabled) {
    	if (isEnabled)
    		return ++i;

    	return 0;
    }

    public int moveIncrementInSwitch(int i, int discriminant) {
    	switch (discriminant) {
    	case 0:
    		return ++i;
    	case 1:
    		return --i;
    	}

    	return 0;
    }
}
