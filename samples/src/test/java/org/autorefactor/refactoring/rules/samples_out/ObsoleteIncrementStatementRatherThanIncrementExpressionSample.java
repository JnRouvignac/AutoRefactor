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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ObsoleteIncrementStatementRatherThanIncrementExpressionSample extends ArrayList<String> {
    private static final long serialVersionUID = -5909621993540999616L;

    private int field= 0;

    public ObsoleteIncrementStatementRatherThanIncrementExpressionSample(int i) {
        super(i);
        i++;
    }

    public ObsoleteIncrementStatementRatherThanIncrementExpressionSample(int doNotRefactor, boolean isEnabled) {
        super(++doNotRefactor);
    }

    public ObsoleteIncrementStatementRatherThanIncrementExpressionSample(int i, int j) {
        this(i);
        i++;
    }

    public String moveIncrementBeforeIf(int i) {
        i++;
        // Keep this comment
        if (i > 0) {
            return "Positive";
        } else {
            return "Negative";
        }
    }

    public String moveDecrementBeforeIf(int i) {
        i--;
        // Keep this comment
        if (i > 0) {
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
        i++;
        // Keep this comment
        throw new NullPointerException("++i " + i);
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

    public int moveIncrementOutsideStatement(int i, int z, Object[] obj, ObsoleteIncrementStatementRatherThanIncrementExpressionSample[] theClass) throws InterruptedException {
        i++;
        // Keep this comment
        String[] texts= new String[i];
        i++;
        texts.wait(i);
        z++;
        int j= i, k= z;
        i++;
        j= i + 123;
        i--;
        i++;
        boolean isString= obj[i] instanceof String;
        i--;
        List<Date> dates= new ArrayList<>(i);
        long l= (long)i;
        i++;
        int m= i;
        i++;
        boolean isEqual= !(i == 10);
        i++;
        theClass[i].field--;
        i++;
        int[] integers= {i, 1, 2, 3};
        i++;
        i++;
        return i;
    }

    public boolean moveIncrementOutsideInfix(int i, boolean isEnabled) {
        // Keep this comment
        boolean isEqual= (i == 10) && isEnabled;
        i++;
        return isEqual;
    }

    public String moveIncrementOutsideSuperMethod(int i) {
        i++;
        // Keep this comment
        return super.remove(i);
    }

    public boolean doNotMoveIncrementOutsideConditionalInfix(int i, boolean isEnabled) {
        boolean isEqual= isEnabled && (i++ == 10);
        return isEqual;
    }

    public boolean moveIncrementOutsideEagerInfix(int i, boolean isEnabled) {
        // Keep this comment
        boolean isEqual= isEnabled & (i == 10);
        i++;
        return isEqual;
    }

    public int moveIncrementOutsideTernaryExpression(int i) {
        // Keep this comment
        int j= (i == 10) ? 10 : 20;
        i++;
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
        if (isEnabled) {
            i++;
            return i;
        }

        return 0;
    }

    public int moveIncrementInSwitch(int i, int discriminant) {
        switch (discriminant) {
        case 0:
            i++;
            return i;
        case 1:
            i--;
            return i;
        }

        return 0;
    }
}
