/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.List;

public class NoLoopIterationRatherThanEmptyCheckSample {
    public int removeCondition(int[] integerArray) {
        int sum = 0;

        // Keep this comment
        // Keep this comment too
        for (int i : integerArray) {
            sum += i;
        }

        return sum;
    }

    public int removeConditionOnExpression(int[] integerArray) {
        int sum = 0;

        // Keep this comment
        // Keep this comment too
        for (int i : integerArray) {
            sum += i;
        }

        return sum;
    }

    public int removeReversedCondition(int[] integerArray) {
        int sum = 0;

        // Keep this comment
        // Keep this comment too
        for (int i : integerArray) {
            sum += i;
        }

        return sum;
    }

    public int removeInequality(int[] integerArray) {
        int sum = 0;

        // Keep this comment
        // Keep this comment too
        for (int i : integerArray) {
            sum += i;
        }

        return sum;
    }

    public int removeLessEquals(int[] integerArray) {
        int sum = 0;

        // Keep this comment
        // Keep this comment too
        for (int i : integerArray) {
            sum += i;
        }

        return sum;
    }

    public int removeGreaterEquals(int[] integerArray) {
        int sum = 0;

        // Keep this comment
        // Keep this comment too
        for (int i : integerArray) {
            sum += i;
        }

        return sum;
    }

    public int removeSecondCondition(int[] integerArray, boolean isEnabled) {
        int sum = 0;

        // Keep this comment
        if (isEnabled) {
            // Keep this comment too
            for (int i : integerArray) {
                sum += i;
            }
        }

        return sum;
    }

    public int removeLastCondition(int[] integerArray, boolean isEnabled) {
        int sum = 0;

        // Keep this comment
        if (isEnabled && integerArray != null) {
            // Keep this comment too
            for (int i : integerArray) {
                sum += i;
            }
        }

        return sum;
    }

    public int removeMandatoryCondition(int[] integerArray, boolean isEnabled) {
        int sum = 0;

        // Keep this comment
        if (isEnabled) {
            // Keep this comment too
            for (int i : integerArray) {
                sum += i;
            }
        }

        return sum;
    }

    public String removeConditionForObjectArray(String[] texts) {
        StringBuilder builder = new StringBuilder();

        // Keep this comment
        // Keep this comment too
        for (String text : texts) {
            builder.append(text);
        }

        return builder.toString();
    }

    public String removeConditionWithClassicForLoop(String[] texts) {
        StringBuilder builder = new StringBuilder();

        // Keep this comment
        // Keep this comment too
        for (int i = 0; i < texts.length; i++) {
            String text = texts[i];
            builder.append(text);
        }

        return builder.toString();
    }

    public String doNotRefactorNonStandardLoop(String[] texts) {
        StringBuilder builder = new StringBuilder();

        if (texts.length > 0) {
            for (int i = 1; i < texts.length; i++) {
                String text = texts[i];
                builder.append(text);
            }
        }

        return builder.toString();
    }

    public String doNotRemoveElse(String[] texts) {
        StringBuilder builder = new StringBuilder();

        if (texts.length > 0) {
            for (int i = 0; i < texts.length; i++) {
                String text = texts[i];
                builder.append(text);
            }
        } else {
            System.out.println("Do not lose me!");
        }

        return builder.toString();
    }

    public String doNotRefactorElse(String[] texts, boolean isEnabled) {
        StringBuilder builder = new StringBuilder();

        if (isEnabled && texts.length > 0) {
            for (int i = 0; i < texts.length; i++) {
                String text = texts[i];
                builder.append(text);
            }
        } else {
            System.out.println("I want empty arrays!");
        }

        return builder.toString();
    }

    public String doNotRefactorWithRemainingStatement(String[] texts) {
        StringBuilder builder = new StringBuilder();

        if (texts.length > 0) {
            for (String text : texts) {
                builder.append(text);
            }
            System.out.println("I don't want empty arrays!");
        }

        return builder.toString();
    }

    public String doNotRefactorDynamicArray(List<String> texts) {
        StringBuilder builder = new StringBuilder();

        if (texts.toArray(new String[0]).length > 0) {
            for (String text : texts.toArray(new String[0])) {
                builder.append(text);
            }
        }

        return builder.toString();
    }

    public String doNotRefactorDifferentArray(String[] texts, String[] anotherTexts) {
        StringBuilder builder = new StringBuilder();

        if (texts.length > 0) {
            for (String text : anotherTexts) {
                builder.append(text);
            }
        }

        return builder.toString();
    }

    public String doNotRefactorFirstCondition(String[] texts) {
        StringBuilder builder = new StringBuilder();

        if (texts.length > 0 && (texts = new String[0]) != null) {
            for (String text : texts) {
                builder.append(text);
            }
        }

        return builder.toString();
    }

    public String doNotRefactorOtherCondition(String[] texts) {
        StringBuilder builder = new StringBuilder();

        if (texts.length > 1) {
            for (String text : texts) {
                builder.append(text);
            }
        }

        return builder.toString();
    }

    public int doNotRemoveORCondition(int[] integerArray, boolean isEnabled) {
        int sum = 0;

        if (isEnabled || integerArray.length > 0) {
            for (int i : integerArray) {
                sum += i;
            }
        }

        return sum;
    }
}
