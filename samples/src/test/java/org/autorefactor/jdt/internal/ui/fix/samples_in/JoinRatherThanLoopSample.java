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

public class JoinRatherThanLoopSample {
    public String refactorConcatenation(String[] texts) {
        // Keep this comment
        boolean isFirst = true;
        // Keep this comment too
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment also
        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorReassignment(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (String text : texts) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation = concatenation.append(", ");
            }
            concatenation = concatenation.append(text);
        }

        return concatenation.toString();
    }

    public Runnable refactorFinalConcatenation(String[] names) {
        boolean isFirst = true;
        final StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < names.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(names[i]);
        }

        Runnable supplier= new Runnable() {
            @Override
            public void run() {
                System.out.println(concatenation.toString());
            }
        };
        return supplier;
    }

    public String refactorConcatenationWithChar(String[] titles) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (String title : titles) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(',');
            }
            concatenation.append(title);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithHardCodedDelimiter(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation = concatenation.append(" " + 1);
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithBuilderFirst(String[] texts) {
        StringBuilder concatenation = new StringBuilder();
        boolean isFirst = true;

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithStringBuffer(String[] texts) {
        boolean isFirst = true;
        StringBuffer concatenation = new StringBuffer();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithBooleanObject(String[] texts) {
        Boolean isFirst = Boolean.TRUE;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = Boolean.FALSE;
            } else {
                concatenation.append(", ");
            }
            concatenation = concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithNegatedBoolean(String[] texts) {
        Boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (!isFirst) {
                concatenation.append(", ");
            } else {
                isFirst = false;
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithReversedBoolean(String[] texts) {
        boolean isVisited = Boolean.FALSE;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (isVisited) {
                concatenation.append(", ");
            } else {
                isVisited = Boolean.TRUE;
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public boolean doNotRefactorUsedBoolean(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        System.out.println(concatenation.toString());
        return isFirst;
    }

    public String refactorConcatenationWithLotsOfMethods(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        System.out.println(concatenation.charAt(0));
        System.out.println(concatenation.chars());
        System.out.println(concatenation.codePoints());
        System.out.println(concatenation.indexOf("foo", 0));
        System.out.println(concatenation.lastIndexOf("foo"));
        System.out.println(concatenation.lastIndexOf("foo", 0));
        System.out.println(concatenation.length());
        System.out.println(concatenation.subSequence(0, 0));
        return concatenation.toString();
    }

    public String doNotRefactorUnhandledMethod(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        System.out.println(concatenation.codePointAt(0));
        System.out.println(concatenation.codePointBefore(0));
        System.out.println(concatenation.codePointCount(0, 0));
        concatenation.getChars(0, 0, new char[0], 0);
        System.out.println(concatenation.indexOf("foo"));
        System.out.println(concatenation.offsetByCodePoints(0, 0));
        System.out.println(concatenation.substring(0));
        System.out.println(concatenation.substring(0, 0));
        System.out.println(concatenation.capacity());
        return concatenation.toString();
    }

    public String doNotRefactorPartialConcatenation(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 1; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorUnfinishedConcatenation(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length - 1; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorReversedConcatenation(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = texts.length - 1; i >= 0; i--) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithOppositeBoolean(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 1; i < texts.length; i++) {
            if (isFirst) {
                concatenation.append(", ");
            } else {
                isFirst = false;
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorOnObjects(Object[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithOtherAppending(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        concatenation.append("foo");

        return concatenation.toString();
    }

    public String doNotRefactorWithInitialization(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder("foo");

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithWrongIndex(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[0]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithWrongBoolean(String[] texts, boolean isSecond) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isSecond) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithWrongBoolean(String[] texts) {
        boolean isSecond = false;
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isSecond = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithWrongArray(String[] texts, String[] names) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(names[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithWrongBuilder(String[] texts, StringBuilder otherBuilder) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            otherBuilder.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithAnotherBuilder(String[] texts, StringBuilder otherBuilder) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                otherBuilder.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithAdditionalStatement(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
            System.out.println("Hi!");
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithWrongMethod(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (int i = 0; i < texts.length; i++) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(texts[i], 0, 2);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationOnForeach(String[] texts) {
        StringBuilder concatenation = new StringBuilder();
        boolean isFirst = true;

        // Keep this comment
        for (String text : texts) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(text);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWrongVariable(String[] texts, String test) {
        StringBuilder concatenation = new StringBuilder();
        boolean isFirst = true;

        for (String text : texts) {
            if (isFirst) {
                isFirst = false;
            } else {
                concatenation.append(", ");
            }
            concatenation.append(test);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithConditionOnIndex(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (i > 0) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithInequalityOnIndex(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (i != 0) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithReversedConditionOnIndex(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (0 < i) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithGreaterOrEqualsOnIndex(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (i >= 1) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithDelimiterAtTheEnd(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            concatenation.append(texts[i]);
            if (i < texts.length - 1) {
                concatenation.append(", ");
            }
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithMirroredCondition(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            concatenation.append(texts[i]);
            if (texts.length - 1 > i) {
                concatenation.append(", ");
            }
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithNotEqualsCondition(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            concatenation.append(texts[i]);
            if (i < texts.length - 1) {
                concatenation.append(", ");
            }
        }

        return concatenation.toString();
    }

    public String refactorConcatenationWithLessOrEqualsCondition(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            concatenation.append(texts[i]);
            if (i <= texts.length - 2) {
                concatenation.append(", ");
            }
        }

        return concatenation.toString();
    }

    public String refactorConcatenationTestingLength(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (concatenation.length() > 0) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationTestingNotEmpty(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (concatenation.length() != 0) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationTestingGreaterOrEqualsOne(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (concatenation.length() >= 1) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationTestingLengthMirrored(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (0 < concatenation.length()) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationTestingNotEmptyMirrored(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (0 != concatenation.length()) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConcatenationTestingGreaterOrEqualsOneMirrored(String[] texts) {
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (int i = 0; i < texts.length; i++) {
            if (1 <= concatenation.length()) {
                concatenation.append(", ");
            }
            concatenation.append(texts[i]);
        }

        return concatenation.toString();
    }

    public String refactorConstantBooleanShift(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (String text : texts) {
            if (!isFirst) {
                concatenation.append(", ");
            }
            isFirst = false;
            concatenation.append(text);
        }

        return concatenation.toString();
    }

    public String refactorWithBooleanShiftAtTheEnd(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (String text : texts) {
            if (!isFirst) {
                concatenation.append(", ");
            }
            concatenation.append(text);
            isFirst = false;
        }

        return concatenation.toString();
    }

    public String refactorWithReversedBooleanShift(String[] texts) {
        boolean isNotFirst = false;
        StringBuilder concatenation = new StringBuilder();

        // Keep this comment
        for (String text : texts) {
            if (isNotFirst) {
                concatenation.append(", ");
            }
            concatenation.append(text);
            isNotFirst = true;
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithBooleanShiftFirst(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            isFirst = false;
            if (!isFirst) {
                concatenation.append(", ");
            }
            concatenation.append(text);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithAppendingFirst(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            concatenation.append(text);
            if (!isFirst) {
                concatenation.append(", ");
            }
            isFirst = false;
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithConditionAtTheEnd(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            concatenation.append(text);
            isFirst = false;
            if (!isFirst) {
                concatenation.append(", ");
            }
        }

        return concatenation.toString();
    }

    public String doNotRefactorWithNonsense(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            isFirst = false;
            concatenation.append(text);
            if (!isFirst) {
                concatenation.append(", ");
            }
        }

        return concatenation.toString();
    }

    public String doNotRefactorUnshiftedBoolean(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            if (!isFirst) {
                concatenation.append(", ");
            }
            isFirst = true;
            concatenation.append(text);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWrongCondition(String[] texts) {
        boolean isFirst = true;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            if (isFirst) {
                concatenation.append(", ");
            }
            isFirst = false;
            concatenation.append(text);
        }

        return concatenation.toString();
    }

    public String doNotRefactorWrongInit(String[] texts) {
        boolean isFirst = false;
        StringBuilder concatenation = new StringBuilder();

        for (String text : texts) {
            if (!isFirst) {
                concatenation.append(", ");
            }
            isFirst = false;
            concatenation.append(text);
        }

        return concatenation.toString();
    }
}
