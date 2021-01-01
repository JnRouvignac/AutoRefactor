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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.List;

public class EndOfLoopRatherThanContinueSample {
    public void removeUselessContinue(List<String> texts) {
        for (String text : texts) {
            continue;
        }
    }

    public void doNotRemoveBreak(List<String> texts) {
        for (String text : texts) {
            break;
        }
    }

    public void doNotRemoveReturn(List<String> texts) {
        for (String text : texts) {
            return;
        }
    }

    public void doNotRemoveThrow(List<String> texts) {
        for (String text : texts) {
            throw new NullPointerException();
        }
    }

    public void removeUselessContinueWithPreviousCode(List<String> texts) {
        for (String text : texts) {
            System.out.println("Keep this line");
            continue;
        }
    }

    public void doNotRemoveContinueWithLabel(List<String> texts, List<String> otherTexts) {
        begin: for (String text : texts) {
            for (String otherText : otherTexts) {
                System.out.println("Keep this line");
                continue begin;
            }
        }
    }

    public void removeUselessContinueWithIf(List<String> texts, boolean isValid) {
        for (String text : texts) {
            if (isValid) {
                System.out.println("Keep this line");
                continue;
            }
        }
    }

    public void replaceByBlock(List<String> texts, boolean isValid) {
        for (String text : texts) {
            System.out.println("Keep this line");
            if (isValid)
                continue;
        }
    }

    public void removeElseStatement(List<String> texts, boolean isValid) {
        for (String text : texts) {
            System.out.println("Keep this line");
            if (isValid)
                System.out.println("isValid is true");
            else
                continue;
        }
    }

    public void removeElseBlock(List<String> texts, boolean isValid) {
        for (String text : texts) {
            System.out.println("Keep this line");
            if (isValid) {
                System.out.println("isValid is true");
            } else {
                continue;
            }
        }
    }

    public void removeUselessContinueWithSwitch(List<String> texts, int myNumber) {
        for (String text : texts) {
            switch (myNumber) {
            case 0:
                System.out.println("Keep this line");
                continue;
            }
        }
    }

    public void doNotRemoveUselessContinueInMiddleOfSwitch(List<String> texts, int myNumber) {
        for (String text : texts) {
            switch (myNumber) {
            case 0:
                System.out.println("I'm not the last statement");
                continue;
            case 1:
                System.out.println("Do some stuff");
                break;
            }
        }
    }

    public void removeUselessContinueWithIfElse(List<String> texts, boolean isValid) {
        for (String text : texts) {
            if (isValid) {
                System.out.println("Keep this line");
                continue;
            } else {
                System.out.println("Remove anyway");
            }
        }
    }

    public void doNotRemoveContinueWithFollowingCode(List<String> texts, boolean isValid) {
        for (String text : texts) {
            if (isValid) {
                System.out.println("Keep this line");
                continue;
            }
            System.out.println("Keep this line");
        }
    }
}
