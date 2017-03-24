/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2017 Fabrice Tiercelin - Avoid to break the workflow
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

public class CommonIfInIfElseSample {

    public void refactorCommonInnerIf(boolean b1, boolean b2) throws Exception {
        if (b1) {
            if (b2) {
                // Keep this comment
                System.out.println(b1);
            }
        } else {
            if (b2) {
                // Keep this comment
                System.out.println(!b1);
            }
        }
    }

    public void doNotRefactorBecauseOfInnerElse1(boolean b1, boolean b2) throws Exception {
        if (b1) {
            if (b2) {
                System.out.println(b2);
            } else {
                System.out.println(b1);
            }
        } else {
            if (b2) {
                System.out.println(!b1);
            }
        }
    }

    public void doNotRefactorBecauseOfInnerElse2(boolean b1, boolean b2) throws Exception {
        if (b1) {
            if (b2) {
                System.out.println(b1);
            }
        } else {
            if (b2) {
                System.out.println(b2);
            } else {
                System.out.println(!b1);
            }
        }
    }

    public void doNotRefactorActiveCondition(List<String> myList) throws Exception {
        if (myList.remove("lorem")) {
            if (myList.isEmpty()) {
                System.out.println("Now empty");
            }
        } else {
            if (myList.isEmpty()) {
                System.out.println("Still empty");
            }
        }
    }

    public void doNotRefactorAssignment(boolean b1, boolean b2) throws Exception {
        if (b2 = b1) {
            if (b2) {
                System.out.println(b1);
            }
        } else {
            if (b2) {
                System.out.println(!b1);
            }
        }
    }

    public void doNotRefactorPostincrement(int i1, int i2) throws Exception {
        if (i1 == i2++) {
            if (i2 == 0) {
                System.out.println(i1);
            }
        } else {
            if (i2 == 0) {
                System.out.println(-i1);
            }
        }
    }

    public void doNotRefactorPreincrement(int i1, int i2) throws Exception {
        if (i1 == ++i2) {
            if (i2 == 0) {
                System.out.println(i1);
            }
        } else {
            if (i2 == 0) {
                System.out.println(-i1);
            }
        }
    }
}
