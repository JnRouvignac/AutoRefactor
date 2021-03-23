/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
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
package org.autorefactor.refactoring.rules.samples_in;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ObsoleteLocalVariableRatherThanFieldSample {
    private long refactorField;
    private long refactorInitializedField = 10L;
    private short refactorFieldWithComplexUse;
    private int refactorArray[];
    private int refactorOneFragment, severalUses;
    private static long refactorStaticField;
    public int doNotRefactorPublicField;
    protected int doNotRefactorProtectedField;
    int doNotRefactorPackageField;
    private int doNotRefactorFieldsInSeveralMethods;
    private int refactorFieldWithSameNameAsLocalVariable;
    private int doNotRefactorFieldInOtherField;
    private int oneField = doNotRefactorFieldInOtherField;
    private int doNotRefactorReadFieldBeforeAssignment;
    private int doNotRefactorUnusedField;
    private int out;
    private List<String> dynamicList= new ArrayList<>(Arrays.asList("foo", "bar"));
    private boolean doNotRefactorFieldWithActiveInitializer = dynamicList.remove("foo");
    private Runnable doNotRefactorObject;
    @Deprecated
    private int doNotRefactorFieldWithAnnotation;

    public void refactorField() {
        // Keep this comment
        refactorField = 123;
        System.out.println(refactorField);
    }

    public void refactorInitializedField() {
        // Keep this comment
        refactorInitializedField = 123;
        System.out.println(refactorInitializedField);
    }

    public void refactorFieldWithComplexUse(boolean b, List<String> texts) {
        // Keep this comment
        refactorFieldWithComplexUse = 123;
        if (b) {
            System.out.println(refactorFieldWithComplexUse);
        } else {
            refactorFieldWithComplexUse = 321;

            for (String text : texts) {
                System.out.println(text);
                System.out.println(refactorFieldWithComplexUse);
            }
        }
    }

    public void refactorArray() {
        // Keep this comment
        refactorArray = new int[]{123};
        System.out.println(refactorArray);
    }

    public void refactorOneFragment() {
        // Keep this comment
        refactorOneFragment = 123;
        System.out.println(refactorOneFragment);
    }

    public void severalUses() {
        severalUses = 123;
        System.out.println(severalUses);
    }

    public void severalUses(int i) {
        severalUses = i;
        System.out.println(severalUses);
    }

    public void refactorStaticField() {
        // Keep this comment
        refactorStaticField = 123;
        System.out.println(refactorStaticField);
    }

    public void doNotRefactorPublicField() {
        doNotRefactorPublicField = 123;
        System.out.println(doNotRefactorPublicField);
    }

    public void doNotRefactorProtectedField() {
        doNotRefactorProtectedField = 123;
        System.out.println(doNotRefactorProtectedField);
    }

    public void doNotRefactorPackageField() {
        doNotRefactorPackageField = 123;
        System.out.println(doNotRefactorPackageField);
    }

    public void doNotRefactorFieldsInSeveralMethods() {
        doNotRefactorFieldsInSeveralMethods = 123;
        System.out.println(doNotRefactorFieldsInSeveralMethods);
    }

    public void doNotRefactorFieldsInSeveralMethods(int i) {
        doNotRefactorFieldsInSeveralMethods = i;
        System.out.println(doNotRefactorFieldsInSeveralMethods);
    }

    public void refactorFieldWithSameNameAsLocalVariable() {
        refactorFieldWithSameNameAsLocalVariable = 123;
        System.out.println(refactorFieldWithSameNameAsLocalVariable);
    }

    public void methodWithLocalVariable() {
        long refactorFieldWithSameNameAsLocalVariable = 123;
        System.out.println(refactorFieldWithSameNameAsLocalVariable);
    }

    public void doNotRefactorReadFieldBeforeAssignment() {
        System.out.println(doNotRefactorReadFieldBeforeAssignment);
        doNotRefactorReadFieldBeforeAssignment = 123;
        System.out.println(doNotRefactorReadFieldBeforeAssignment);
    }

    public void doNotRefactorFieldInOtherField() {
        doNotRefactorFieldInOtherField = 123;
        System.out.println(doNotRefactorFieldInOtherField);
    }

    public void refactorFieldWithSameNameAsAttribute() {
        out = 123;
        System.out.println(out);
    }

    public void doNotRefactorFieldWithActiveInitializer() {
        doNotRefactorFieldWithActiveInitializer = true;
        System.out.println(doNotRefactorFieldWithActiveInitializer);
    }

    public void doNotRefactorObject() {
        doNotRefactorObject = new Runnable() {
            @Override
            public void run() {
                while (true) {
                    System.out.println("Don't stop me!");
                }
            }
        };
        doNotRefactorObject.run();
    }

    public void doNotRefactorFieldWithAnnotation() {
        doNotRefactorFieldWithAnnotation = 123456;
        System.out.println(doNotRefactorFieldWithAnnotation);
    }
}
