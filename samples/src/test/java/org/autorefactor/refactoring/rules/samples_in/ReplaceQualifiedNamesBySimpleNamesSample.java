/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Handle local variable and outer classes
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

import static java.math.BigDecimal.ONE;
import static java.math.BigInteger.*;
import static java.util.Calendar.*;
import static java.util.Collections.EMPTY_LIST;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Map.Entry;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@SuppressWarnings("javadoc")
public class ReplaceQualifiedNamesBySimpleNamesSample {

    private static final String TOP_LEVEL_PRIVATE_CONSTANT = "visible from sublevel";

    static long classField;

    java.lang.Long instanceField = java.lang.Long.MIN_VALUE;

    static {
        long classField = 0l;
        ReplaceQualifiedNamesBySimpleNamesSample.classField = java.lang.Long.MAX_VALUE + classField;
    }

    public ReplaceQualifiedNamesBySimpleNamesSample(java.lang.Long long1) {
        java.lang.Long long2 = java.lang.Long.valueOf(long1 + 1);
    }

    public java.util.List<String> removeQualifiedNameForImportNoWildcard(List<String> l) {
        return l;
    }

    public java.util.Calendar removeQualifiedNameForImportWithWildcard(Calendar cal) {
        return cal;
    }

    public int removeQualifiedNameForStaticImportWithWildcard() {
        // Keep this comment
        return java.util.Calendar.DATE;
    }

    public boolean removeQualifiedNameForStaticMethodImport() {
        // Keep this comment
        return java.util.Collections.emptyList().containsAll(Collections.emptyList());
    }

    public boolean removeQualifiedNameForStaticFieldImport() {
        // Keep this comment
        return java.util.Collections.EMPTY_LIST.containsAll(Collections.EMPTY_LIST);
    }

    public void doNotRemoveQualifiedNameForGenericStaticMethodImport() {
        acceptListString(Collections.<String> emptyList());
    }

    public long removeQualifiedNameForParameterType(java.lang.Long i) {
        return i;
    }

    private void acceptListString(List<String> l) {
        // nothing to do
    }

    public boolean removeQualifiedNameForStaticTypeImport(Map.Entry<?, ?> e) {
        return e.getKey() != null;
    }

    public BigDecimal doNotRemoveQualifiedNameForConflictingFieldSimpleNameFromStaticImports() {
        return BigDecimal.ZERO;
    }

    public BigDecimal removeQualifiedNameForOverridingFieldSimpleNameFromStaticImports() {
        // Keep this comment
        return BigDecimal.ONE;
    }

    public Map<Object, Object> doNotRemoveQualifiedNameForConflictingMethodSimpleName() {
        return Collections.emptyMap();
    }

    public int removeQualifiedNameForNonConflictingLocalMethodSimpleName() {
        // Keep this comment
        return ReplaceQualifiedNamesBySimpleNamesSample.emptyMap();
    }

    private static int emptyMap() {
        return 0;
    }

    public java.lang.Boolean removeQualifiedNameForJavaLangPackage() {
        // Keep this comment
        return java.lang.Boolean.TRUE;
    }

    public java.lang.Integer doNotRemoveQualifiedNameForConflictingTypeFromJavaLangPackage() {
        return new java.lang.Integer(0);
    }

    public Integer removeQualifiedNameForNonConflictingLocalType() {
        // Keep this comment
        return new ReplaceQualifiedNamesBySimpleNamesSample.Integer();
    }

    private static final class Integer {
    }

    public int doNotRemoveQualifiedNameForConflictingFieldSimpleName() {
        return Calendar.DAY_OF_WEEK_IN_MONTH;
    }

    public String removeQualifiedNameForNonConflictingLocalField() {
        // Keep this comment
        return ReplaceQualifiedNamesBySimpleNamesSample.DAY_OF_WEEK_IN_MONTH;
    }

    private static final String DAY_OF_WEEK_IN_MONTH = "DAY_OF_WEEK_IN_MONTH";

    static class A {
        private int replaceFromEnclosingType() {
            // Keep this comment
            return ReplaceQualifiedNamesBySimpleNamesSample.emptyMap();
        }
    }

    static class B {
        private int doNotReplaceCurrentTypeDiffers() {
            return ReplaceQualifiedNamesBySimpleNamesSample.emptyMap();
        }

        static int emptyMap() {
            return 0;
        }
    }

    static class C {
        private static final String PRIVATE_CONSTANT = "not visible";

        private int replaceFromCurrentType() {
            // Keep this comment
            return C.emptyMap();
        }

        static int emptyMap() {
            return 0;
        }
    }

    static class D extends C {
        private int replaceFromSuperType() {
            // Keep this comment
            return C.emptyMap();
        }

        public void replaceThisTopLevelPrivateConstant() {
            System.out.println("value is " + ReplaceQualifiedNamesBySimpleNamesSample.TOP_LEVEL_PRIVATE_CONSTANT);
        }

        public void doNotRefactorThisPrivateConstant() {
            System.out.println("value is " + C.PRIVATE_CONSTANT);
        }
    }

    public class Z {
        public Integer i;

        private Boolean b() {
            return Boolean.FALSE;
        }

        public boolean isEqualTo(Z other) {
            if (other == null) {
                return false;
            }
            if (this == other) {
                return true;
            }
            return b().equals(other.b())
                && i.equals(other.i);
        }
    }

    public void doNotConflictInstanceFieldAndLocalVariable(long instanceField) {
        this.instanceField = instanceField;
    }

    public void doNotConflictClassFieldAndLocalVariable(long classField) {
        ReplaceQualifiedNamesBySimpleNamesSample.classField = classField;
    }

    static String property;
    static void setProperty(String property) {
        ReplaceQualifiedNamesBySimpleNamesSample.property = property;
    }

    public void doNotConflictClassFieldAndOuterClassField(String property) {
        Outer.property = property;
        Outer.NestedOuter.property = property;
    }

    static class Inner {
        static class InnerInner {
            // empty
        }
        private static final Inner INSTANCE = new Inner();
        private static Inner getInstance() {
            return INSTANCE;
        }
    }

    public String doNotReplaceFieldUseOfInnerClass() {
        return Inner.INSTANCE.toString();
    }

    public String doNotReplaceMethodUseOfInnerClass() {
        return Inner.getInstance().toString();
    }

    public Class<Inner.InnerInner> doNotReplaceTypeUseOfInnerClass() {
        return Inner.InnerInner.class;
    }
}

class Outer {
    static java.lang.String property;

    void doNotRefactorTopLevelClassCall() {
        ReplaceQualifiedNamesBySimpleNamesSample.setProperty("hi");
    }

    static class NestedOuter {
        private static final String PRIVATE_CONSTANT = "not visible";

        static java.lang.String property;

        void doNotRefactorTopLevelClassCall() {
            ReplaceQualifiedNamesBySimpleNamesSample.setProperty("hi");
        }
    }

    public static class OuterInheritedClass extends NestedOuter {
        public void doNotRefactorThisPrivateConstant() {
            System.out.println("value is " + NestedOuter.PRIVATE_CONSTANT);
        }
    }
}
