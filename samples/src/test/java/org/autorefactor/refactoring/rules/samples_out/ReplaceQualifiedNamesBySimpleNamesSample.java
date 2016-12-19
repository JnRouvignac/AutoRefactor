/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Jean-Noël Rouvignac - initial API and implementation
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

public class ReplaceQualifiedNamesBySimpleNamesSample {
    public List<String> removeQualifiedNameForImportNoWildcard(List<String> l) {
        return l;
    }

    public Calendar removeQualifiedNameForImportWithWildcard(Calendar cal) {
        return cal;
    }

    public int removeQualifiedNameForStaticImportWithWildcard() {
        return DATE;
    }

    public boolean removeQualifiedNameForStaticMethodImport() {
        return emptyList().containsAll(emptyList());
    }

    public boolean removeQualifiedNameForStaticFieldImport() {
        return EMPTY_LIST.containsAll(EMPTY_LIST);
    }

    public void doNotRemoveQualifiedNameForGenericStaticMethodImport() {
        acceptListString(Collections.<String> emptyList());
    }

    private void acceptListString(List<String> l) {
        // nothing to do
    }

    public boolean removeQualifiedNameForStaticTypeImport(Entry<?, ?> e) {
        return e.getKey() != null;
    }

    public BigDecimal doNotRemoveQualifiedNameForConflictingFieldSimpleNameFromStaticImports() {
        return BigDecimal.ZERO;
    }

    public BigDecimal removeQualifiedNameForOverridingFieldSimpleNameFromStaticImports() {
        return ONE;
    }

    public Map<Object, Object> doNotRemoveQualifiedNameForConflictingMethodSimpleName() {
        return Collections.emptyMap();
    }

    public int removeQualifiedNameForNonConflictingLocalMethodSimpleName() {
        return emptyMap();
    }

    private static int emptyMap() {
        return 0;
    }

    public Boolean removeQualifiedNameForJavaLangPackage() {
        return Boolean.TRUE;
    }

    public java.lang.Integer doNotRemoveQualifiedNameForConflictingTypeFromJavaLangPackage() {
        return new java.lang.Integer(0);
    }

    public Integer removeQualifiedNameForNonConflictingLocalType() {
        return new Integer();
    }

    private static final class Integer {
    }

    public int doNotRemoveQualifiedNameForConflictingFieldSimpleName() {
        return Calendar.DAY_OF_WEEK_IN_MONTH;
    }

    public String removeQualifiedNameForNonConflictingLocalField() {
        return DAY_OF_WEEK_IN_MONTH;
    }

    private static final String DAY_OF_WEEK_IN_MONTH = "DAY_OF_WEEK_IN_MONTH";

    static class A {
        private int replaceFromEnclosingType() {
            return emptyMap();
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
        private int replaceFromCurrentType() {
            return emptyMap();
        }

        static int emptyMap() {
            return 0;
        }
    }

    static class D extends C {
        private int replaceFromSuperType() {
            return emptyMap();
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

    static String property = null;
    static void setProperty(String property) {
        ReplaceQualifiedNamesBySimpleNamesSample.property = property;
    }

}

class Outer {
    void foo() {
        ReplaceQualifiedNamesBySimpleNamesSample.setProperty("hi");
    }
}