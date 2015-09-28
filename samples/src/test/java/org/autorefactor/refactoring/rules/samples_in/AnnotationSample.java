/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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

public class AnnotationSample {

    private @interface MyAnnotation {
        boolean booleanField() default true;
        char charField() default 'c';
        byte byteField() default 42;
        short shortField() default 42;
        int intField() default 42;
        long longField() default 42;
        float floatField() default 42;
        double doubleField() default 42;

        String stringField() default "";
        String[] stringArrayField() default {};
        String[] stringArrayFieldWithDefaults() default { "a", "b" };
    }

    @SuppressWarnings(value = "javadoc")
    @Override()
    public int hashCode() {
        return super.hashCode();
    }

    @MyAnnotation(
        booleanField = true,
        charField = 'c',
        byteField = 42,
        shortField = 42,
        intField = 42,
        longField = 42,
        floatField = 42,
        doubleField = 42,
        stringArrayField = {},
        stringArrayFieldWithDefaults = { "a", "b" })
    public void refactorToMarkerAnnotation() throws Exception {
        return;
    }

    @MyAnnotation(
        booleanField = true,
        charField = 'c',
        byteField = 0x2a,
        shortField = 42,
        intField = 42,
        longField = 42L,
        floatField = 42.0f,
        doubleField = 42.0d,
        stringArrayField = {},
        stringArrayFieldWithDefaults = { "a", "b" })
    public void refactorToMarkerAnnotation2() throws Exception {
        return;
    }

    @MyAnnotation(
        booleanField = false,
        charField = 'z',
        byteField = 1,
        shortField = 1,
        intField = 1,
        longField = 1,
        floatField = 1,
        doubleField = 1,
        stringArrayField = { "", "" },
        stringArrayFieldWithDefaults = {})
    public void doNotRefactorNotUsingDefaults() throws Exception {
        return;
    }

    @MyAnnotation(stringArrayField = { "refactorToMarkerAnnotation" })
    public void removeCurlyBraces() throws Exception {
        return;
    }
}
