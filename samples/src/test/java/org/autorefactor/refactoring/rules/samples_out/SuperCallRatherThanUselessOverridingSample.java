/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Split the code
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

public class SuperCallRatherThanUselessOverridingSample {

    public class Parent {
        void removeUselessOverride() {
        }
        void removeOverrideWithInsignificantAnnotations() {
        }
        void doNotRemoveSignificantAnnotation() {
        }
        protected void doNotRemoveVisibilityChange() {
        }
        public void doNotRemoveOverrideWithOtherParam(String classname) {
        }
        public void doNotRemoveOverrideWithUnorderedParam(String firstname, String lastname) {
        }
    }

    public class Child extends Parent {
        @Deprecated
        @Override
        void doNotRemoveSignificantAnnotation() {
            super.doNotRemoveSignificantAnnotation();
        }

        @Override
        public void doNotRemoveVisibilityChange() {
            super.doNotRemoveVisibilityChange();
        }

        @Override
        public void doNotRemoveOverrideWithOtherParam(String classname) {
            super.doNotRemoveOverrideWithOtherParam("java.util." + classname);
        }

        @Override
        public void doNotRemoveOverrideWithUnorderedParam(String firstname, String lastname) {
            super.doNotRemoveOverrideWithUnorderedParam(lastname, firstname);
        }
    }
}
