/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

import java.beans.ConstructorProperties;
import java.util.Date;
import java.util.Observable;

public class ImplicitDefaultConstructorRatherThanWrittenOneSample {
    class RemoveDefaultConstructor {
    }

    class RemoveDefaultConstructorWithSuperCall {
    }

    class RemoveDefaultConstructorWithInheritance extends Observable {
    }

    class RemoveDefaultConstructorWithSuperInheritedCall extends Observable {
    }

    class DoNotRemoveProtectedConstructor {
        protected DoNotRemoveProtectedConstructor() {}
    }

    class DoNotRemoveProtectedConstructorWithSuperCall {
        protected DoNotRemoveProtectedConstructorWithSuperCall() {
            super();
        }
    }

    class DoNotRemoveAnnotatedConstructor {
        @ConstructorProperties(value = { "We need an annotation" })
        public DoNotRemoveAnnotatedConstructor() {}
    }

    class DoNotRemoveConstructorWithParameter {
        public DoNotRemoveConstructorWithParameter(int i) {}
    }

    class DoNotRemoveConstructorWithVararg {
        public DoNotRemoveConstructorWithVararg(int... i) {}
    }

    class DoNotRemoveConstructorWithParameterizedSuperCall extends Date {
        private static final long serialVersionUID = -2817725354012950325L;

        public DoNotRemoveConstructorWithParameterizedSuperCall() {
            super(0);
        }
    }

    class DoNotRemoveDefaultConstructorWithOtherConstructors {
        public DoNotRemoveDefaultConstructorWithOtherConstructors() {}
        public DoNotRemoveDefaultConstructorWithOtherConstructors(int i) {}
    }

    class RemoveDefaultConstructorWithMethod {
        public void process() {}
    }
}
