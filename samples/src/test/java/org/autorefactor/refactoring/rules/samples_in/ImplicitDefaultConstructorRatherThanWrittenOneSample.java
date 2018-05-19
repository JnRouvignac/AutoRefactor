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
package org.autorefactor.refactoring.rules.samples_in;

import java.beans.ConstructorProperties;
import java.io.Serializable;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.text.ParseException;
import java.util.Date;
import java.util.Observable;

public class ImplicitDefaultConstructorRatherThanWrittenOneSample {
    public ImplicitDefaultConstructorRatherThanWrittenOneSample() {
    }

    public class RemoveDefaultConstructor {
        public RemoveDefaultConstructor() {}
    }

    public class RemoveDefaultConstructorWithSuperCall {
        public RemoveDefaultConstructorWithSuperCall() {
            super();
        }
    }

    public class DoNotRemoveConstructorWithCode {
        public DoNotRemoveConstructorWithCode() {
            System.out.println("Don't lose me!");
        }
    }

    class DoNotRemovePackageConstructorWithCode {
        DoNotRemovePackageConstructorWithCode() {
            System.out.println("Don't lose me!");
        }
    }

    public class DoNotRemoveConstructorWithSuperAndCode {
        public DoNotRemoveConstructorWithSuperAndCode() {
            super();
            System.out.println("Don't lose me!");
        }
    }

    public class RemoveDefaultConstructorWithInheritance extends Observable {
        public RemoveDefaultConstructorWithInheritance() {}
    }

    public class RemoveDefaultConstructorWithSuperInheritedCall extends Observable {
        public RemoveDefaultConstructorWithSuperInheritedCall() {
            super();
        }
    }

    protected class RemoveProtectedConstructor {
        protected RemoveProtectedConstructor() {}
    }

    class RemovePackageConstructor {
        RemovePackageConstructor() {}
    }

    private class RemovePrivateConstructor {
        private RemovePrivateConstructor() {}
    }

    public class DoNotRemoveProtectedConstructor {
        protected DoNotRemoveProtectedConstructor() {}
    }

    public class DoNotRemovePackageConstructor {
        DoNotRemovePackageConstructor() {}
    }

    public class DoNotRemovePrivateConstructor {
        private DoNotRemovePrivateConstructor() {}
    }

    protected class DoNotRemovePublicConstructor {
        public DoNotRemovePublicConstructor() {}
    }

    public class DoNotRemoveProtectedConstructorWithSuperCall {
        protected DoNotRemoveProtectedConstructorWithSuperCall() {
            super();
        }
    }

    public class DoNotRemoveAnnotatedConstructor {
        @ConstructorProperties(value = { "We need an annotation" })
        public DoNotRemoveAnnotatedConstructor() {}
    }

    public class DoNotRemoveConstructorWithParameter {
        public DoNotRemoveConstructorWithParameter(int i) {}
    }

    public class DoNotRemoveConstructorWithVararg {
        public DoNotRemoveConstructorWithVararg(int... i) {}
    }

    public class DoNotRemoveConstructorWithParameterizedSuperCall extends Date {
        private static final long serialVersionUID = -2817725354012950325L;

        public DoNotRemoveConstructorWithParameterizedSuperCall() {
            super(0);
        }
    }

    public class DoNotRemoveDefaultConstructorWithOtherConstructors {
        public DoNotRemoveDefaultConstructorWithOtherConstructors() {}
        public DoNotRemoveDefaultConstructorWithOtherConstructors(int i) {}
    }

    public class RemoveDefaultConstructorWithMethod {
        public RemoveDefaultConstructorWithMethod() {}
        public void process() {}
    }

    public class RemoveDefaultConstructorWithCheckedException {
        public RemoveDefaultConstructorWithCheckedException() throws ParseException {}
    }

    public class RemoveSerializableConstructorWithCheckedException implements Serializable {
        public RemoveSerializableConstructorWithCheckedException() throws ParseException {}
    }

    public class DoNotRemoveInheritedCheckedException extends UnicastRemoteObject {
        public DoNotRemoveInheritedCheckedException() throws RemoteException {}
    }

    public class RemoveDefaultConstructorWithRuntimeException extends Date {
        public RemoveDefaultConstructorWithRuntimeException() throws NullPointerException {}
    }
}
