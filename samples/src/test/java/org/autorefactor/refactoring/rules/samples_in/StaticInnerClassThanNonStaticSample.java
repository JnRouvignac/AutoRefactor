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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules.samples_in;

import static java.lang.Integer.bitCount;

import java.io.File;
import java.util.Arrays;

public class StaticInnerClassThanNonStaticSample {
    public class RefactorThisInnerClass {
        int i;

        public boolean anotherMethod() {
            return true;
        }
    }

    public interface DoNotRefactorInnerInterface {
        boolean anotherMethod();
    }

    public class RefactorThisInnerClassThatUsesStaticField {
        int i;

        public boolean anotherMethod() {
            return CONSTANT != null;
        }
    }

    public class DoNotRefactorThisInnerClass {
        int i;

        public boolean anotherMethod() {
            return aString != null;
        }
    }

    public class RefactorInnerClassThatOnlyUsesItsFields {
        int i;

        public boolean anotherMethod() {
            return i == 0;
        }
    }

    public class RefactorInnerClassThatUsesStaticMethod {
        int i;

        public boolean anotherMethod() {
            return aStaticMethod();
        }
    }

    public class DoNotRefactorInnerClassThatUsesMethod {
        int i;

        public boolean anotherMethod() {
            return aMethod();
        }
    }

    public static class DoNotRefactorAlreadyStaticInnerClass {
        int i;

        public boolean anotherMethod() {
            return true;
        }
    }

    public final class RefactorThisFinalInnerClass {
        int i;

        public boolean anotherMethod() {
            return true;
        }
    }

    class RefactorThisInnerClassWithoutModifier {
        int i;

        public boolean anotherMethod() {
            return true;
        }
    }

    @Deprecated
    class RefactorThisInnerClassWithAnnotation {
        int i;

        public boolean anotherMethod() {
            return true;
        }
    }

    public class RefactorInnerClassThatUsesStaticImport {
        int i;

        public int anotherMethod() {
            return bitCount(0);
        }
    }

    public class RefactorInnerClassThatUsesStaticField {
        int i;

        public char anotherMethod() {
            return File.separatorChar;
        }
    }

    public class RefactorInheritedInnerClass extends File {
        private static final long serialVersionUID = -1124849036813595100L;
        private int i;

        public RefactorInheritedInnerClass(File arg0, String arg1) {
            super(arg0, arg1);
        }

        public boolean anotherMethod() {
            return true;
        }
    }

    public class RefactorGenericInnerClass<T> {
        T i;

        public boolean anotherMethod() {
            return true;
        }
    }

    public class NotStaticClass {
        public class DoNotRefactorInnerClassInNotStaticClass {
            int i;

            public boolean anotherMethod() {
                return true;
            }
        }

        public boolean anotherMethod() {
            return aMethod();
        }
    }

    private static final String CONSTANT= "foo";

    private String aString= "bar";

    public static boolean aStaticMethod() {
        return false;
    }

    public boolean aMethod() {
        return true;
    }
}
