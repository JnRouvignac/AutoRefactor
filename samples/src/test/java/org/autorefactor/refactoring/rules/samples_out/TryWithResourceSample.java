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

import java.io.FileInputStream;

public class TryWithResourceSample {
    public void refactorFullyInitializedResourceRemoveFinally() throws Exception {
        try (FileInputStream inputStream = new FileInputStream("out.txt")) {
            System.out.println(inputStream.read());
        }
    }

    public void refactorFullyInitializedResourceDoNotRemoveFinally() throws Exception {
        try (FileInputStream inputStream = new FileInputStream("out.txt")) {
            System.out.println(inputStream.read());
        } finally {
            System.out.println("Done");
        }
    }

    public void refactorNullInitializedResourceRemoveFinally() throws Exception {
        try (FileInputStream inputStream = new FileInputStream("out.txt")) {
            System.out.println(inputStream.read());
        }
    }

    public void refactorNullInitializedResourceDoNotRemoveFinally() throws Exception {
        try (FileInputStream inputStream = new FileInputStream("out.txt")) {
            System.out.println(inputStream.read());
        } finally {
            System.out.println("Done");
        }
    }

    public void doNotRefactorNonEffectivelyFinalResource() throws Exception {
        try (FileInputStream inputStream = new FileInputStream("out.txt")) {
            System.out.println(inputStream.read());
        }
    }

    public void doNotRefactorFurtherAssignmentsToResource() throws Exception {
        FileInputStream inputStream = null;
        try {
            inputStream = new FileInputStream("out.txt");
            System.out.println(inputStream.read());
            inputStream = new FileInputStream("out.txt");
        } finally {
            inputStream.close();
        }
    }

    public void removeInnerTry() throws Exception {
        try (final FileInputStream inputStream = new FileInputStream("out.txt")) {
            {
                System.out.println(inputStream.read());
            }
        } catch (Exception e) {
            throw e;
        }
    }

    public void doNotRemoveInnerTryWithCatchClause() throws Exception {
        try {
            try (final FileInputStream inputStream = new FileInputStream("out.txt")) {
                System.out.println(inputStream.read());
            } catch (Exception e) {
                e.printStackTrace();
            }
        } catch (Exception e) {
            throw e;
        }
    }

    public void doNotRefactorUnrelated() throws Exception {
        FileInputStream inputStream = new FileInputStream("out.txt");
        Object o = null;
        try {
            o = inputStream.read();
        } finally {
            inputStream.close();
        }
    }
}
