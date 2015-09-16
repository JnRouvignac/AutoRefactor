/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

/** Ignore the '{' character in this javadoc. */
/* And the '{' character in this block comment. */
// And the '{' character in this line comment.
public class RemoveEmptyLinesSample
{
    private int aField;

    private int anotherField;

    /** Ignore the '{' character in this javadoc. */
    public void aMethod() throws Exception
    {
        System.out.println("Remove empty line before me");
    }

    public void anotherMethod() throws Exception
    {
    }

    public void doNotRefactorMultipleBracesSameLine() throws Exception {
        try {
        } catch (Exception e) { }
    }

    public void doNotRefactorTrailingWhitespaceAfterBraces() throws Exception { 
        try { 
          System.out.println();
        } catch (Exception e) { 
        }
    }

    public void doNotRemoveCodeOnSameLineAsTry() {
        try { Thread.sleep(100);
        }
        catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
    }

    private interface MethodDeclarationsWithoutBody
    {
        void aMethod();

        boolean aBoolean();

        int anInt();
    }

    private static enum WeekOfDay { MONDAY, TUESDAY, WEDNESDAY, THRUSDAY, FRIDAY, SATURDAY, SUNDAY }
}
