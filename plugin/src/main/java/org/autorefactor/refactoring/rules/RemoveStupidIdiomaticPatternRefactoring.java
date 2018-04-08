/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2015 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.refactoring.rules;

import java.util.List;

/**
 * TODO JNR
 *
 * <pre>
 * public myMethod() throws MyException {
 *     int result;
 *     try {
 *         result = someOtherMethod();
 *     } catch (MyException2 e) {
 *         throw new MyException(e);
 *     }
 *     return result;
 * }
 * </pre>
 *
 * should be transformed into:
 *
 * <pre>
 * public myMethod() throws MyException {
 *     try {
 *         return someOtherMethod();
 *     } catch (MyException2 e) {
 *         throw new MyException(e);
 *     }
 * }
 * </pre>
 */
/** See {@link #getDescription()} method. */
public class RemoveStupidIdiomaticPatternRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Try statement then return";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Refactors to a proper use of try statements and return statements.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters."
                + " It also improves the time and space performance.";
    }

    // TODO JNR

    // public Object myMethod() {
    // Object result = null;
    // try {
    // result = myOtherMethod();
    // } catch (Exception e) {
    // throw e;
    // }
    // return result;
    // }

    private Object getModifiersOnly(List modifiers) {
        return null;
    }
}
