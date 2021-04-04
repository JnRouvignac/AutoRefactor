/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
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

public class CharacterParameterRatherThanStringSample {
    public void refactorIndexOf() {
        String b = "b";
        "a".indexOf('a', 0);
        b.indexOf('a');
        b.trim().indexOf('\t', 0);
        "a".indexOf('\n');
    }

    public void refactorlastIndexOfCases() {
        String b = "b";
        "a".lastIndexOf('a', 0);
        b.lastIndexOf('a');
        b.toLowerCase().lastIndexOf('\t', 0);
        "a".lastIndexOf('\n');
    }

    public void doNotRefactorInvocationsOtherThanOneChar() {
        "a".indexOf(1);
        "a".indexOf(1, 0);
        "a".indexOf("as", 0);
        "a".indexOf("\\t", 0);
        "a".indexOf("as");
        "a".indexOf("\\b");
        "a".lastIndexOf(1);
        "a".lastIndexOf(1, 0);
        "a".lastIndexOf("as", 0);
        "a".lastIndexOf("\\t", 0);
        "a".lastIndexOf("as");
        "a".lastIndexOf("\\b");
    }
}
