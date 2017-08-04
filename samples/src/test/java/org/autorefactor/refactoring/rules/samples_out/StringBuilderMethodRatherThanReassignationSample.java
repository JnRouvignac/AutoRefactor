/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - Initial API and implementation
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

public abstract class StringBuilderMethodRatherThanReassignationSample {

    private StringBuffer classBuffer = new StringBuffer();

    private StringBuilder classBuilder = new StringBuilder();

    public void removeReassignation(StringBuffer buffer, StringBuilder builder) {
        // Keep this comment
        buffer.append("foo");
        buffer.appendCodePoint(10);
        buffer.delete(1, 2);
        buffer.deleteCharAt(3);
        buffer.insert(4, "foo");
        buffer.replace(5, 6, "foo");
        buffer.reverse();

        // Keep this comment too
        builder.append("foo");
        builder.appendCodePoint(10);
        builder.delete(1, 2);
        builder.deleteCharAt(3);
        builder.insert(4, "foo");
        builder.replace(5, 6, "foo");
        builder.reverse();
    }

    public void removeReassignationOnSeveralCall(StringBuffer buffer, StringBuilder builder) {
        // Keep this comment
        buffer.append("f").appendCodePoint(1).delete(1, 2).deleteCharAt(3).insert(4, "f").replace(5, 6, "f").reverse();

        // Keep this comment too
        builder.append("f").appendCodePoint(1).delete(1, 2).deleteCharAt(3).insert(4, "f").replace(5, 6, "f").reverse();
    }

    public void removeEmbeddedReassignation(StringBuffer buffer, StringBuilder builder) {
        // Keep this comment
        if ((buffer.append("foo")) != null) {
            buffer.reverse();
        }

        // Keep this comment too
        if ((builder.append("foo")) != null) {
            builder.reverse();
        }
    }

    public void doNotRemoveDeclaration(StringBuffer buffer, StringBuilder builder) {
        StringBuffer localBuffer = buffer.append("foo");
        StringBuilder localBuilder = builder.append("foo");
    }

    public void doNotRefactorAutoAssignment(StringBuffer buffer, StringBuilder builder) {
        buffer = buffer;
        builder = builder;
    }

    public void doNotRefactorOtherClasses(String stringBuilder) {
        stringBuilder = stringBuilder.replace('a', 'b');
    }

    public void doNotRemoveOtherAssignation(StringBuffer buffer, StringBuilder builder) {
        int i = 0;

        StringBuffer localBuffer = new StringBuffer();
        localBuffer = buffer.append("foo");
        i = buffer.capacity();

        StringBuilder localBuilder = new StringBuilder();
        localBuilder = builder.append("foo");
        i = builder.capacity();
    }

    public void doNotRemoveClassVarAssignation(StringBuffer classBuffer, StringBuilder classBuilder) {
        this.classBuffer = classBuffer.append("foo");
        this.classBuilder = classBuilder.append("foo");
    }
}
