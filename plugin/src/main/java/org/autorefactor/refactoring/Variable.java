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
package org.autorefactor.refactoring;

import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/**
 * Represents a variable being written in the source code, including its name and possibly its type.
 * <p>
 * It helps generate various declarations and accesses to this variable.
 */
public class Variable {
    private final String typeName;
    private final String variableName;
    private final ASTBuilder b;

    /**
     * Builds a variable with its name and its type.
     *
     * @param typeName the variable's type name
     * @param variableName the variable's name
     * @param astBuilder the builder to build new AST nodes
     */
    public Variable(String typeName, String variableName, ASTBuilder astBuilder) {
        this.typeName = typeName;
        this.variableName = variableName;
        this.b = astBuilder;
    }

    /**
     * Builds a variable with its name and no type information.
     * Calling any method dealing with the type will throw an exception.
     *
     * @param variableName the variable's name
     * @param astBuilder the builder to build new AST nodes
     */
    public Variable(String variableName, ASTBuilder astBuilder) {
        this(null, variableName, astBuilder);
    }

    /**
     * Builds a {@link SimpleName} holding the name of this variable.
     *
     * @return a new {@link SimpleName}
     */
    public SimpleName varName() {
        return b.simpleName(variableName);
    }

    /**
     * Returns the name of this variable as a string.
     *
     * @return the variable's name
     */
    public String varNameRaw() {
        return variableName;
    }

    /**
     * Builds a {@link Type} holding the type of this variable.
     *
     * @return a new {@link SimpleType}
     */
    public Type type() {
        checkTypeDefined();
        return b.type(typeName);
    }

    /**
     * Builds a {@link SimpleName} holding the type name of this variable.
     *
     * @return a new {@link SimpleName}
     */
    public SimpleName typeName() {
        checkTypeDefined();
        return b.simpleName(typeName);
    }

    /**
     * Returns the type of this variable as a string.
     *
     * @return the variable's type name
     */
    public String typeNameRaw() {
        checkTypeDefined();
        return typeName;
    }

    private void checkTypeDefined() {
        if (typeName == null) {
            throw new IllegalStateException("This method cannot be called on variable '"
                + variableName + "', because no type has been defined for it.");
        }
    }

    /**
     * Builds a {@link VariableDeclarationStatement} with the name and type this variable.
     *
     * @return a new {@link VariableDeclarationStatement}
     */
    public VariableDeclarationStatement declareStmt() {
        return b.declareStmt(type(), varName(), null);
    }

    @Override
    public String toString() {
        return Variable.class.getSimpleName() + "[" + (typeName != null ? typeName + " " : "") + variableName + "]";
    }
}
