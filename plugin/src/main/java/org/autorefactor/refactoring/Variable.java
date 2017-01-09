/*
 * The contents of this file are subject to the terms of the Common Development and
 * Distribution License (the License). You may not use this file except in compliance with the
 * License.
 *
 * You can obtain a copy of the License at legal/CDDLv1.0.txt. See the License for the
 * specific language governing permission and limitations under the License.
 *
 * When distributing Covered Software, include this CDDL Header Notice in each file and include
 * the License file at legal/CDDLv1.0.txt. If applicable, add the following below the CDDL
 * Header, with the fields enclosed by brackets [] replaced by your own identifying
 * information: "Portions Copyright [year] [name of copyright owner]".
 *
 * Copyright 2016 ForgeRock AS.
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
