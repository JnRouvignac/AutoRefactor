/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.cfg;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.Type;

/**
 * Represents an access to a variable.
 */
public class VariableAccess {

    /** A declaration which value is already initialized. */
    public static final int DECL_INIT = 1 << 0;
    /** A declaration which value may or may not be initialized. */
    public static final int DECL_UNINIT = 1 << 1;
    /** A variable read. */
    public static final int READ = 1 << 2;
    /** A variable write. */
    public static final int WRITE = 1 << 4;

    private final ASTNode astNode;
    private final Name name;
    private final Type type;
    private final int accessType;

    /**
     * Class constructor.
     *
     * @param astNode the AST node of the variable
     * @param name the name of the variable
     * @param type the type of the variable
     * @param accessType the access type to the variable
     */
    public VariableAccess(ASTNode astNode, Name name, Type type, int accessType) {
        this.astNode = astNode;
        this.name = name;
        this.type = type;
        this.accessType = accessType;
    }

    /**
     * Class constructor.
     *
     * @param astNode the AST node of the variable
     * @param accessType the access type to the variable
     */
    public VariableAccess(ASTNode astNode, int accessType) {
        this(astNode, astNode instanceof Name ? (Name) astNode : null, null, accessType);
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("VAR_ACCESS[");
        toString(sb);
        return sb.append("]").toString();
    }

    private void toString(final StringBuilder sb) {
        sb.append(this.type.toString());
        sb.append(" ").append(this.name.toString()).append(" <= ");
        if ((this.accessType & DECL_INIT) != 0) {
            sb.append("DECL_INIT");
        }
        if ((this.accessType & DECL_UNINIT) != 0) {
            if (sb.length() > 0) {
                sb.append("|");
            }
            sb.append("DECL_UNINIT");
        }
        if ((this.accessType & READ) != 0) {
            if (sb.length() > 0) {
                sb.append("|");
            }
            sb.append("READ");
        }
        if ((this.accessType & WRITE) != 0) {
            if (sb.length() > 0) {
                sb.append("|");
            }
            sb.append("WRITE");
        }
    }
}
