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
package org.autorefactor.util;

import org.eclipse.jdt.core.dom.ASTNode;

/** Exception thrown when some code is not implemented, or when some conditions are not handled. */
public class NotImplementedException extends AutoRefactorException {
    private static final String DEFAULT_MESSAGE = "Code is not implemented";

    /**
     * Class constructor to use when some code is not implemented.
     *
     * @param node the node from which to retrieve the source location
     */
    public NotImplementedException(ASTNode node) {
        this(node, node);
    }

    /**
     * Constructor to use when the provided object was not expected.
     *
     * @param node the node from which to retrieve the source location
     * @param cause the unexpected object
     */
    public NotImplementedException(ASTNode node, Object cause) {
        this(node, "for an object of type " + (cause != null ? cause.getClass() : null));
    }

    /**
     * Constructor to use with a provided reason.
     *
     * @param node the node from which to retrieve the source location
     * @param reason an additional message
     */
    public NotImplementedException(ASTNode node, String reason) {
        super(node, DEFAULT_MESSAGE + " " + (reason != null ? reason : ""));
    }
}
