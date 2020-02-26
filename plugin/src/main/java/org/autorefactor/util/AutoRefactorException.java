/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2015 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;

/**
 * This is the base class for exceptions used in AutoRefactor.
 * <p>
 * It provides services for locating the closest possible source code location
 * that caused a failure.
 */
public class AutoRefactorException extends RuntimeException {
    /**
     * Constructor.
     *
     * @param node the node from which to retrieve the source location
     */
    public AutoRefactorException(final ASTNode node) {
        super(ASTNodes.getSourceLocation(node));
    }

    /**
     * Constructor.
     *
     * @param node    the node from which to retrieve the source location
     * @param message the exception message
     */
    public AutoRefactorException(final ASTNode node, final String message) {
        super(buildMessage(node, message));
    }

    /**
     * Constructor.
     *
     * @param node  the node from which to retrieve the source location
     * @param cause the cause
     */
    public AutoRefactorException(final ASTNode node, final Throwable cause) {
        super(ASTNodes.getSourceLocation(node), cause);
    }

    /**
     * Constructor.
     *
     * @param node    the node from which to retrieve the source location
     * @param message the exception message
     * @param cause   the cause
     */
    public AutoRefactorException(final ASTNode node, final String message, final Throwable cause) {
        super(buildMessage(node, message), cause);
    }

    private static String buildMessage(final ASTNode node, final String message) {
        String sourceLocation= ASTNodes.getSourceLocation(node);
        if (!sourceLocation.isEmpty()) {
            return sourceLocation + ":" + message; //$NON-NLS-1$
        }

        return message;
    }
}
