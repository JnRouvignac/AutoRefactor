/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.Iterator;
import java.util.List;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Expression;

/** Helper class for printing AST information. */
public final class ASTPrintHelper {
    private ASTPrintHelper() {
        // Hide utility class ctor
    }

    static String codeExcerpt(final List<Expression> expressions) {
        StringBuilder sb= new StringBuilder();
        for (final Iterator<Expression> iter= expressions.iterator(); iter.hasNext();) {
            Expression expression= iter.next();
            sb.append(expression);
            if (iter.hasNext()) {
                sb.append(", "); //$NON-NLS-1$
            }
        }

        return sb.toString();
    }

    static String codeExcerpt(final ASTNode node) {
        String nodeString= node.toString();
        String[] nodeLines= nodeString.split("\n"); //$NON-NLS-1$
        String codeExcerpt;
        if (nodeLines[0].matches("\\s*\\{\\s*")) { //$NON-NLS-1$
            codeExcerpt= nodeLines[0] + " " + nodeLines[1] + " ..."; //$NON-NLS-1$ //$NON-NLS-2$
        } else {
            codeExcerpt= nodeLines[0];
        }

        return codeExcerpt.replaceAll("\\s+", " "); //$NON-NLS-1$ //$NON-NLS-2$
    }
}
