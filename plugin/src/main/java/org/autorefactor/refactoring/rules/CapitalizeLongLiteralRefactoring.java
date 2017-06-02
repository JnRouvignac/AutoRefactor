/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.*;

import org.eclipse.jdt.core.dom.NumberLiteral;

/** See {@link #getDescription()} method. */
public class CapitalizeLongLiteralRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Capitalize lower case 'l' -> 'L' for long number literals";
    }

    @Override
    public String getName() {
        return "Capitalize lower case 'l' -> 'L' for long number literals";
    }

    @Override
    public boolean visit(NumberLiteral node) {
        String token = node.getToken();
        if (token.endsWith("l")) {
            NumberLiteral replacement = ctx.getAST().newNumberLiteral();
            String newToken = token.substring(0, token.length() - 1) + "L";
            replacement.setToken(newToken);
            ctx.getRefactorings().replace(node, replacement);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

}
