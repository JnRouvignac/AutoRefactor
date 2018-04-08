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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.NumberLiteral;

/**
 * See {@link #getDescription()} method.
 *
 * This rule refactors the Sonar squid:LowerCaseLongSuffixCheck.
 */
public class CapitalizeLongLiteralRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Capitalize lower case 'l' -> 'L' for long number literals";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Capitalize lower case 'l' -> 'L' for long number literals";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It disambiguates the code to reduce bug hazard.";
    }

    @Override
    public boolean visit(NumberLiteral node) {
        final String token = node.getToken();
        if (token.endsWith("l")) {
            replaceLong(node, token);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private void replaceLong(final NumberLiteral node, final String token) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final NumberLiteral replacement = b.numberLiteral();
        final String newToken = token.substring(0, token.length() - 1) + "L";
        replacement.setToken(newToken);
        ctx.getRefactorings().replace(node, replacement);
    }

}
