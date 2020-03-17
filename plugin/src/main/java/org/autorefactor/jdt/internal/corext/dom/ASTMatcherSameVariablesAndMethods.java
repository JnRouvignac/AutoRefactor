/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015-2016 Jean-Noël Rouvignac - initial API and implementation
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
package org.autorefactor.jdt.internal.corext.dom;

import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;

/**
 * Matches two piece of code only if the variables/methods in use are the same.
 */
public final class ASTMatcherSameVariablesAndMethods extends ASTSemanticMatcher {
    @Override
    public boolean match(final SimpleName node, final Object other) {
        return super.match(node, other) && sameReference(node, (SimpleName) other);
    }

    private boolean sameReference(final SimpleName node1, final SimpleName node2) {
        return Utils.equalNotNull(getDeclaration(node1), getDeclaration(node2));
    }

    private IBinding getDeclaration(final SimpleName node) {
        IBinding ast= node.resolveBinding();
        if (ast != null) {
            switch (ast.getKind()) {
            case IBinding.VARIABLE:
                return ((IVariableBinding) ast).getVariableDeclaration();

            case IBinding.METHOD:
                return ((IMethodBinding) ast).getMethodDeclaration();
            }

            return ast;
        }

        return null;
    }
}
