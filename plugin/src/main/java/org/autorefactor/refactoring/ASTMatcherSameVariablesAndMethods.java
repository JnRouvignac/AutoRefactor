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
package org.autorefactor.refactoring;

import static org.autorefactor.util.Utils.equalNotNull;

import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.SimpleName;

/** Matches two piece of code only if the variables/methods in use are the same. */
public final class ASTMatcherSameVariablesAndMethods extends ASTSemanticMatcher {
    @Override
    public boolean match(SimpleName node, Object other) {
        return super.match(node, other)
                && sameReference(node, (SimpleName) other);
    }

    private boolean sameReference(SimpleName node1, SimpleName node2) {
        return equalNotNull(getDeclaration(node1), getDeclaration(node2));
    }

    private IBinding getDeclaration(SimpleName node) {
        final IBinding b = node.resolveBinding();
        if (b != null) {
            switch (b.getKind()) {
            case IBinding.VARIABLE:
                return ((IVariableBinding) b).getVariableDeclaration();

            case IBinding.METHOD:
                return ((IMethodBinding) b).getMethodDeclaration();
            }

            return b;
        }
        return null;
    }
}
