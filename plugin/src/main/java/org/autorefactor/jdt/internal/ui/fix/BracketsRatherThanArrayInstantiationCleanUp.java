/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - initial API and implementation
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
package org.autorefactor.jdt.internal.ui.fix;

import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.VISIT_SUBTREE;
import static org.autorefactor.jdt.internal.corext.dom.ASTNodes.getDestinationType;
import static org.autorefactor.util.Utils.equalNotNull;

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;

/** See {@link #getDescription()} method. */
public class BracketsRatherThanArrayInstantiationCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_BracketsRatherThanArrayInstantiationCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_BracketsRatherThanArrayInstantiationCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_BracketsRatherThanArrayInstantiationCleanUp_reason;
    }

    @Override
    public boolean visit(ArrayCreation node) {
        if (node.getInitializer() != null || isVoid(node)) {
            final ITypeBinding arrayType = node.resolveTypeBinding();
            final ITypeBinding destinationType = getDestinationType(node);

            if (equalNotNull(arrayType, destinationType) && isDestinationAllowed(node)) {
                refactorWithInitializer(node);
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    private void refactorWithInitializer(final ArrayCreation node) {
        if (node.getInitializer() != null) {
            ctx.getRefactorings().replace(node, node.getInitializer());
        } else {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.copy(b.arrayInitializer()));
        }
    }

    private boolean isDestinationAllowed(final ASTNode node) {
        final int parentType = node.getParent().getNodeType();

        switch (parentType) {
        case ASTNode.FIELD_DECLARATION:
        case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
        case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
        case ASTNode.VARIABLE_DECLARATION_STATEMENT:
            return true;

        default:
            return false;
        }
    }

    private boolean isVoid(final ArrayCreation node) {
        @SuppressWarnings("unchecked")
        final List<Expression> dimensions = (List<Expression>) node.dimensions();

        for (final Expression dimension : dimensions) {
            final Object dimensionLiteral = dimension.resolveConstantExpressionValue();

            if (!(dimensionLiteral instanceof Number)
                    || ((Number) dimensionLiteral).longValue() != 0) {
                return false;
            }
        }

        return true;
    }
}
