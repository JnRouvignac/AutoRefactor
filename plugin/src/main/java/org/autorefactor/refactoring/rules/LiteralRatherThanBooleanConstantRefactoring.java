/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016-2017 Fabrice Tiercelin - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.isField;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.autorefactor.refactoring.ASTHelper.resolveTypeBinding;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class LiteralRatherThanBooleanConstantRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Literal rather than boolean constant";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
            + "Replace Boolean.TRUE/Boolean.FALSE by true/false on primitive assignment.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility.";
    }

    @Override
    public boolean visit(QualifiedName node) {
        final ASTNode parent = removeParentheses(node.getParent());
        if (parent instanceof VariableDeclarationFragment) {
            final ITypeBinding typeBinding = resolveTypeBinding((VariableDeclarationFragment) parent);
            return replaceBooleanObjectByPrimitive(node, typeBinding);
        } else if (parent instanceof Assignment) {
            final ITypeBinding typeBinding = ((Assignment) parent).resolveTypeBinding();
            return replaceBooleanObjectByPrimitive(node, typeBinding);
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceBooleanObjectByPrimitive(final QualifiedName node, final ITypeBinding typeBinding) {
        if (typeBinding != null && typeBinding.isPrimitive()) {
            if (isField(node, "java.lang.Boolean", "TRUE")) {
                return replaceWithBooleanLiteral(node, true);
            } else if (isField(node, "java.lang.Boolean", "FALSE")) {
                return replaceWithBooleanLiteral(node, false);
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithBooleanLiteral(final QualifiedName node, final boolean val) {
        final BooleanLiteral booleanLiteral = this.ctx.getASTBuilder().boolean0(val);
        this.ctx.getRefactorings().replace(node, booleanLiteral);
        return DO_NOT_VISIT_SUBTREE;
    }
}
