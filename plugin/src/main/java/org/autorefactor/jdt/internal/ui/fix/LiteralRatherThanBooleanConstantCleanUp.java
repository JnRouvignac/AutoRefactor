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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.BooleanLiteral;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class LiteralRatherThanBooleanConstantCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_LiteralRatherThanBooleanConstantCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_LiteralRatherThanBooleanConstantCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_LiteralRatherThanBooleanConstantCleanUp_reason;
    }

    @Override
    public boolean visit(QualifiedName node) {
        final ASTNode parent= ASTNodes.getUnparenthesedExpression(node.getParent());
        if (parent instanceof VariableDeclarationFragment) {
            final ITypeBinding typeBinding= ASTNodes.resolveTypeBinding((VariableDeclarationFragment) parent);
            return replaceBooleanObjectByPrimitive(node, typeBinding);
        } else if (parent instanceof Assignment) {
            final ITypeBinding typeBinding= ((Assignment) parent).resolveTypeBinding();
            return replaceBooleanObjectByPrimitive(node, typeBinding);
        }
        return true;
    }

    private boolean replaceBooleanObjectByPrimitive(final QualifiedName node, final ITypeBinding typeBinding) {
        if (typeBinding != null && typeBinding.isPrimitive()) {
            if (ASTNodes.isField(node, Boolean.class.getCanonicalName(), "TRUE")) { //$NON-NLS-1$
                return replaceWithBooleanLiteral(node, true);
            } else if (ASTNodes.isField(node, Boolean.class.getCanonicalName(), "FALSE")) { //$NON-NLS-1$
                return replaceWithBooleanLiteral(node, false);
            }
        }
        return true;
    }

    private boolean replaceWithBooleanLiteral(final QualifiedName node, final boolean val) {
        final BooleanLiteral booleanLiteral= this.ctx.getASTBuilder().boolean0(val);
        this.ctx.getRefactorings().replace(node, booleanLiteral);
        return false;
    }
}
