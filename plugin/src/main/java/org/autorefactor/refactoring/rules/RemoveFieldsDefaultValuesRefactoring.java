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
package org.autorefactor.refactoring.rules;

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.EnumDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.Modifier.*;

/** See {@link #getDescription()} method. */
public class RemoveFieldsDefaultValuesRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Removes field initializers when they are the default value of the field's types.\n"
            + "For example, the initializer will be removed for integer fields initialized to \"0\".\n"
            + "Likewise, the initializer will be removed for non primitive fields initialized to \"null\".";
    }

    @Override
    public String getName() {
        return "Remove fields default values";
    }

    @Override
    public boolean visit(FieldDeclaration node) {
        if (!canRemoveFieldDefaultValue(node)) {
            return VISIT_SUBTREE;
        }
        final ITypeBinding fieldType = node.getType().resolveBinding();
        if (fieldType == null || isFinal(node.getModifiers())) {
            return VISIT_SUBTREE;
        }

        boolean visitSubtree = VISIT_SUBTREE;
        for (VariableDeclarationFragment vdf : fragments(node)) {
            final Expression initializer = vdf.getInitializer();
            if (initializer != null) {
                if (!fieldType.isPrimitive()
                        && isNullLiteral(initializer)) {
                    this.ctx.getRefactorings().remove(initializer);
                    visitSubtree = DO_NOT_VISIT_SUBTREE;
                } else if (fieldType.isPrimitive()
                        && isPrimitiveLiteral(initializer)
                        && isPrimitiveDefaultValue(initializer.resolveConstantExpressionValue())) {
                    this.ctx.getRefactorings().remove(initializer);
                    visitSubtree = DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return visitSubtree;
    }

    private boolean canRemoveFieldDefaultValue(final FieldDeclaration node) {
        // Do not remove default values from interface/annotation fields
        // because they are final by default
        final ASTNode parent = node.getParent();
        if (parent instanceof TypeDeclaration) {
            return !((TypeDeclaration) parent).isInterface();
        }
        return parent instanceof AnonymousClassDeclaration
            || parent instanceof EnumDeclaration;
    }

    private boolean isPrimitiveDefaultValue(Object val) {
        if (val instanceof Short
                || val instanceof Integer
                || val instanceof Long) {
            return ((Number) val).longValue() == 0;
        } else if (val instanceof Double
                || val instanceof Float) {
            return ((Number) val).doubleValue() == 0;
        } else if (val instanceof Boolean) {
            return Boolean.FALSE.equals(val);
        } else if (val instanceof Character) {
            return ((Character) val).charValue() == '\u0000';
        }
        return false;
    }

    private boolean isPrimitiveLiteral(Expression initializer) {
        switch (initializer.getNodeType()) {
        case ASTNode.BOOLEAN_LITERAL:
        case ASTNode.CHARACTER_LITERAL:
        case ASTNode.NUMBER_LITERAL:
            return true;

        default: // including string and null literal
            return false;
        }
    }
}
