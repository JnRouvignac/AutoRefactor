/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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
import static org.eclipse.jdt.core.dom.Modifier.ModifierKeyword.*;

import java.util.LinkedList;
import java.util.List;

import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IExtendedModifier;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Modifier.ModifierKeyword;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

public class RemoveFieldsDefaultValuesRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private RefactoringContext ctx;

    public RemoveFieldsDefaultValuesRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(FieldDeclaration node) {
        final ITypeBinding fieldType = node.getType().resolveBinding();
        if (fieldType == null || isConstant(node)) {
            return VISIT_SUBTREE;
        }
        boolean visitSubtree = VISIT_SUBTREE;
        for (VariableDeclarationFragment vdf : fragments(node)) {
            if (vdf.getInitializer() != null) {
                final Object val = vdf.getInitializer().resolveConstantExpressionValue();
                if (val == null
                        && !fieldType.isPrimitive()) {
                    this.ctx.getRefactorings().remove(vdf.getInitializer());
                    visitSubtree = DO_NOT_VISIT_SUBTREE;
                } else if (val != null
                        && fieldType.isPrimitive()
                        && isPrimitiveDefaultValue(val)) {
                    this.ctx.getRefactorings().remove(vdf.getInitializer());
                    visitSubtree = DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return visitSubtree;
    }

    private boolean isConstant(FieldDeclaration node) {
        final List<ModifierKeyword> toFind = new LinkedList<ModifierKeyword>();
        toFind.add(STATIC_KEYWORD);
        toFind.add(FINAL_KEYWORD);
        for (IExtendedModifier em : modifiers(node)) {
            if (em.isModifier()) {
                toFind.remove(((Modifier) em).getKeyword());
            }
        }
        return toFind.isEmpty();
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

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }

}
