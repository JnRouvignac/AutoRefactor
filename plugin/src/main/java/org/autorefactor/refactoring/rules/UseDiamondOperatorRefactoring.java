/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2015 Jean-Noël Rouvignac - initial API and implementation
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

import java.util.List;

import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/** Change class instance creations to use the diamond operator wherever possible. */
public class UseDiamondOperatorRefactoring extends AbstractRefactoringRule {

    @Override
    public boolean visit(ClassInstanceCreation node) {
        if (this.ctx.getJavaProjectOptions().getJavaSERelease().isCompatibleWith(Release.javaSE("1.7.0"))) {
            final Type type = node.getType();
            if (type.isParameterizedType()
                    && node.getAnonymousClassDeclaration() == null
                    && canUseDiamondOperator(node)) {
                return removeAllTypeArguments((ParameterizedType) type);
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean canUseDiamondOperator(ClassInstanceCreation node) {
        final ASTNode parentInfo = getFirstParentOfType(node, ParenthesizedExpression.class);
        final StructuralPropertyDescriptor locationInParent = parentInfo.getLocationInParent();

        switch (parentInfo.getParent().getNodeType()) {
        case ASSIGNMENT:
            return Assignment.RIGHT_HAND_SIDE_PROPERTY.equals(locationInParent);
        case METHOD_INVOCATION:
            return false; // FIXME some of them can be refactored
        case RETURN_STATEMENT:
            return ReturnStatement.EXPRESSION_PROPERTY.equals(locationInParent);
        case VARIABLE_DECLARATION_FRAGMENT:
            return VariableDeclarationFragment.INITIALIZER_PROPERTY.equals(locationInParent);
        default:
            return false;
        }
    }

    private boolean removeAllTypeArguments(ParameterizedType pt) {
        final List<Type> typeArguments = typeArguments(pt);
        if (!typeArguments.isEmpty()) {
            for (Type type : typeArguments) {
                this.ctx.getRefactorings().remove(type);
            }
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}
