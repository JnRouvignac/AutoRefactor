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

import org.autorefactor.preferences.Preferences;
import org.autorefactor.util.IllegalStateException;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.TypeDeclaration;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Remove useless use of "this" from method calls.
 */
public class RemoveUnneededThisExpressionRefactoring extends AbstractRefactoring {

    /** {@inheritDoc} */
    @Override
    public boolean isEnabled(Preferences preferences) {
        return preferences.removeThisForNonStaticMethodAccess();
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(MethodInvocation node) {
        final ThisExpression te = as(node.getExpression(), ThisExpression.class);
        if (thisExpressionRefersToSurroundingType(te)
                && isCallingMethodDeclaredInSurroundingType(node)) {
            // remove useless thisExpressions
            this.ctx.getRefactorings().remove(node.getExpression());
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private static boolean thisExpressionRefersToSurroundingType(ThisExpression thisExpression) {
        return thisExpression != null
                && thisExpressionRefersToSurroundingType(thisExpression.getQualifier(), thisExpression);
    }

    private static boolean thisExpressionRefersToSurroundingType(Name thisQualifierName, ASTNode node) {
        if (thisQualifierName == null) {
            return true;
        }
        final ASTNode surroundingType = getSurroundingType(node);
        if (surroundingType instanceof AnonymousClassDeclaration) {
            return false;
        }
        final TypeDeclaration ancestor = (TypeDeclaration) surroundingType;
        if (thisQualifierName instanceof SimpleName) {
            return isEqual((SimpleName) thisQualifierName, ancestor.getName());
        } else if (thisQualifierName instanceof QualifiedName) {
            final QualifiedName qn = (QualifiedName) thisQualifierName;
            return isEqual(qn.getName(), ancestor.getName())
                    && thisExpressionRefersToSurroundingType(qn.getQualifier(), ancestor);
        }
        throw new NotImplementedException(thisQualifierName);
    }

    private boolean isCallingMethodDeclaredInSurroundingType(MethodInvocation node) {
        final ASTNode currentType = getSurroundingType(node);
        final IMethodBinding mb = node.resolveMethodBinding();
        if (currentType instanceof AnonymousClassDeclaration) {
            final AnonymousClassDeclaration c = (AnonymousClassDeclaration) currentType;
            final ITypeBinding surroundingTypeBinding = c.resolveBinding();
            return surroundingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
        } else if (currentType instanceof TypeDeclaration) {
            final TypeDeclaration td = (TypeDeclaration) currentType;
            final ITypeBinding surroundingTypeBinding = td.resolveBinding();
            return surroundingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
        }
        throw new NotImplementedException(node, node);
    }

    private static ASTNode getSurroundingType(ASTNode node) {
        return getFirstAncestor(node, TypeDeclaration.class, AnonymousClassDeclaration.class);
    }

    private static ASTNode getFirstAncestor(ASTNode node, Class<?>... ancestorClasses) {
        if (node == null || node.getParent() == null) {
            throw new IllegalStateException(node,
                    "Could not find any ancestor for " + ancestorClasses + "and node " + node);
        }
        final ASTNode parent = node.getParent();
        for (Class<?> ancestorClazz : ancestorClasses) {
            if (ancestorClazz.isAssignableFrom(parent.getClass())) {
                return parent;
            }
        }
        return getFirstAncestor(parent, ancestorClasses);
    }
}
