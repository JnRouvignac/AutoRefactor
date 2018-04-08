/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.getEnclosingType;
import static org.autorefactor.refactoring.ASTHelper.isEqual;
import static org.autorefactor.refactoring.ASTHelper.typeArguments;

import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.ThisExpression;

/** See {@link #getDescription()} method. */
public class RemoveUnneededThisExpressionRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove unneeded this expressions";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Remove useless use of \"this\" from method calls.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters.";
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final ThisExpression te = as(node.getExpression(), ThisExpression.class);
        if (thisExpressionRefersToEnclosingType(te)
                && isCallingMethodDeclaredInEnclosingType(node)
                && typeArguments(node).isEmpty()) {
            // remove useless thisExpressions
            this.ctx.getRefactorings().remove(node.getExpression());
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private static boolean thisExpressionRefersToEnclosingType(ThisExpression thisExpression) {
        return thisExpression != null
                && thisExpressionRefersToEnclosingType(thisExpression.getQualifier(), thisExpression);
    }

    private static boolean thisExpressionRefersToEnclosingType(Name thisQualifierName, ASTNode node) {
        if (thisQualifierName == null) {
            return true;
        }
        final ASTNode enclosingType = getEnclosingType(node);
        if (enclosingType instanceof AnonymousClassDeclaration) {
            return false;
        }
        final AbstractTypeDeclaration ancestor = (AbstractTypeDeclaration) enclosingType;
        if (thisQualifierName instanceof SimpleName) {
            return isEqual((SimpleName) thisQualifierName, ancestor.getName());
        } else if (thisQualifierName instanceof QualifiedName) {
            final QualifiedName qn = (QualifiedName) thisQualifierName;
            return isEqual(qn.getName(), ancestor.getName())
                    && thisExpressionRefersToEnclosingType(qn.getQualifier(), ancestor);
        }
        throw new NotImplementedException(thisQualifierName);
    }

    private boolean isCallingMethodDeclaredInEnclosingType(MethodInvocation node) {
        final ASTNode currentType = getEnclosingType(node);
        final IMethodBinding mb = node.resolveMethodBinding();
        if (currentType instanceof AnonymousClassDeclaration) {
            final AnonymousClassDeclaration c = (AnonymousClassDeclaration) currentType;
            final ITypeBinding enclosingTypeBinding = c.resolveBinding();
            return enclosingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
        } else if (currentType instanceof AbstractTypeDeclaration) {
            final AbstractTypeDeclaration ed = (AbstractTypeDeclaration) currentType;
            final ITypeBinding enclosingTypeBinding = ed.resolveBinding();
            return enclosingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
        }
        throw new NotImplementedException(node, node);
    }
}
