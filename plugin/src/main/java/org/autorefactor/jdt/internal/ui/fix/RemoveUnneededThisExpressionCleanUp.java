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
package org.autorefactor.jdt.internal.ui.fix;

import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
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
public class RemoveUnneededThisExpressionCleanUp extends NoImportVisitCleanUp {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnneededThisExpressionCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnneededThisExpressionCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnneededThisExpressionCleanUp_reason;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        final ThisExpression te= ASTNodes.as(node.getExpression(), ThisExpression.class);
        if (thisExpressionRefersToEnclosingType(te) && isCallingMethodDeclaredInEnclosingType(node)
                && ASTNodes.typeArguments(node).isEmpty()) {
            // Remove useless thisExpressions
            this.ctx.getRefactorings().remove(node.getExpression());
            return false;
        }

        return true;
    }

    private static boolean thisExpressionRefersToEnclosingType(ThisExpression thisExpression) {
        return thisExpression != null
                && thisExpressionRefersToEnclosingType(thisExpression.getQualifier(), thisExpression);
    }

    private static boolean thisExpressionRefersToEnclosingType(Name thisQualifierName, ASTNode node) {
        if (thisQualifierName == null) {
            return true;
        }
        final ASTNode enclosingType= ASTNodes.getEnclosingType(node);
        if (enclosingType instanceof AnonymousClassDeclaration) {
            return false;
        }
        final AbstractTypeDeclaration ancestor= (AbstractTypeDeclaration) enclosingType;
        if (thisQualifierName instanceof SimpleName) {
            return ASTNodes.isEqual((SimpleName) thisQualifierName, ancestor.getName());
        }
        if (thisQualifierName instanceof QualifiedName) {
            final QualifiedName qn= (QualifiedName) thisQualifierName;
            return ASTNodes.isEqual(qn.getName(), ancestor.getName())
                    && thisExpressionRefersToEnclosingType(qn.getQualifier(), ancestor);
        }
        throw new NotImplementedException(thisQualifierName);
    }

    private boolean isCallingMethodDeclaredInEnclosingType(MethodInvocation node) {
        final ASTNode currentType= ASTNodes.getEnclosingType(node);
        final IMethodBinding mb= node.resolveMethodBinding();
        if (currentType instanceof AnonymousClassDeclaration) {
            final AnonymousClassDeclaration c= (AnonymousClassDeclaration) currentType;
            final ITypeBinding enclosingTypeBinding= c.resolveBinding();
            return enclosingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
        }
        if (currentType instanceof AbstractTypeDeclaration) {
            final AbstractTypeDeclaration ed= (AbstractTypeDeclaration) currentType;
            final ITypeBinding enclosingTypeBinding= ed.resolveBinding();
            return enclosingTypeBinding.isSubTypeCompatible(mb.getDeclaringClass());
        }
        throw new NotImplementedException(node, node);
    }
}
