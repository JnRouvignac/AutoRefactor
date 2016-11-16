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
package org.autorefactor.refactoring.rules;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.autorefactor.refactoring.Release;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.StructuralPropertyDescriptor;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/** See {@link #getDescription()} method. */
public class UseDiamondOperatorRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return "Refactors class instance creations to use the diamond operator wherever possible.";
    }

    @Override
    public String getName() {
        return "Diamond operator";
    }

    private boolean isEnabled() {
        return ctx.getJavaProjectOptions().getJavaSERelease().isCompatibleWith(Release.javaSE("1.7.0"));
    }

    @Override
    public boolean visit(ClassInstanceCreation node) {
        final Type type = node.getType();
        if (isEnabled()
                && type.isParameterizedType()
                && node.getAnonymousClassDeclaration() == null
                && parentAllowsDiamondOperator(node)
                && canUseDiamondOperator(node, type)) {
            return removeAllTypeArguments((ParameterizedType) type);
        }
        return VISIT_SUBTREE;
    }

    /**
     * Tries to rebuild the process that leads to
     * {@link ClassInstanceCreation#isResolvedTypeInferredFromExpectedType()}.
     */
    private boolean canUseDiamondOperator(ClassInstanceCreation node, final Type type) {
        List<Expression> args = arguments(node);
        if (args.isEmpty()) {
            return true;
        }

        ITypeBinding typeBinding = type.resolveBinding();
        IMethodBinding ctorBinding = node.resolveConstructorBinding();
        if (typeBinding == null || ctorBinding == null) {
            return false;
        }
        List<ITypeBinding> typeArguments = new ArrayList<ITypeBinding>();
        Collections.addAll(typeArguments, typeBinding.getTypeArguments());
        ITypeBinding typeDecl = typeBinding.getTypeDeclaration();
        List<ITypeBinding> typeParameters = new ArrayList<ITypeBinding>();
        Collections.addAll(typeParameters, typeDecl.getTypeParameters());

        IMethodBinding methodDecl = ctorBinding.getMethodDeclaration();
        ITypeBinding[] actualCtorParamTypes = ctorBinding.getParameterTypes();
        ITypeBinding[] declMethodParamTypes = methodDecl.getParameterTypes();
        for (int i = 0; i < declMethodParamTypes.length; i++) {
            ITypeBinding declParamType = declMethodParamTypes[i];
            ITypeBinding actualParamType = actualCtorParamTypes[i];
            String actualParamTypeQName = actualParamType.getErasure().getQualifiedName();
            Expression actualArg = args.get(i);
            ITypeBinding actualArgType = findImplementedType(actualArg.resolveTypeBinding(), actualParamTypeQName);
            if (actualArgType != null && declParamType.isParameterizedType()) {
                ITypeBinding[] declParamTypeArgs = declParamType.getTypeArguments();
                ITypeBinding[] actualParamTypeArgs = actualParamType.getTypeArguments();
                ITypeBinding[] actualArgTypeArgs = actualArgType.getTypeArguments();
                for (int j = 0; j < declParamTypeArgs.length; j++) {
                    ITypeBinding declParamTypeArg = declParamTypeArgs[j];
                    ITypeBinding actualParamTypeArg = actualParamTypeArgs[j];
                    ITypeBinding actualArgTypeArg = actualArgTypeArgs[j];
                    if (declParamTypeArg.isWildcardType()) {
                        ITypeBinding declParamTypeArgBound = declParamTypeArg.getBound();
                        ITypeBinding actualParamTypeArgBound = actualParamTypeArg.getBound();
                        int typeParamIndex = typeParameters.indexOf(declParamTypeArgBound);
                        int typeArgIndex = typeArguments.indexOf(actualArgTypeArg);
                        if (typeParamIndex != -1 && typeArgIndex != -1) {
                            // the type parameter is matching
                            typeParameters.remove(typeParamIndex);
                            typeArguments.remove(typeArgIndex);
                        }
                    }
                }
            }
        }
        // all the type parameters are matching
        return typeParameters.isEmpty();
    }

    private boolean parentAllowsDiamondOperator(ClassInstanceCreation node) {
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
            this.ctx.getRefactorings().remove(typeArguments);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }
}
