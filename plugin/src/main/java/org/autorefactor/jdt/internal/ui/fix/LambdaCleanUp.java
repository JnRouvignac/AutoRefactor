/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2018 Fabrice Tiercelin - Initial API and implementation
 * Copyright (C) 2018 Jean-Noël Rouvignac - fix NPE
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

import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CreationReference;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionMethodReference;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.eclipse.jdt.core.dom.SuperFieldAccess;
import org.eclipse.jdt.core.dom.SuperMethodInvocation;
import org.eclipse.jdt.core.dom.SuperMethodReference;
import org.eclipse.jdt.core.dom.ThisExpression;
import org.eclipse.jdt.core.dom.TypeMethodReference;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/** See {@link #getDescription()} method. */
public class LambdaCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_LambdaCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_LambdaCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_LambdaCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 8;
    }

    @Override
    public boolean visit(final LambdaExpression node) {
        if (node.hasParentheses() && node.parameters().size() == 1
                && node.parameters().get(0) instanceof VariableDeclarationFragment) {
            // TODO it should also be possible to deal with a SingleVariableDeclaration
            // when the type matches the expected inferred type
            // To do this, we should visit the whole block and check the target type
            removeParamParentheses(node);
            return false;
        }
        if (node.getBody() instanceof Block) {
            List<Statement> statements= ASTNodes.asList((Block) node.getBody());

            if (statements.size() == 1 && statements.get(0) instanceof ReturnStatement) {
                removeReturnAndBrackets(node, statements);
                return false;
            }
        } else if (node.getBody() instanceof ClassInstanceCreation) {
            ClassInstanceCreation ci= (ClassInstanceCreation) node.getBody();
            List<Expression> arguments= ASTNodes.arguments(ci);

            if (node.parameters().size() == arguments.size() && areSameIdentifiers(node, arguments) && ci.resolveTypeBinding() != null) {
                replaceByCreationReference(node, ci);
                return false;
            }
        } else if (node.getBody() instanceof SuperMethodInvocation) {
            SuperMethodInvocation smi= (SuperMethodInvocation) node.getBody();
            List<Expression> arguments= ASTNodes.arguments(smi);

            if (node.parameters().size() == arguments.size() && areSameIdentifiers(node, arguments)) {
                replaceBySuperMethodReference(node, smi);
                return false;
            }
        } else if (node.getBody() instanceof MethodInvocation) {
            MethodInvocation mi= (MethodInvocation) node.getBody();
            Expression calledExpression= mi.getExpression();
            ITypeBinding calledType= ASTNodes.getCalledType(mi);
            List<Expression> arguments= ASTNodes.arguments(mi);

            if (node.parameters().size() == arguments.size()) {
                if (!areSameIdentifiers(node, arguments)) {
                    return true;
                }

                if (isStaticMethod(mi)) {
                    if (!arguments.isEmpty()) {
                        String[] remainingParams= new String[arguments.size() - 1];
                        for (int i= 0; i < arguments.size() - 1; i++) {
                            ITypeBinding argumentBinding= arguments.get(i + 1).resolveTypeBinding();

                            if (argumentBinding == null) {
                                return true;
                            }

                            remainingParams[i]= argumentBinding.getQualifiedName();
                        }

                        for (IMethodBinding methodBinding : calledType.getDeclaredMethods()) {
                            if ((methodBinding.getModifiers() & Modifier.STATIC) == 0 && ASTNodes.usesGivenSignature(methodBinding,
                                    calledType.getQualifiedName(), mi.getName().getIdentifier(), remainingParams)) {
                                return true;
                            }
                        }
                    }

                    replaceByTypeReference(node, mi);
                    return false;
                }

                if (calledExpression == null || calledExpression instanceof StringLiteral || calledExpression instanceof NumberLiteral
                        || calledExpression instanceof ThisExpression) {
                    replaceByMethodReference(node, mi);
                    return false;
                }
                if (calledExpression instanceof FieldAccess) {
                    FieldAccess fieldAccess= (FieldAccess) calledExpression;
                    if (fieldAccess.resolveFieldBinding().isEffectivelyFinal()) {
                        replaceByMethodReference(node, mi);
                        return false;
                    }
                } else if (calledExpression instanceof SuperFieldAccess) {
                    SuperFieldAccess fieldAccess= (SuperFieldAccess) calledExpression;
                    if (fieldAccess.resolveFieldBinding().isEffectivelyFinal()) {
                        replaceByMethodReference(node, mi);
                        return false;
                    }
                }
            } else if (calledExpression instanceof SimpleName && node.parameters().size() == arguments.size() + 1) {
                SimpleName calledObject= (SimpleName) calledExpression;
                if (isSameIdentifier(node, 0, calledObject)) {
                    for (int i= 0; i < arguments.size(); i++) {
                        ASTNode expression= ASTNodes.getUnparenthesedExpression(arguments.get(i));
                        if (!(expression instanceof SimpleName) || !isSameIdentifier(node, i + 1, (SimpleName) expression)) {
                            return true;
                        }
                    }

                    ITypeBinding clazz= calledExpression.resolveTypeBinding();

                    if (clazz == null) {
                        return true;
                    }

                    String[] remainingParams= new String[arguments.size() + 1];
                    remainingParams[0]= clazz.getQualifiedName();
                    for (int i= 0; i < arguments.size(); i++) {
                        ITypeBinding argumentBinding= arguments.get(i).resolveTypeBinding();

                        if (argumentBinding == null) {
                            return true;
                        }

                        remainingParams[i + 1]= argumentBinding.getQualifiedName();
                    }

                    for (IMethodBinding methodBinding : clazz.getDeclaredMethods()) {
                        if ((methodBinding.getModifiers() & Modifier.STATIC) > 0 && ASTNodes.usesGivenSignature(methodBinding,
                                clazz.getQualifiedName(), mi.getName().getIdentifier(), remainingParams)) {
                            return true;
                        }
                    }

                    replaceByTypeReference(node, mi);
                    return false;
                }
            }
        }

        return true;
    }

    private boolean isStaticMethod(final MethodInvocation mi) {
        Expression calledExpression= mi.getExpression();

        if (calledExpression == null) {
            return mi.resolveMethodBinding() != null && (mi.resolveMethodBinding().getModifiers() & Modifier.STATIC) != 0;
        }

        Name typeName= ASTNodes.as(calledExpression, Name.class);

        return typeName != null && typeName.resolveBinding() != null && typeName.resolveBinding().getKind() == IBinding.TYPE;
    }

    private boolean areSameIdentifiers(final LambdaExpression node, final List<Expression> arguments) {
        for (int i= 0; i < node.parameters().size(); i++) {
            Expression expression= ASTNodes.getUnparenthesedExpression(arguments.get(i));

            if (!(expression instanceof SimpleName) || !isSameIdentifier(node, i, (SimpleName) expression)) {
                return false;
            }
        }

        return true;
    }

    private boolean isSameIdentifier(final LambdaExpression node, final int i, final SimpleName argument) {
        Object param0= node.parameters().get(i);
        if (param0 instanceof VariableDeclarationFragment) {
            VariableDeclarationFragment vdf= (VariableDeclarationFragment) param0;
            return vdf.getName().getIdentifier().equals(argument.getIdentifier());
            // } else if (param0 instanceof SingleVariableDeclaration) {
            // TODO it should also be possible to deal with a SingleVariableDeclaration
            // when the type matches the expected inferred type
            // To do this, we should visit the whole block and check the target type
        }

        return false;
    }

    @SuppressWarnings("unchecked")
    private void removeParamParentheses(final LambdaExpression node) {
        ASTNodeFactory b= ctx.getASTBuilder();

        LambdaExpression copyOfLambdaExpression= b.lambda();
        ASTNode copyOfParameter= b.createMoveTarget((ASTNode) node.parameters().get(0));
        copyOfLambdaExpression.parameters().add(copyOfParameter);
        copyOfLambdaExpression.setBody(b.createMoveTarget(node.getBody()));
        copyOfLambdaExpression.setParentheses(false);
        ctx.getRefactorings().replace(node, copyOfLambdaExpression);
    }

    private void removeReturnAndBrackets(final LambdaExpression node, final List<Statement> statements) {
        ASTNodeFactory b= ctx.getASTBuilder();

        ReturnStatement returnStatement= (ReturnStatement) statements.get(0);
        ctx.getRefactorings().replace(node.getBody(), b.parenthesizeIfNeeded(b.createMoveTarget(returnStatement.getExpression())));
    }

    private void replaceByCreationReference(final LambdaExpression node, final ClassInstanceCreation ci) {
        ASTNodeFactory b= ctx.getASTBuilder();

        TypeNameDecider typeNameDecider= new TypeNameDecider(ci);

        CreationReference creationRef= b.creationRef();
        creationRef.setType(b.toType(ci.resolveTypeBinding().getErasure(), typeNameDecider));
        ctx.getRefactorings().replace(node, creationRef);
    }

    private void replaceBySuperMethodReference(final LambdaExpression node, final SuperMethodInvocation ci) {
        ASTNodeFactory b= ctx.getASTBuilder();

        SuperMethodReference creationRef= b.superMethodRef();
        creationRef.setName(b.createMoveTarget(ci.getName()));
        ctx.getRefactorings().replace(node, creationRef);
    }

    private void replaceByTypeReference(final LambdaExpression node, final MethodInvocation mi) {
        ASTNodeFactory b= ctx.getASTBuilder();

        TypeNameDecider typeNameDecider= new TypeNameDecider(mi);

        TypeMethodReference typeMethodRef= b.typeMethodRef();
        typeMethodRef.setType(b.toType(ASTNodes.getCalledType(mi).getErasure(), typeNameDecider));
        typeMethodRef.setName(b.createMoveTarget(mi.getName()));
        ctx.getRefactorings().replace(node, typeMethodRef);
    }

    private void replaceByMethodReference(final LambdaExpression node, final MethodInvocation mi) {
        ASTNodeFactory b= ctx.getASTBuilder();

        ExpressionMethodReference typeMethodRef= b.exprMethodRef();
        if (mi.getExpression() != null) {
            typeMethodRef.setExpression(b.createMoveTarget(mi.getExpression()));
        } else {
            typeMethodRef.setExpression(b.this0());
        }
        typeMethodRef.setName(b.createMoveTarget(mi.getName()));
        ctx.getRefactorings().replace(node, typeMethodRef);
    }
}
