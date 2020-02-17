/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
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

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Refactorings;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeMethodReference;

/** See {@link #getDescription()} method. */
public class LambdaExpressionRatherThanComparatorCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final ClassInstanceCreation node) {
            return LambdaExpressionRatherThanComparatorCleanUp.this
                    .maybeRefactorClassInstanceCreation(node, getClassesToUseWithImport());
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_LambdaExpressionRatherThanComparatorCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_LambdaExpressionRatherThanComparatorCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_LambdaExpressionRatherThanComparatorCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 8;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(Arrays.asList(Comparator.class.getCanonicalName()));
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public boolean visit(final ClassInstanceCreation node) {
        return maybeRefactorClassInstanceCreation(node, getAlreadyImportedClasses(node));
    }

    private boolean maybeRefactorClassInstanceCreation(final ClassInstanceCreation node,
            final Set<String> classesToUseWithImport) {
        final AnonymousClassDeclaration anonymousClassDecl= node.getAnonymousClassDeclaration();
        final Type type= node.getType();

        if (type != null && type.resolveBinding() != null
                && type.resolveBinding().getTypeArguments() != null
                && type.resolveBinding().getTypeArguments().length == 1
                && ASTNodes.hasType(type.resolveBinding(), Comparator.class.getCanonicalName())
                && node.arguments().isEmpty()
                && anonymousClassDecl != null
                && anonymousClassDecl.bodyDeclarations() != null
                && anonymousClassDecl.bodyDeclarations().size() == 1) {
            @SuppressWarnings("unchecked")
            final List<BodyDeclaration> bodies= anonymousClassDecl.bodyDeclarations();
            final ITypeBinding typeArgument= type.resolveBinding().getTypeArguments()[0];

            if (bodies != null && bodies.size() == 1 && typeArgument != null) {
                final BodyDeclaration body= bodies.get(0);

                if (body instanceof MethodDeclaration) {
                    return maybeRefactorMethod(node, typeArgument, body, classesToUseWithImport);
                }
            }
        }

        return true;
    }

    private boolean maybeRefactorMethod(final ClassInstanceCreation node, final ITypeBinding typeArgument,
            final BodyDeclaration body, final Set<String> classesToUseWithImport) {
        final MethodDeclaration methodDecl= (MethodDeclaration) body;
        final Block methodBody= methodDecl.getBody();

        if (ASTNodes.usesGivenSignature(methodDecl, Comparator.class.getCanonicalName(), "compare", typeArgument.getQualifiedName(), //$NON-NLS-1$
                typeArgument.getQualifiedName())) {
            @SuppressWarnings("unchecked")
            final List<Statement> statements= methodBody.statements();

            if (statements != null && statements.size() == 1) {
                final ReturnStatement returnStatement= ASTNodes.as(statements.get(0), ReturnStatement.class);

                if (returnStatement != null) {
                    final MethodInvocation compareToMethod= ASTNodes.as(returnStatement.getExpression(), MethodInvocation.class);

                    if (compareToMethod != null && compareToMethod.getExpression() != null) {
                        final ITypeBinding comparisonType= compareToMethod.getExpression().resolveTypeBinding();

                        if (compareToMethod != null && compareToMethod.getExpression() != null && comparisonType != null
                                && ASTNodes.usesGivenSignature(compareToMethod, comparisonType.getQualifiedName(), "compareTo", comparisonType.getQualifiedName())) { //$NON-NLS-1$
                            return maybeRefactorComparison(node, methodDecl, compareToMethod, typeArgument,
                                    classesToUseWithImport);
                        }
                    }
                }
            }
        }

        return true;
    }

    private boolean maybeRefactorComparison(final ClassInstanceCreation node, final MethodDeclaration methodDecl,
            final MethodInvocation compareToMethod, final ITypeBinding typeArgument,
            final Set<String> classesToUseWithImport) {
        final SingleVariableDeclaration object1= (SingleVariableDeclaration) methodDecl.parameters().get(0);
        final String identifier1= object1.getName().getIdentifier();

        final SingleVariableDeclaration object2= (SingleVariableDeclaration) methodDecl.parameters().get(1);
        final String identifier2= object2.getName().getIdentifier();

        final Expression expr1= compareToMethod.getExpression();
        final Expression expr2= (Expression) compareToMethod.arguments().get(0);

        final MethodInvocation method1= ASTNodes.as(expr1, MethodInvocation.class);
        final MethodInvocation method2= ASTNodes.as(expr2, MethodInvocation.class);

        final QualifiedName field1= ASTNodes.as(expr1, QualifiedName.class);
        final QualifiedName field2= ASTNodes.as(expr2, QualifiedName.class);

        if (method1 != null && (method1.arguments() == null || method1.arguments().isEmpty()) && method2 != null
                && (method2.arguments() == null || method2.arguments().isEmpty())) {
            final String methodName1= method1.getName().getIdentifier();
            final String methodName2= method2.getName().getIdentifier();

            final SimpleName objectExpr1= ASTNodes.as(method1.getExpression(), SimpleName.class);
            final SimpleName objectExpr2= ASTNodes.as(method2.getExpression(), SimpleName.class);

            if (Utils.equalNotNull(methodName1, methodName2) && objectExpr1 != null && objectExpr2 != null) {
                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier1)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier2)) {
                    refactorMethod(node, typeArgument, method1, classesToUseWithImport, true);
                    return false;
                }

                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier2)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier1)) {
                    refactorMethod(node, typeArgument, method1, classesToUseWithImport, false);
                    return false;
                }
            }
        } else if (field1 != null && field2 != null) {
            final String fieldName1= field1.getName().getIdentifier();
            final String fieldName2= field2.getName().getIdentifier();

            final SimpleName objectExpr1= ASTNodes.as(field1.getQualifier(), SimpleName.class);
            final SimpleName objectExpr2= ASTNodes.as(field2.getQualifier(), SimpleName.class);

            if (Utils.equalNotNull(fieldName1, fieldName2) && objectExpr1 != null && objectExpr2 != null) {
                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier1)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier2)) {
                    refactorField(node, typeArgument, field1, identifier1, classesToUseWithImport, true);
                    return false;
                }

                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier2)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier1)) {
                    refactorField(node, typeArgument, field1, identifier1, classesToUseWithImport, false);
                    return false;
                }
            }
        }

        return true;
    }

    private void refactorMethod(final ClassInstanceCreation node, final ITypeBinding type,
            final MethodInvocation method, final Set<String> classesToUseWithImport, final boolean straightOrder) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final Refactorings r= ctx.getRefactorings();

        final TypeNameDecider typeNameDecider= new TypeNameDecider(method);

        final TypeMethodReference typeMethodRef= b.typeMethodRef();
        typeMethodRef.setType(b.toType(type, typeNameDecider));
        typeMethodRef.setName(b.createMoveTarget(method.getName()));
        final MethodInvocation comparingMethod= b
                .invoke(b.name(classesToUseWithImport.contains(Comparator.class.getCanonicalName()) ? Comparator.class.getSimpleName() : Comparator.class.getCanonicalName()), "comparing", typeMethodRef); //$NON-NLS-1$
        if (straightOrder) {
            r.replace(node, comparingMethod);
        } else {
            r.replace(node, b.invoke(comparingMethod, "reversed")); //$NON-NLS-1$
        }
    }

    @SuppressWarnings("unchecked")
    private void refactorField(final ClassInstanceCreation node, final ITypeBinding type, final QualifiedName field,
            final String identifier1, final Set<String> classesToUseWithImport, final boolean straightOrder) {
        final ASTNodeFactory b= ctx.getASTBuilder();
        final Refactorings r= ctx.getRefactorings();

        final TypeNameDecider typeNameDecider= new TypeNameDecider(field);

        final LambdaExpression lambdaExpression= b.lambda();
        final ITypeBinding destinationType= ASTNodes.getTargetType(node);

        boolean isTypeKnown= destinationType != null && ASTNodes.hasType(destinationType, Comparator.class.getCanonicalName())
                && destinationType.getTypeArguments() != null && destinationType.getTypeArguments().length == 1 && Utils.equalNotNull(destinationType.getTypeArguments()[0], type);

        if (isTypeKnown && straightOrder) {
            lambdaExpression.parameters().add(b.declareFragment(b.simpleName(identifier1)));
        } else {
            lambdaExpression.parameters().add(b.declareSingleVariable(identifier1, b.toType(type, typeNameDecider)));
        }

        lambdaExpression.setBody(b.fieldAccess(b.simpleName(identifier1), b.createMoveTarget(field.getName())));
        lambdaExpression.setParentheses(false);
        final MethodInvocation comparingMethod= b
                .invoke(b.name(classesToUseWithImport.contains(Comparator.class.getCanonicalName()) ? Comparator.class.getSimpleName() : Comparator.class.getCanonicalName()), "comparing", lambdaExpression); //$NON-NLS-1$
        if (straightOrder) {
            r.replace(node, comparingMethod);
        } else {
            r.replace(node, b.invoke(comparingMethod, "reversed")); //$NON-NLS-1$
        }
    }
}
