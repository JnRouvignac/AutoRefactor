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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.getDestinationType;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.Release;
import org.autorefactor.refactoring.TypeNameDecider;
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
public class LambdaExpressionRatherThanComparatorRefactoring extends NewClassImportRefactoring {
    private final class RefactoringWithObjectsClass extends RefactoringWithNewClassImport {

        @Override
        public boolean visit(final ClassInstanceCreation node) {
            final boolean isSubTreeToVisit = LambdaExpressionRatherThanComparatorRefactoring.this
                    .maybeRefactorClassInstanceCreation(node, getClassesToUseWithImport(), getImportsToAdd());

            return isSubTreeToVisit;
        }
    }

    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Lambda expression rather than comparator";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Replace a plain comparator instance by a lambda expression passed to a Comparator.comparing() method.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the readibility.";
    }

    @Override
    public boolean isJavaVersionSupported(Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 8;
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<String>(Arrays.asList("java.util.Comparator"));
    }

    @Override
    public RefactoringWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public boolean visit(final ClassInstanceCreation node) {
        return maybeRefactorClassInstanceCreation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorClassInstanceCreation(final ClassInstanceCreation node,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        final AnonymousClassDeclaration anonymousClassDecl = node.getAnonymousClassDeclaration();
        final Type type = node.getType();

        if (hasType(type.resolveBinding(), "java.util.Comparator") && node.arguments().isEmpty()
                && anonymousClassDecl != null
                && anonymousClassDecl.bodyDeclarations() != null && anonymousClassDecl.bodyDeclarations().size() == 1
                && type != null && type.resolveBinding() != null && type.resolveBinding().getTypeArguments() != null
                && type.resolveBinding().getTypeArguments().length == 1) {
            @SuppressWarnings("unchecked")
            final List<BodyDeclaration> bodies = anonymousClassDecl.bodyDeclarations();
            final ITypeBinding typeArgument = type.resolveBinding().getTypeArguments()[0];

            if (bodies != null && bodies.size() == 1 && typeArgument != null) {
                final BodyDeclaration body = bodies.get(0);

                if (body != null && body instanceof MethodDeclaration) {
                    return maybeRefactorMethod(node, typeArgument, body, classesToUseWithImport);
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorMethod(final ClassInstanceCreation node, final ITypeBinding typeArgument,
            final BodyDeclaration body, final Set<String> classesToUseWithImport) {
        final MethodDeclaration methodDecl = (MethodDeclaration) body;
        final Block methodBody = methodDecl.getBody();

        if (isMethod(methodDecl, "java.util.Comparator", "compare", typeArgument.getQualifiedName(),
                typeArgument.getQualifiedName())) {
            @SuppressWarnings("unchecked")
            final List<Statement> stmts = methodBody.statements();

            if (stmts != null && stmts.size() == 1) {
                final ReturnStatement returnStmt = as(stmts.get(0), ReturnStatement.class);

                if (returnStmt != null) {
                    final MethodInvocation compareToMethod = as(returnStmt.getExpression(),
                            MethodInvocation.class);

                    if (compareToMethod != null && compareToMethod.getExpression() != null) {
                        final String comparisonClass = compareToMethod.getExpression().resolveTypeBinding()
                                .getQualifiedName();

                        if (compareToMethod != null && compareToMethod.getExpression() != null
                                && compareToMethod.getExpression().resolveTypeBinding() != null && isMethod(
                                        compareToMethod, comparisonClass, "compareTo", comparisonClass)) {
                            return maybeRefactorComparison(node, methodDecl, compareToMethod, typeArgument,
                                    classesToUseWithImport);
                        }
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean maybeRefactorComparison(final ClassInstanceCreation node, final MethodDeclaration methodDecl,
            final MethodInvocation compareToMethod, final ITypeBinding typeArgument,
            final Set<String> classesToUseWithImport) {
        final SingleVariableDeclaration object1 = (SingleVariableDeclaration) methodDecl.parameters().get(0);
        final String identifier1 = object1.getName().getIdentifier();

        final SingleVariableDeclaration object2 = (SingleVariableDeclaration) methodDecl.parameters().get(1);
        final String identifier2 = object2.getName().getIdentifier();

        final Expression expr1 = compareToMethod.getExpression();
        final Expression expr2 = (Expression) compareToMethod.arguments().get(0);

        final MethodInvocation method1 = as(expr1, MethodInvocation.class);
        final MethodInvocation method2 = as(expr2, MethodInvocation.class);

        final QualifiedName field1 = as(expr1, QualifiedName.class);
        final QualifiedName field2 = as(expr2, QualifiedName.class);

        if (method1 != null && (method1.arguments() != null || method1.arguments().isEmpty()) && method2 != null
                && (method2.arguments() != null || method2.arguments().isEmpty())) {
            final String methodName1 = method1.getName().getIdentifier();
            final String methodName2 = method2.getName().getIdentifier();

            final SimpleName objectExpr1 = as(method1.getExpression(), SimpleName.class);
            final SimpleName objectExpr2 = as(method2.getExpression(), SimpleName.class);

            if (Utils.equalNotNull(methodName1, methodName2) && objectExpr1 != null && objectExpr2 != null) {
                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier1)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier2)) {
                    refactorMethod(node, typeArgument, method1, classesToUseWithImport, true);
                    return DO_NOT_VISIT_SUBTREE;
                }

                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier2)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier1)) {
                    refactorMethod(node, typeArgument, method1, classesToUseWithImport, false);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        } else if (field1 != null && field2 != null) {
            final String fieldName1 = field1.getName().getIdentifier();
            final String fieldName2 = field2.getName().getIdentifier();

            final SimpleName objectExpr1 = as(field1.getQualifier(), SimpleName.class);
            final SimpleName objectExpr2 = as(field2.getQualifier(), SimpleName.class);

            if (Utils.equalNotNull(fieldName1, fieldName2) && objectExpr1 != null && objectExpr2 != null) {
                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier1)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier2)) {
                    refactorField(node, typeArgument, field1, identifier1, classesToUseWithImport, true);
                    return DO_NOT_VISIT_SUBTREE;
                }

                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier2)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier1)) {
                    refactorField(node, typeArgument, field1, identifier1, classesToUseWithImport, false);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private void refactorMethod(final ClassInstanceCreation node, final ITypeBinding type,
            final MethodInvocation method, final Set<String> classesToUseWithImport, final boolean straightOrder) {
        final ASTBuilder b = ctx.getASTBuilder();
        final Refactorings r = ctx.getRefactorings();

        final TypeNameDecider typeNameDecider = new TypeNameDecider(method);

        final TypeMethodReference typeMethodRef = b.typeMethodRef();
        typeMethodRef.setType(b.toType(type, typeNameDecider));
        typeMethodRef.setName(b.copy(method.getName()));
        final MethodInvocation comparingMethod = b
                .invoke(classesToUseWithImport.contains("java.util.Comparator") ? b.name("Comparator")
                : b.name("java", "util", "Comparator"), "comparing", typeMethodRef);
        if (straightOrder) {
            r.replace(node, comparingMethod);
        } else {
            r.replace(node, b.invoke(comparingMethod, "reversed"));
        }
    }

    @SuppressWarnings("unchecked")
    private void refactorField(final ClassInstanceCreation node, final ITypeBinding type, final QualifiedName field,
            final String identifier1, final Set<String> classesToUseWithImport, final boolean straightOrder) {
        final ASTBuilder b = ctx.getASTBuilder();
        final Refactorings r = ctx.getRefactorings();

        final TypeNameDecider typeNameDecider = new TypeNameDecider(field);

        final LambdaExpression lambdaExpr = b.lambda();
        final ITypeBinding destinationType = getDestinationType(node);

        boolean isTypeKnown = false;
        if (destinationType != null && hasType(destinationType, "java.util.Comparator")
                && destinationType.getTypeArguments() != null && destinationType.getTypeArguments().length == 1) {
            isTypeKnown = Utils.equalNotNull(destinationType.getTypeArguments()[0], type);
        }

        if (isTypeKnown && straightOrder) {
            lambdaExpr.parameters().add(b.declareFragment(b.simpleName(identifier1)));
        } else {
            lambdaExpr.parameters().add(b.declareSingleVariable(identifier1, b.toType(type, typeNameDecider)));
        }

        lambdaExpr.setBody(b.fieldAccess(b.simpleName(identifier1), b.copy(field.getName())));
        lambdaExpr.setParentheses(false);
        final MethodInvocation comparingMethod = b
                .invoke(classesToUseWithImport.contains("java.util.Comparator") ? b.name("Comparator")
                : b.name("java", "util", "Comparator"), "comparing", lambdaExpr);
        if (straightOrder) {
            r.replace(node, comparingMethod);
        } else {
            r.replace(node, b.invoke(comparingMethod, "reversed"));
        }
    }
}
