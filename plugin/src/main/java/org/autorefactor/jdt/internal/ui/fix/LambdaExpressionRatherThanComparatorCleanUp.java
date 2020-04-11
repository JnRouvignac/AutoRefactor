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
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Bindings;
import org.autorefactor.jdt.internal.corext.dom.ControlWorkflowMatcher;
import org.autorefactor.jdt.internal.corext.dom.ControlWorkflowMatcherRunnable;
import org.autorefactor.jdt.internal.corext.dom.NodeMatcher;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.jdt.internal.corext.dom.TypedInfixExpression;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NullLiteral;
import org.eclipse.jdt.core.dom.PrefixExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeMethodReference;
import org.eclipse.jdt.core.dom.VariableDeclaration;

/** See {@link #getDescription()} method. */
public class LambdaExpressionRatherThanComparatorCleanUp extends NewClassImportCleanUp {
    private static final class ObjectNotNullMatcher extends NodeMatcher<Expression> {
        private final String identifier;

        private ObjectNotNullMatcher(final String identifier) {
            this.identifier= identifier;
        }

        @Override
        public Boolean isMatching(final Expression node) {
            InfixExpression condition= ASTNodes.as(node, InfixExpression.class);

            if (condition != null
                    && ASTNodes.hasOperator(condition, InfixExpression.Operator.EQUALS, InfixExpression.Operator.NOT_EQUALS)) {
                TypedInfixExpression<SimpleName, NullLiteral> typedInfix= ASTNodes.typedInfix(condition, SimpleName.class, NullLiteral.class);

                if (typedInfix != null
                        && ASTNodes.isPassive(typedInfix.getFirstOperand())
                        && Utils.equalNotNull(typedInfix.getFirstOperand().getIdentifier(), identifier)) {
                    return ASTNodes.hasOperator(condition, InfixExpression.Operator.NOT_EQUALS);
                }
            }

            return null;
        }
    }

    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final ClassInstanceCreation node) {
            return maybeRefactorClassInstanceCreation(node, getClassesToUseWithImport());
        }

        @Override
        public boolean visit(final LambdaExpression node) {
            return maybeRefactorLambdaExpression(node, getClassesToUseWithImport());
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
    public boolean visit(final LambdaExpression node) {
        return maybeRefactorLambdaExpression(node, getAlreadyImportedClasses(node));
    }

    private boolean maybeRefactorLambdaExpression(final LambdaExpression node,
            final Set<String> classesToUseWithImport) {
        ITypeBinding targetType= ASTNodes.getTargetType(node);

        if (ASTNodes.hasType(targetType, Comparator.class.getCanonicalName())
                && targetType.getTypeArguments() != null
                && targetType.getTypeArguments().length == 1
                && node.parameters() != null
                && node.parameters().size() == 2) {
            VariableDeclaration object1= (VariableDeclaration) node.parameters().get(0);
            VariableDeclaration object2= (VariableDeclaration) node.parameters().get(1);

            if (node.getBody() instanceof Statement) {
                return maybeRefactorBody(node, targetType.getTypeArguments()[0], classesToUseWithImport, object1, object2, ASTNodes.asList((Statement) node.getBody()));
            } else if (node.getBody() instanceof Expression) {
                String identifier1= object1.getName().getIdentifier();
                String identifier2= object2.getName().getIdentifier();

                return maybeRefactorExpression(node, targetType.getTypeArguments()[0], classesToUseWithImport, identifier1, identifier2,
                        (Expression) node.getBody());
            }
        }

        return true;
    }

    @Override
    public boolean visit(final ClassInstanceCreation node) {
        return maybeRefactorClassInstanceCreation(node, getAlreadyImportedClasses(node));
    }

    private boolean maybeRefactorClassInstanceCreation(final ClassInstanceCreation node,
            final Set<String> classesToUseWithImport) {
        AnonymousClassDeclaration anonymousClassDecl= node.getAnonymousClassDeclaration();
        Type type= node.getType();

        if (type != null && type.resolveBinding() != null
                && type.resolveBinding().getTypeArguments() != null
                && type.resolveBinding().getTypeArguments().length == 1
                && ASTNodes.hasType(type.resolveBinding(), Comparator.class.getCanonicalName())
                && node.arguments().isEmpty()
                && anonymousClassDecl != null
                && anonymousClassDecl.bodyDeclarations() != null
                && anonymousClassDecl.bodyDeclarations().size() == 1) {
            @SuppressWarnings("unchecked")
            List<BodyDeclaration> bodies= anonymousClassDecl.bodyDeclarations();
            ITypeBinding typeArgument= type.resolveBinding().getTypeArguments()[0];

            if (bodies != null && bodies.size() == 1 && typeArgument != null) {
                BodyDeclaration body= bodies.get(0);

                if (body instanceof MethodDeclaration) {
                    return maybeRefactorMethod(node, typeArgument, (MethodDeclaration) body, classesToUseWithImport);
                }
            }
        }

        return true;
    }

    private boolean maybeRefactorMethod(final ClassInstanceCreation node, final ITypeBinding typeArgument,
            final MethodDeclaration methodDecl, final Set<String> classesToUseWithImport) {
        Block methodBody= methodDecl.getBody();

        if (ASTNodes.usesGivenSignature(methodDecl, Comparator.class.getCanonicalName(), "compare", typeArgument.getQualifiedName(), //$NON-NLS-1$
                typeArgument.getQualifiedName())) {
            VariableDeclaration object1= (VariableDeclaration) methodDecl.parameters().get(0);
            VariableDeclaration object2= (VariableDeclaration) methodDecl.parameters().get(1);

            @SuppressWarnings("unchecked")
            List<Statement> statements= methodBody.statements();

            return maybeRefactorBody(node, typeArgument, classesToUseWithImport, object1, object2, statements);
        }

        return true;
    }

    private boolean maybeRefactorBody(final Expression node, final ITypeBinding typeArgument,
            final Set<String> classesToUseWithImport, final VariableDeclaration object1, final VariableDeclaration object2,
            final List<Statement> statements) {
        String identifier1= object1.getName().getIdentifier();
        String identifier2= object2.getName().getIdentifier();

        if (!maybeRefactorCompareToMethod(node, typeArgument, classesToUseWithImport, statements, identifier1, identifier2)) {
            return false;
        }

        AtomicReference<Expression> criteria= new AtomicReference<>();
        AtomicBoolean isForward= new AtomicBoolean(true);

        NodeMatcher<Expression> compareToMatcher= new NodeMatcher<Expression>() {
            @Override
            public Boolean isMatching(final Expression node) {
                if (isReturnedExpressionToRefactor(node, criteria, isForward, identifier1, identifier2)) {
                    return true;
                }

                return null;
            }
        };

        NodeMatcher<Expression> zeroMatcher= new NodeMatcher<Expression>() {
            @Override
            public Boolean isMatching(final Expression node) {
                if (Utils.equalNotNull(Long.valueOf(0), ASTNodes.integerLiteral(node))) {
                    return true;
                }

                return null;
            }
        };

        NodeMatcher<Expression> positiveMatcher= new NodeMatcher<Expression>() {
            @Override
            public Boolean isMatching(final Expression node) {
                Long value= ASTNodes.integerLiteral(node);

                if (value != null && value > 0) {
                    return true;
                }

                return null;
            }
        };

        NodeMatcher<Expression> negativeMatcher= new NodeMatcher<Expression>() {
            @Override
            public Boolean isMatching(final Expression node) {
                Long value= ASTNodes.integerLiteral(node);

                if (value != null && value < 0) {
                    return true;
                }

                return null;
            }
        };

        ControlWorkflowMatcherRunnable runnableMatcher= ControlWorkflowMatcher.createControlWorkflowMatcher().addWorkflow(new ObjectNotNullMatcher(identifier1)).condition(new ObjectNotNullMatcher(identifier2)).returnedValue(compareToMatcher)
                .addWorkflow(new ObjectNotNullMatcher(identifier1).negate()).condition(new ObjectNotNullMatcher(identifier2).negate()).returnedValue(zeroMatcher)
                .addWorkflow(new ObjectNotNullMatcher(identifier1).negate()).condition(new ObjectNotNullMatcher(identifier2)).returnedValue(negativeMatcher)
                .addWorkflow(new ObjectNotNullMatcher(identifier1)).condition(new ObjectNotNullMatcher(identifier2).negate()).returnedValue(positiveMatcher);

        if (runnableMatcher.isMatching(statements)) {
            refactor(node, typeArgument, classesToUseWithImport, identifier1, criteria, isForward, Boolean.TRUE);

            return false;
        }

        runnableMatcher= ControlWorkflowMatcher.createControlWorkflowMatcher().addWorkflow(new ObjectNotNullMatcher(identifier1)).condition(new ObjectNotNullMatcher(identifier2)).returnedValue(compareToMatcher)
                .addWorkflow(new ObjectNotNullMatcher(identifier1).negate()).condition(new ObjectNotNullMatcher(identifier2).negate()).returnedValue(zeroMatcher)
                .addWorkflow(new ObjectNotNullMatcher(identifier1)).condition(new ObjectNotNullMatcher(identifier2).negate()).returnedValue(negativeMatcher)
                .addWorkflow(new ObjectNotNullMatcher(identifier1).negate()).condition(new ObjectNotNullMatcher(identifier2)).returnedValue(positiveMatcher);

        if (runnableMatcher.isMatching(statements)) {
            refactor(node, typeArgument, classesToUseWithImport, identifier1, criteria, isForward, Boolean.FALSE);

            return false;
        }

        return true;
    }

    private boolean maybeRefactorCompareToMethod(final Expression node, final ITypeBinding typeArgument,
            final Set<String> classesToUseWithImport, final List<Statement> statements,
            final String identifier1, final String identifier2) {
        if (statements != null && statements.size() == 1) {
            ReturnStatement returnStatement= ASTNodes.as(statements.get(0), ReturnStatement.class);

            if (returnStatement != null) {
                return maybeRefactorExpression(node, typeArgument, classesToUseWithImport, identifier1, identifier2,
                        returnStatement.getExpression());
            }
        }

        return true;
    }

    private boolean maybeRefactorExpression(final Expression node, final ITypeBinding typeArgument,
            final Set<String> classesToUseWithImport, final String identifier1, final String identifier2,
            final Expression expression) {
        AtomicReference<Expression> criteria= new AtomicReference<>();
        AtomicBoolean isForward= new AtomicBoolean(true);

        if (isReturnedExpressionToRefactor(expression, criteria, isForward, identifier1, identifier2)) {
            refactor(node, typeArgument, classesToUseWithImport, identifier1, criteria, isForward, null);

            return false;
        }

        return true;
    }

    private boolean isReturnedExpressionToRefactor(final Expression returnExpression, final AtomicReference<Expression> criteria,
            final AtomicBoolean isForward, final String identifier1,
            final String identifier2) {
        PrefixExpression negativeExpression= ASTNodes.as(returnExpression, PrefixExpression.class);

        if (negativeExpression != null && ASTNodes.hasOperator(negativeExpression, PrefixExpression.Operator.MINUS)) {
            isForward.set(!isForward.get());
            return isReturnedExpressionToRefactor(negativeExpression.getOperand(), criteria, isForward, identifier1, identifier2);
        }

        MethodInvocation compareToMethod= ASTNodes.as(returnExpression, MethodInvocation.class);

        if (compareToMethod != null && compareToMethod.getExpression() != null) {
            ITypeBinding comparisonType= compareToMethod.getExpression().resolveTypeBinding();

            if (comparisonType != null) {
                if (compareToMethod.getExpression() != null
                        && ASTNodes.usesGivenSignature(compareToMethod, comparisonType.getQualifiedName(), "compareTo", comparisonType.getQualifiedName())) { //$NON-NLS-1$
                    return isRefactorComparisonToRefactor(criteria, isForward, identifier1, identifier2, compareToMethod.getExpression(), (Expression) compareToMethod.arguments().get(0));
                }

                String primitiveType= Bindings.getUnboxedTypeName(comparisonType.getQualifiedName());

                if (primitiveType != null
                        && ASTNodes.usesGivenSignature(compareToMethod, comparisonType.getQualifiedName(), "compare", primitiveType, primitiveType)) { //$NON-NLS-1$
                    return isRefactorComparisonToRefactor(criteria, isForward, identifier1, identifier2, (Expression) compareToMethod.arguments().get(0), (Expression) compareToMethod.arguments().get(1));
                }
            }
        }

        return false;
    }

    private boolean isRefactorComparisonToRefactor(final AtomicReference<Expression> criteria,
            final AtomicBoolean isForward, final String identifier1, final String identifier2, final Expression expr1,
            final Expression expr2) {
        MethodInvocation method1= ASTNodes.as(expr1, MethodInvocation.class);
        MethodInvocation method2= ASTNodes.as(expr2, MethodInvocation.class);

        QualifiedName field1= ASTNodes.as(expr1, QualifiedName.class);
        QualifiedName field2= ASTNodes.as(expr2, QualifiedName.class);

        if (method1 != null && Utils.isEmpty(method1.arguments()) && method2 != null
                && Utils.isEmpty(method2.arguments())) {
            String methodName1= method1.getName().getIdentifier();
            String methodName2= method2.getName().getIdentifier();

            SimpleName objectExpr1= ASTNodes.as(method1.getExpression(), SimpleName.class);
            SimpleName objectExpr2= ASTNodes.as(method2.getExpression(), SimpleName.class);

            if (Utils.equalNotNull(methodName1, methodName2) && objectExpr1 != null && objectExpr2 != null) {
                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier1)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier2)) {
                    criteria.set(method1);
                    return true;
                }

                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier2)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier1)) {
                    criteria.set(method1);
                    isForward.set(!isForward.get());
                    return true;
                }
            }
        } else if (field1 != null && field2 != null) {
            String fieldName1= field1.getName().getIdentifier();
            String fieldName2= field2.getName().getIdentifier();

            SimpleName objectExpr1= ASTNodes.as(field1.getQualifier(), SimpleName.class);
            SimpleName objectExpr2= ASTNodes.as(field2.getQualifier(), SimpleName.class);

            if (Utils.equalNotNull(fieldName1, fieldName2) && objectExpr1 != null && objectExpr2 != null) {
                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier1)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier2)) {
                    criteria.set(field1);
                    return true;
                }

                if (Utils.equalNotNull(objectExpr1.getIdentifier(), identifier2)
                        && Utils.equalNotNull(objectExpr2.getIdentifier(), identifier1)) {
                    criteria.set(field1);
                    isForward.set(!isForward.get());
                    return true;
                }
            }
        }

        return false;
    }

    private void refactor(final Expression node, final ITypeBinding typeArgument,
            final Set<String> classesToUseWithImport, final String identifier1, final AtomicReference<Expression> criteria,
            final AtomicBoolean isForward, final Boolean isNullFirst) {
        String comparatorClassName= addImport(Comparator.class, classesToUseWithImport, new HashSet<>(0));

        Expression lambda;
        if (criteria.get() instanceof MethodInvocation) {
            lambda= buildMethod(typeArgument, (MethodInvocation) criteria.get());
        } else {
            lambda= buildField(node, typeArgument, isForward.get(), isNullFirst, (QualifiedName) criteria.get(), identifier1);
        }

        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        Expression comparingMethod= ast.invoke(ast.name(comparatorClassName), "comparing", lambda); //$NON-NLS-1$

        if (!isForward.get()) {
            comparingMethod= ast.invoke(comparingMethod, "reversed"); //$NON-NLS-1$
        }

        if (isNullFirst != null) {
            if (isNullFirst) {
                comparingMethod= ast.invoke(ast.name(comparatorClassName), "nullsFirst", comparingMethod); //$NON-NLS-1$
            } else {
                comparingMethod= ast.invoke(ast.name(comparatorClassName), "nullsLast", comparingMethod); //$NON-NLS-1$
            }
        }

        rewrite.replace(node, comparingMethod, null);
    }

    private TypeMethodReference buildMethod(final ITypeBinding type, final MethodInvocation method) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        TypeNameDecider typeNameDecider= new TypeNameDecider(method);

        TypeMethodReference typeMethodRef= ast.typeMethodRef();
        typeMethodRef.setType(ast.toType(type, typeNameDecider));
        typeMethodRef.setName(rewrite.createMoveTarget(method.getName()));
        return typeMethodRef;
    }

    @SuppressWarnings("unchecked")
    private LambdaExpression buildField(final Expression node, final ITypeBinding type, final boolean straightOrder,
            final Boolean isNullFirst, final QualifiedName field, final String identifier1) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        TypeNameDecider typeNameDecider= new TypeNameDecider(field);

        LambdaExpression lambdaExpression= ast.lambda();
        ITypeBinding destinationType= ASTNodes.getTargetType(node);

        boolean isTypeKnown= destinationType != null && ASTNodes.hasType(destinationType, Comparator.class.getCanonicalName())
                && destinationType.getTypeArguments() != null && destinationType.getTypeArguments().length == 1 && Utils.equalNotNull(destinationType.getTypeArguments()[0], type);

        if (isTypeKnown && straightOrder && isNullFirst == null) {
            lambdaExpression.parameters().add(ast.declareFragment(ast.simpleName(identifier1)));
        } else {
            lambdaExpression.parameters().add(ast.declareSingleVariable(identifier1, ast.toType(type, typeNameDecider)));
        }

        lambdaExpression.setBody(ast.fieldAccess(ast.simpleName(identifier1), rewrite.createMoveTarget(field.getName())));
        lambdaExpression.setParentheses(false);
        return lambdaExpression;
    }
}
