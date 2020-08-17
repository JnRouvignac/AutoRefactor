/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
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

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.Release;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.util.Utils;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.LambdaExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.TypeMethodReference;

/** See {@link #getDescription()} method. */
public class TreeSetRatherThanTreeMapCleanUp extends AbstractClassSubstituteCleanUp {
    private static final Map<String, String[]> CAN_BE_CASTED_TO= new HashMap<>();

    static {
        CAN_BE_CASTED_TO.put(Object.class.getCanonicalName(), new String[] { Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Cloneable.class.getCanonicalName(), new String[] { Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Serializable.class.getCanonicalName(), new String[] { Serializable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(Map.class.getCanonicalName(), new String[] { Map.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(AbstractMap.class.getCanonicalName(), new String[] { AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
        CAN_BE_CASTED_TO.put(TreeMap.class.getCanonicalName(), new String[] { TreeMap.class.getCanonicalName(), Serializable.class.getCanonicalName(), Map.class.getCanonicalName(), AbstractMap.class.getCanonicalName(), Cloneable.class.getCanonicalName(), Object.class.getCanonicalName() });
    }

    private Expression keyExpression;

    @Override
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_TreeSetRatherThanTreeMapCleanUp_name;
    }

    @Override
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_TreeSetRatherThanTreeMapCleanUp_description;
    }

    @Override
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_TreeSetRatherThanTreeMapCleanUp_reason;
    }

    @Override
    public boolean isJavaVersionSupported(final Release javaSeRelease) {
        return javaSeRelease.getMinorVersion() >= 8;
    }

    @Override
    protected String[] getExistingClassCanonicalName() {
        return new String[] { TreeMap.class.getCanonicalName() };
    }

    @Override
    public Set<String> getClassesToImport() {
        return new HashSet<>(
                Arrays.asList(TreeSet.class.getCanonicalName(), AbstractSet.class.getCanonicalName(), Set.class.getCanonicalName(), Comparator.class.getCanonicalName()));
    }

    @Override
    protected String getSubstitutingClassName(final String origRawType) {
        if (TreeMap.class.getCanonicalName().equals(origRawType)) {
            return TreeSet.class.getCanonicalName();
        }

        if (SortedMap.class.getCanonicalName().equals(origRawType)) {
            return SortedSet.class.getCanonicalName();
        }

        if (AbstractMap.class.getCanonicalName().equals(origRawType)) {
            return AbstractSet.class.getCanonicalName();
        }

        if (Map.class.getCanonicalName().equals(origRawType)) {
            return Set.class.getCanonicalName();
        }

        return null;
    }

    @Override
    protected boolean canCodeBeRefactored() {
        return keyExpression != null;
    }

    @Override
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        keyExpression= null;
        ITypeBinding[] parameterTypes= instanceCreation.resolveConstructorBinding().getParameterTypes();

        return parameterTypes.length == 0;
    }

    @Override
    protected void refactorInstantiation(final ClassInstanceCreation originalInstanceCreation,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();
        ASTNodeFactory ast= cuRewrite.getASTBuilder();

        Type substituteType= substituteType(originalInstanceCreation.getType(), originalInstanceCreation, classesToUseWithImport,
                importsToAdd);

        if (substituteType != null) {
            TypeNameDecider typeNameDecider= new TypeNameDecider(keyExpression);

            String comparatorClassName= addImport(Comparator.class, classesToUseWithImport, importsToAdd);

            Expression lambda= null;
            if (keyExpression instanceof MethodInvocation) {
                TypeMethodReference typeMethodRef= ast.typeMethodRef();

                MethodInvocation methodInvocation = (MethodInvocation) keyExpression;
                typeMethodRef.setType(ast.toType(methodInvocation.getExpression().resolveTypeBinding(), typeNameDecider));
                typeMethodRef.setName(ASTNodes.createMoveTarget(rewrite, methodInvocation.getName()));

                lambda= typeMethodRef;
            } else if (keyExpression instanceof FieldAccess) {
                FieldAccess fieldAccess= (FieldAccess) keyExpression;

                String varName= "o"; //$NON-NLS-1$
                SingleVariableDeclaration singleVariableDeclaration= ast.declareSingleVariable(varName, ast.toType(fieldAccess.getExpression().resolveTypeBinding(), typeNameDecider));

                LambdaExpression lambdaExpression= ast.lambda();

                lambdaExpression.setParentheses(true);
                @SuppressWarnings("unchecked")
                List<ASTNode> parameters= lambdaExpression.parameters();
                parameters.add(singleVariableDeclaration);
                lambdaExpression.setBody(ast.fieldAccess(ast.simpleName(varName), fieldAccess.getName()));

                lambda= lambdaExpression;
            } else if (keyExpression instanceof QualifiedName) {
                QualifiedName qualifiedName= (QualifiedName) keyExpression;

                String varName= "o"; //$NON-NLS-1$
                SingleVariableDeclaration singleVariableDeclaration= ast.declareSingleVariable(varName, ast.toType(qualifiedName.getQualifier().resolveTypeBinding(), typeNameDecider));

                LambdaExpression lambdaExpression= ast.lambda();

                lambdaExpression.setParentheses(true);
                @SuppressWarnings("unchecked")
                List<ASTNode> parameters= lambdaExpression.parameters();
                parameters.add(singleVariableDeclaration);
                lambdaExpression.setBody(ast.name(varName + "." + qualifiedName.getName().getIdentifier())); //$NON-NLS-1$

                lambda= lambdaExpression;
            }

            MethodInvocation comparingMethod= ast.newMethodInvocation(ast.name(comparatorClassName), "comparing", lambda); //$NON-NLS-1$

            ClassInstanceCreation copyOfInstanceCreation= ast.copySubtree(originalInstanceCreation);
            copyOfInstanceCreation.setType(substituteType);
            @SuppressWarnings("unchecked")
            List<Expression> arguments= copyOfInstanceCreation.arguments();
            arguments.add(comparingMethod);
            rewrite.replace(originalInstanceCreation, copyOfInstanceCreation, null);
        }
    }

    @Override
    protected Type substituteType(final Type origType, final ASTNode originalExpression, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();

        ITypeBinding origTypeBinding= origType.resolveBinding();

        if (origTypeBinding == null) {
            return null;
        }

        String substitutingType= getSubstitutingClassName(origTypeBinding.getErasure().getQualifiedName());

        if (classesToUseWithImport.contains(substitutingType)) {
            importsToAdd.add(substitutingType);
            substitutingType= getSimpleName(substitutingType);
        }

        TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpression);

        if (origTypeBinding.isParameterizedType()) {
            ITypeBinding[] origTypeArgs= origTypeBinding.getTypeArguments();
            Type[] newTypes;
            if (origTypeArgs.length > 1 && ((ParameterizedType) origType).typeArguments().size() > 1) {
                newTypes= new Type[1];
                newTypes[0]= ast.toType(origTypeArgs[1], typeNameDecider);
            } else {
                newTypes= new Type[0];
            }

            return ast.genericType(substitutingType, newTypes);
        }

        return ast.type(substitutingType);
    }

    @Override
    protected boolean canMethodBeRefactored(final MethodInvocation methodInvocation,
            final List<MethodInvocation> methodCallsToRefactor) {
        if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "clear") || ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "isEmpty") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "size") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "finalize") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notify") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "notifyAll") //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait") || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName()) //$NON-NLS-1$ //$NON-NLS-2$
                || ASTNodes.usesGivenSignature(methodInvocation, Object.class.getCanonicalName(), "wait", long.class.getSimpleName(), int.class.getSimpleName())) { //$NON-NLS-1$
            return true;
        }

        if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "containsValue", Object.class.getCanonicalName()) //$NON-NLS-1$
                || ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "values")) { //$NON-NLS-1$
            methodCallsToRefactor.add(methodInvocation);
            return true;
        }

        if (ASTNodes.usesGivenSignature(methodInvocation, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
            Expression key= (Expression) methodInvocation.arguments().get(0);
            Expression value= (Expression) methodInvocation.arguments().get(1);

            if (isKeyValid(key, value) && isReturnValueLost(methodInvocation)) {
                methodCallsToRefactor.add(methodInvocation);
                return true;
            }
        }

        // Here are the following rejected cases:
        //
        // HashMap.clone()
        // HashMap.containsKey(Object)
        // HashMap.entrySet()
        // AbstractMap.equals(Object)
        // HashMap.get(Object)
        // AbstractMap.hashCode()
        // AbstractMap.toString()
        // HashMap.keySet()
        // HashMap.putAll(Map)
        return false;
    }

    private boolean isKeyValid(final Expression key, final Expression value) {
        MethodInvocation methodInvocationKey= ASTNodes.as(key, MethodInvocation.class);

        if (methodInvocationKey != null) {
            if (methodInvocationKey.getExpression() != null
                    && ASTNodes.isPassive(methodInvocationKey.getExpression())
                    && methodInvocationKey.arguments().isEmpty()
                    && ASTNodes.match(methodInvocationKey.getExpression(), value)) {
                if (keyExpression == null) {
                    keyExpression = methodInvocationKey;
                    return true;
                } else if (methodInvocationKey.getNodeType() == keyExpression.getNodeType()
                        && Utils.equalNotNull(methodInvocationKey.resolveTypeBinding(), keyExpression.resolveTypeBinding())) {
                    return true;
                }
            }

            return false;
        }

        FieldAccess fieldAccess= ASTNodes.as(key, FieldAccess.class);

        if (fieldAccess != null
                && fieldAccess.getExpression() != null
                && ASTNodes.isPassive(fieldAccess.getExpression())
                && ASTNodes.match(fieldAccess.getExpression(), value)) {
            if (keyExpression == null) {
                keyExpression = fieldAccess;
                return true;
            } else if (fieldAccess.getNodeType() == keyExpression.getNodeType()
                    && Utils.equalNotNull(fieldAccess.resolveTypeBinding(), keyExpression.resolveTypeBinding())) {
                return true;
            }

            return false;
        }

        QualifiedName qualifiedName= ASTNodes.as(key, QualifiedName.class);

        if (qualifiedName != null
                && qualifiedName.getQualifier() != null
                && ASTNodes.isPassive(qualifiedName.getQualifier())
                && ASTNodes.match(qualifiedName.getQualifier(), value)) {
            if (keyExpression == null) {
                keyExpression = qualifiedName;
                return true;
            } else if (qualifiedName.getNodeType() == keyExpression.getNodeType()
                    && Utils.equalNotNull(qualifiedName.resolveTypeBinding(), keyExpression.resolveTypeBinding())) {
                return true;
            }
        }

        return false;
    }

    private boolean isReturnValueLost(final ASTNode node) {
        ASTNode parentNode= node.getParent();

        return parentNode instanceof ExpressionStatement
                || (parentNode instanceof ParenthesizedExpression && isReturnValueLost(parentNode));
    }

    @Override
    protected Expression getRefactoredMethod(final MethodInvocation originalMi) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        MethodInvocation refactoredMi= ast.copySubtree(originalMi);

        if (ASTNodes.usesGivenSignature(originalMi, Map.class.getCanonicalName(), "containsValue", Object.class.getCanonicalName())) { //$NON-NLS-1$
            refactoredMi.setName(ast.simpleName("contains")); //$NON-NLS-1$
        } else if (ASTNodes.usesGivenSignature(originalMi, Map.class.getCanonicalName(), "put", Object.class.getCanonicalName(), Object.class.getCanonicalName())) { //$NON-NLS-1$
            refactoredMi.setName(ast.simpleName("add")); //$NON-NLS-1$
            refactoredMi.arguments().remove(0);
        } else if (ASTNodes.usesGivenSignature(originalMi, Map.class.getCanonicalName(), "values") && !isReturnValueLost(originalMi)) { //$NON-NLS-1$
            return ast.copySubtree(originalMi.getExpression());
        }

        return refactoredMi;
    }

    @Override
    protected boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding refType) {
        return super.isTypeCompatible(variableType, refType) || ASTNodes.hasType(variableType,
                CAN_BE_CASTED_TO.getOrDefault(refType.getErasure().getQualifiedName(), new String[0]));
    }
}
