/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Andrei Paikin - Initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.fragments;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getFirstAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.removeParentheses;
import static org.autorefactor.refactoring.ASTHelper.typeArguments;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_STATEMENT;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/**
 * Abstract class for replacement other collections with enum as a type <br>
 * with specific enum implementations, e.g. HashMap -> EnumMap
 */
public abstract class AbstractEnumCollectionReplacementCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {

        @Override
        public boolean visit(ClassInstanceCreation node) {
            final boolean isSubTreeToVisit =
                    AbstractEnumCollectionReplacementCleanUp.this.maybeRefactorClassInstanceCreation(node,
                            getClassesToUseWithImport(), getImportsToAdd());

            return isSubTreeToVisit;
        }
    }

    @Override
    public RefactoringWithObjectsClass getRefactoringClassInstance() {
        final RefactoringWithObjectsClass refactoringWithNewClassImport = new RefactoringWithObjectsClass();

        return refactoringWithNewClassImport;
    }

    @Override
    public boolean visit(final ClassInstanceCreation node) {
        return maybeRefactorClassInstanceCreation(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    private boolean maybeRefactorClassInstanceCreation(final ClassInstanceCreation node,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        Type type = node.getType();

        if (isEnabled() && type.isParameterizedType() && creates(node, getImplType())) {
            ASTNode parent = getFirstAncestorOrNull(node, ReturnStatement.class, Assignment.class,
                    VariableDeclarationStatement.class);
            if (parent != null) {
                switch (parent.getNodeType()) {

                case RETURN_STATEMENT:
                    return handleReturnStatement(node, (ReturnStatement) parent, classesToUseWithImport, importsToAdd);

                case ASSIGNMENT:
                    return handleAssignment(node, (Assignment) parent, classesToUseWithImport, importsToAdd);

                case VARIABLE_DECLARATION_STATEMENT:
                    return handleVarDeclarationStatement((VariableDeclarationStatement) parent, classesToUseWithImport,
                            importsToAdd);

                // TODO: probably, it can be applied to method invocation for
                // some cases
                // [A.Paikin]
                // case ASTNode.METHOD_INVOCATION:
                // return handleMethodInvocation((MethodInvocation) parent);
                }
            }
        }

        return VISIT_SUBTREE;
    }

    abstract String getImplType();

    abstract String getInterfaceType();

    abstract boolean maybeReplace(ClassInstanceCreation node, Set<String> classesToUseWithImport,
            Set<String> importsToAdd,
            Type... types);

    private boolean handleReturnStatement(final ClassInstanceCreation node, final ReturnStatement rs,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        MethodDeclaration md = getAncestorOrNull(node, MethodDeclaration.class);

        if (md != null) {
            Type returnType = md.getReturnType2();

            if (isTargetType(returnType)) {
                List<Type> typeArguments = typeArgs(returnType);

                if (!typeArguments.isEmpty() && isEnum(typeArguments.get(0))) {
                    return maybeReplace(node, classesToUseWithImport, importsToAdd,
                            typeArguments.toArray(new Type[] {}));
                }
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean handleAssignment(final ClassInstanceCreation node, final Assignment a,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        Expression lhs = a.getLeftHandSide();

        if (isTargetType(lhs.resolveTypeBinding())) {
            ITypeBinding[] typeArguments = lhs.resolveTypeBinding().getTypeArguments();

            if (typeArguments.length > 0 && typeArguments[0].isEnum()) {
                final TypeNameDecider typeNameDecider = new TypeNameDecider(lhs);
                ASTBuilder b = ctx.getASTBuilder();
                Type[] types = new Type[typeArguments.length];

                for (int i = 0; i < types.length; i++) {
                    types[i] = b.toType(typeArguments[i], typeNameDecider);
                }

                return maybeReplace(node, classesToUseWithImport, importsToAdd, types);
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean handleVarDeclarationStatement(final VariableDeclarationStatement node,
            final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        Type type = node.getType();

        if (type.isParameterizedType() && isTargetType(type)) {
            ParameterizedType ptype = (ParameterizedType) type;
            List<Type> typeArguments = typeArguments(ptype);

            if (!typeArguments.isEmpty() && typeArguments.get(0).resolveBinding().isEnum()) {
                List<VariableDeclarationFragment> fragments = fragments(node);

                for (VariableDeclarationFragment vdf:fragments) {
                    Expression initExpr = vdf.getInitializer();

                    if (initExpr != null) {
                        initExpr = removeParentheses(initExpr);

                        if (creates(initExpr, getImplType())) {
                            return maybeReplace((ClassInstanceCreation) initExpr,
                                    classesToUseWithImport, importsToAdd,
                                    typeArguments.toArray(new Type[typeArguments.size()]));
                        }
                    }
                }
            }
        }

        return VISIT_SUBTREE;
    }

    /**
     * Just one more wrapper to extract type arguments, <br>
     * to avoid boilerplate casting and shorten method name.
     */
    List<Type> typeArgs(final Type parameterizedType) {
        return typeArguments((ParameterizedType) parameterizedType);
    }

    boolean isTargetType(final ITypeBinding it) {
        return hasType(it, getInterfaceType());
    }

    private boolean isEnum(final Type type) {
        return type.resolveBinding().isEnum();
    }

    private boolean creates(final Expression exp, final String type) {
        return exp.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION && hasType(exp.resolveTypeBinding(), type);
    }

    private boolean isTargetType(final Type type) {
        return type != null && type.isParameterizedType() && isTargetType(type.resolveBinding());
    }

    private boolean isEnabled() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion() >= 5;
    }
}
