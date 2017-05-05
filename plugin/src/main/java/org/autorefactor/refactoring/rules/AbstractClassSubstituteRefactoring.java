/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017 Jean-Noël Rouvignac - fix NPE with Eclipse 4.5.2
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
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InstanceofExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
/** See {@link #getDescription()} method. */
public abstract class AbstractClassSubstituteRefactoring extends AbstractRefactoringRule {

    /**
     * Get the existing class canonical name.
     *
     * @return the existing class canonical name.
     */
    public abstract String getExistingClassCanonicalName();

    /**
     * Get the substituting class name.
     *
     * @return the substituting class name.
     */
    public abstract String getSubstitutingClassName();

    /**
     * Is the method returning existing class.
     *
     * @param mi The method invocation
     * @return True if the method returns the existing class.
     */
    public abstract boolean isMethodReturningExistingClass(final MethodInvocation mi);

    /**
     * If the method can be refactored.
     *
     * @param mi The method invocation
     * @param methodCallsToRefactor The method calls to refactor
     * @return True if the method can be refactored.
     */
    public abstract boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor);

    /**
     * Refactor the method.
     * @param b The builder
     * @param mi The method invocation
     */
    public abstract void refactorMethod(final ASTBuilder b, final MethodInvocation mi);

    @Override
    public boolean visit(Block node) {
        final ObjectInstantiationVisitor classCreationVisitor = new ObjectInstantiationVisitor();
        node.accept(classCreationVisitor);

        for (ClassInstanceCreation instanceCreation : classCreationVisitor.getObjectInstantiations()) {
            final List<VariableDeclaration> varDecls = new ArrayList<VariableDeclaration>();
            final List<MethodInvocation> methodCallsToRefactorAlone = new ArrayList<MethodInvocation>();
            final List<MethodInvocation> methodCallsToRefactorWithVariable = new ArrayList<MethodInvocation>();

            if (canBeRefactored(node, instanceCreation, varDecls,
                    methodCallsToRefactorAlone,
                    methodCallsToRefactorWithVariable)) {
                replaceClass(instanceCreation, varDecls, methodCallsToRefactorAlone,
                        methodCallsToRefactorWithVariable);
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean canBeRefactored(Block node, final ASTNode instanceCreation,
            final List<VariableDeclaration> varDecls, final List<MethodInvocation> methodCallsToRefactorAlone,
            final List<MethodInvocation> methodCallsToRefactorWithVariable) {
        return canInstantiationBeRefactored(instanceCreation, varDecls, methodCallsToRefactorAlone,
                methodCallsToRefactorWithVariable)
                && canVarOccurrenceBeRefactored(node, varDecls, methodCallsToRefactorAlone,
                        methodCallsToRefactorWithVariable);
    }

    private boolean canVarOccurrenceBeRefactored(final Block node, final List<VariableDeclaration> varDecls,
            final List<MethodInvocation> methodCallsToRefactorAlone,
            final List<MethodInvocation> methodCallsToRefactorWithVariable) {
        final List<VariableDeclaration> otherVarDecls = new ArrayList<VariableDeclaration>();
        final boolean canBeRefactored = canVarOccurrenceBeRefactored0(node,
                                                                      varDecls,
                                                                      methodCallsToRefactorAlone,
                                                                      methodCallsToRefactorWithVariable,
                                                                      otherVarDecls);
        varDecls.addAll(otherVarDecls);
        return canBeRefactored;
    }

    private boolean canVarOccurrenceBeRefactored0(final Block node, final List<VariableDeclaration> varDecls,
                                                  final List<MethodInvocation> methodCallsToRefactorAlone,
                                                  final List<MethodInvocation> methodCallsToRefactorWithVariable,
                                                  final List<VariableDeclaration> otherVarDecls) {
        for (VariableDeclaration varDecl : varDecls) {
            final VarOccurrenceVisitor varOccurrenceVisitor = new VarOccurrenceVisitor(varDecl);
            node.accept(varOccurrenceVisitor);
            if (varOccurrenceVisitor.isUsedInAnnonymousClass()) {
                return false;
            }

            for (SimpleName varOccurrence : varOccurrenceVisitor.getVarOccurrences()) {
                final List<VariableDeclaration> subVarDecls = new ArrayList<VariableDeclaration>();
                if (!canBeRefactored(node, varOccurrence, subVarDecls,
                                     methodCallsToRefactorAlone, methodCallsToRefactorWithVariable)) {
                    return false;
                }
                otherVarDecls.addAll(subVarDecls);
            }
        }
        return true;
    }

    private void replaceClass(final ClassInstanceCreation originalInstanceCreation,
            final List<VariableDeclaration> variableDecls,
            final List<MethodInvocation> methodCallsToRefactorAlone,
            final List<MethodInvocation> methodCallsToRefactorWithVariable) {
        final ASTBuilder b = ctx.getASTBuilder();

        if (variableDecls.isEmpty()) {
            final ClassInstanceCreation newInstanceCreation = createInstanceCreation(b, originalInstanceCreation);
            ctx.getRefactorings().replace(originalInstanceCreation, newInstanceCreation);
        } else {
            replaceConstructorType(b, originalInstanceCreation);

            for (final MethodInvocation methodCall : methodCallsToRefactorWithVariable) {
                refactorMethod(b, methodCall);
            }

            for (final VariableDeclaration variableDecl : variableDecls) {
                final VariableDeclarationStatement parent = (VariableDeclarationStatement) variableDecl.getParent();
                final VariableDeclarationStatement newDeclareStmt = b.copySubtree(parent);

                replaceVariableType(b, newDeclareStmt, parent);
                ctx.getRefactorings().replace(parent, newDeclareStmt);
            }

            for (final MethodInvocation methodCall : methodCallsToRefactorAlone) {
                final MethodInvocation copyOfMethodCall = b.copySubtree(methodCall);
                refactorMethod(b, copyOfMethodCall);
                ctx.getRefactorings().replace(methodCall, copyOfMethodCall);
            }
        }
    }

    private ClassInstanceCreation createInstanceCreation(final ASTBuilder b,
            final ClassInstanceCreation originalInstanceCreation) {
        return b.new0(substituteType(b, originalInstanceCreation.getType()),
                      b.copyRange(arguments(originalInstanceCreation)));
    }

    private void replaceConstructorType(final ASTBuilder b, final ClassInstanceCreation instanceCreation) {
        instanceCreation.setType(substituteType(b, instanceCreation.getType()));
    }

    private void replaceVariableType(final ASTBuilder b, final VariableDeclarationStatement newDeclareStmt,
            final VariableDeclarationStatement oldDeclareStmt) {
        newDeclareStmt.setType(substituteType(b, oldDeclareStmt.getType()));
    }

    private Type substituteType(final ASTBuilder b, Type origType) {
        if (origType.isParameterizedType()) {
            final Type[] types = typeArgumentsToTypes(b, origType);
            return b.genericType(getSubstitutingClassName(), types);
        }
        return b.type(getSubstitutingClassName());
    }

    private Type[] typeArgumentsToTypes(final ASTBuilder b, Type type) {
        final ITypeBinding[] typeArgs = type.resolveBinding().getTypeArguments();
        final Type[] types = new Type[typeArgs.length];
        for (int i = 0; i < typeArgs.length; i++) {
            types[i] = b.type(typeArgs[i].getName());
        }
        return types;
    }

    private boolean canInstantiationBeRefactored(final ASTNode node,
            final List<VariableDeclaration> variablesToRefactor,
            final List<MethodInvocation> methodCallsToRefactorAlone,
            final List<MethodInvocation> methodCallsToRefactorWithVariable) {
        final List<MethodInvocation> localMethodCallsToRefactor = new ArrayList<MethodInvocation>();

        ASTNode childNode = node;

        do {
            ASTNode parentNode = childNode.getParent();

            if (parentNode instanceof ReturnStatement
                    || parentNode instanceof Assignment) {
                return false;
            } else if (parentNode instanceof VariableDeclaration) {
                final VariableDeclaration varDecl = (VariableDeclaration) parentNode;
                final VariableDeclarationStatement variableDeclaration =
                        (VariableDeclarationStatement) varDecl.getParent();
                if (hasType(variableDeclaration.getType().resolveBinding(), getExistingClassCanonicalName())) {
                    variablesToRefactor.add(varDecl);
                    methodCallsToRefactorWithVariable.addAll(localMethodCallsToRefactor);
                    return true;
                }
                return false;
            } else if (parentNode instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) parentNode;
                if (isObjectPassedInParameter(childNode, mi)) {
                    return false;
                } else if (canMethodBeRefactored(mi, localMethodCallsToRefactor)) {
                    if (!isMethodReturningExistingClass(mi)) {
                        methodCallsToRefactorAlone.addAll(localMethodCallsToRefactor);
                        return true;
                    }
                }
            } else if ((parentNode instanceof CastExpression)
                    || (parentNode instanceof InstanceofExpression)) {
                return false;
            } else if (!(parentNode instanceof ParenthesizedExpression)) {
                methodCallsToRefactorAlone.addAll(localMethodCallsToRefactor);
                return true;
            }

            childNode = parentNode;
        } while (true);
    }

    private boolean isObjectPassedInParameter(final ASTNode subNode, final MethodInvocation mi) {
        return !mi.getExpression().equals(subNode);
    }

    private final class ObjectInstantiationVisitor extends ASTVisitor {

        private final List<ClassInstanceCreation> objectInstantiations = new ArrayList<ClassInstanceCreation>();

        public List<ClassInstanceCreation> getObjectInstantiations() {
            return objectInstantiations;
        }

        @Override
        public boolean visit(ClassInstanceCreation instanceCreation) {
            final ITypeBinding typeBinding;
            try {
                typeBinding = instanceCreation.getType().resolveBinding();
            } catch (NullPointerException ignored) {
                // FIXME NPE occurs within Eclipse 4.5.2 (running on Java 8)
                // This NPE does not occur with Eclipse Indigo (running on Java 7)
                return VISIT_SUBTREE;
            }
            if (hasType(typeBinding, getExistingClassCanonicalName())) {
                objectInstantiations.add(instanceCreation);
            }
            return VISIT_SUBTREE;
        }
    }

    private static final class VarOccurrenceVisitor extends ASTVisitor {

        private final VariableDeclaration varDecl;
        private final List<SimpleName> varOccurrences = new ArrayList<SimpleName>();
        private boolean isUsedInAnnonymousClass;

        public VarOccurrenceVisitor(VariableDeclaration variable) {
            varDecl = variable;
        }

        public List<SimpleName> getVarOccurrences() {
            return varOccurrences;
        }

        public boolean isUsedInAnnonymousClass() {
            return isUsedInAnnonymousClass;
        }

        @Override
        public boolean visit(SimpleName aVariable) {
            final SimpleName varDeclName = varDecl.getName();
            if (aVariable.getIdentifier().equals(varDeclName.getIdentifier())
                    && !aVariable.equals(varDeclName)) {
                varOccurrences.add(aVariable);
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(AnonymousClassDeclaration node) {
            isUsedInAnnonymousClass = true;
            return DO_NOT_VISIT_SUBTREE;
        }
    }
}
