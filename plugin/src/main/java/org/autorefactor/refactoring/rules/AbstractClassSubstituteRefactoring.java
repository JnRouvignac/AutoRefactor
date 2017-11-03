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

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getNextSibling;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.CAST_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.CLASS_INSTANCE_CREATION;
import static org.eclipse.jdt.core.dom.ASTNode.CONDITIONAL_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.CONSTRUCTOR_INVOCATION;
import static org.eclipse.jdt.core.dom.ASTNode.ENHANCED_FOR_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.INSTANCEOF_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.METHOD_INVOCATION;
import static org.eclipse.jdt.core.dom.ASTNode.PARENTHESIZED_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.SINGLE_VARIABLE_DECLARATION;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_STATEMENT;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.TypeNameDecider;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
/** See {@link #getDescription()} method. */
public abstract class AbstractClassSubstituteRefactoring extends AbstractRefactoringRule {

    /**
     * Get the existing class canonical name.
     *
     * @return the existing class canonical name.
     */
    protected abstract String[] getExistingClassCanonicalName();

    /**
     * Get the substituting class name.
     *
     * @return the substituting class name.
     */
    protected abstract String getSubstitutingClassName();

    /**
     * If a local variable can be used in a runnable.
     *
     * @return True if a local variable can be used in a runnable.
     */
    protected boolean canBeSharedInOtherThread() {
        return true;
    }

    /**
     * If an iterator can be implicitly or explicitly invoked on the object.
     *
     * @return True if an iterator can be implicitly or explicitly invoked on the object.
     */
    protected boolean canInvokeIterator() {
        return true;
    }

    /**
     * If the instantiation can be refactored.
     *
     * @param instanceCreation The instantiation
     * @return True if the instantiation can be refactored.
     */
    protected boolean canInstantiationBeRefactored(final ClassInstanceCreation instanceCreation) {
        return true;
    }

    /**
     * Is the method returning existing class.
     *
     * @param mi The method invocation
     * @return True if the method returns the existing class.
     */
    protected boolean isMethodReturningExistingClass(final MethodInvocation mi) {
        return false;
    }

    /**
     * If the method can be refactored.
     *
     * @param mi The method invocation
     * @param methodCallsToRefactor The method calls to refactor
     * @return True if the method can be refactored.
     */
    protected boolean canMethodBeRefactored(final MethodInvocation mi,
            final List<MethodInvocation> methodCallsToRefactor) {
        return true;
    }

    /**
     * Refactor the method.
     *
     * @param b The builder
     * @param originalMi The original method invocation
     * @param refactoredMi The new method invocation
     */
    protected void refactorMethod(final ASTBuilder b, final MethodInvocation originalMi,
            final MethodInvocation refactoredMi) {
    }

    /**
     * If the refactoring can be done.
     *
     * @return True if refactoring can be done.
     */
    protected boolean canCodeBeRefactored() {
        return true;
    }

    @Override
    public boolean visit(Block node) {
        final ObjectInstantiationVisitor classCreationVisitor = new ObjectInstantiationVisitor(node);
        node.accept(classCreationVisitor);

        for (final ClassInstanceCreation instanceCreation : classCreationVisitor.getObjectInstantiations()) {
            final List<VariableDeclaration> varDecls = new ArrayList<>();
            final List<MethodInvocation> methodCallsToRefactor = new ArrayList<>();

            if (canInstantiationBeRefactored(instanceCreation)
                    && canBeRefactored(node, instanceCreation, instanceCreation.resolveTypeBinding(), varDecls,
                            methodCallsToRefactor)
                    && canCodeBeRefactored()) {
                replaceClass(instanceCreation, varDecls, methodCallsToRefactor);
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        return VISIT_SUBTREE;
    }

    private boolean canBeRefactored(Block node, final ASTNode itemToRefactor,
            final ITypeBinding itemTypeBinding, final List<VariableDeclaration> varDecls,
            final List<MethodInvocation> methodCallsToRefactor) {
        return canInstantiationBeRefactored(itemToRefactor, itemTypeBinding, varDecls, methodCallsToRefactor)
                && canVarOccurrenceBeRefactored(node, varDecls, methodCallsToRefactor);
    }

    private boolean canVarOccurrenceBeRefactored(final Block node, final List<VariableDeclaration> varDecls,
            final List<MethodInvocation> methodCallsToRefactor) {
        final List<VariableDeclaration> otherVarDecls = new ArrayList<>();
        final boolean canBeRefactored = canVarOccurrenceBeRefactored0(node,
                                                                      varDecls,
                                                                      methodCallsToRefactor,
                                                                      otherVarDecls);
        varDecls.addAll(otherVarDecls);
        return canBeRefactored;
    }

    private boolean canVarOccurrenceBeRefactored0(final Block node, final List<VariableDeclaration> varDecls,
                                                  final List<MethodInvocation> methodCallsToRefactor,
                                                  final List<VariableDeclaration> otherVarDecls) {
        for (final VariableDeclaration varDecl : varDecls) {
            final VarOccurrenceVisitor varOccurrenceVisitor = new VarOccurrenceVisitor(varDecl);

            final Statement parent = getAncestorOrNull(varDecl, Statement.class);
            Statement nextSibling = getNextSibling(parent);
            while (nextSibling != null) {
                nextSibling.accept(varOccurrenceVisitor);
                nextSibling = getNextSibling(nextSibling);
            }

            if (varOccurrenceVisitor.isUsedInAnnonymousClass()) {
                return false;
            }

            for (final SimpleName varOccurrence : varOccurrenceVisitor.getVarOccurrences()) {
                final List<VariableDeclaration> subVarDecls = new ArrayList<>();
                if (!canBeRefactored(node, varOccurrence, varOccurrence.resolveTypeBinding(), subVarDecls,
                                     methodCallsToRefactor)) {
                    return false;
                }
                otherVarDecls.addAll(subVarDecls);
            }
        }
        return true;
    }

    private void replaceClass(final ClassInstanceCreation originalInstanceCreation,
            final List<VariableDeclaration> variableDecls,
            final List<MethodInvocation> methodCallsToRefactor) {
        final ASTBuilder b = ctx.getASTBuilder();

        final Type substituteType = substituteType(b, originalInstanceCreation.getType(),
                originalInstanceCreation);
        ctx.getRefactorings().replace(originalInstanceCreation.getType(),
                substituteType);
        originalInstanceCreation.setType(substituteType);

        for (final MethodInvocation methodCall : methodCallsToRefactor) {
            final MethodInvocation copyOfMethodCall = b.copySubtree(methodCall);
            refactorMethod(b, methodCall, copyOfMethodCall);
            ctx.getRefactorings().replace(methodCall, copyOfMethodCall);
        }

        for (final VariableDeclaration variableDecl : variableDecls) {
            final VariableDeclarationStatement oldDeclareStmt =
                    (VariableDeclarationStatement) variableDecl.getParent();
            ctx.getRefactorings().replace(oldDeclareStmt.getType(),
                    substituteType(b, oldDeclareStmt.getType(),
                            (ASTNode) oldDeclareStmt.fragments().get(0)));
        }
    }

    /**
     * Returns the substitute type.
     *
     * @param b The builder.
     * @param origType The original type
     * @param originalExpr The original expression
     * @return the substitute type.
     */
    protected Type substituteType(final ASTBuilder b, final Type origType, final ASTNode originalExpr) {
        final ITypeBinding origTypeBinding = origType.resolveBinding();
        final TypeNameDecider typeNameDecider = new TypeNameDecider(originalExpr);

        if (origTypeBinding.isParameterizedType()) {
            final ITypeBinding[] origTypeArgs = origTypeBinding.getTypeArguments();
            final Type[] newTypes = new Type[origTypeArgs.length];
            for (int i = 0; i < origTypeArgs.length; i++) {
                newTypes[i] = b.toType(origTypeArgs[i], typeNameDecider);
            }
            return b.genericType(getSubstitutingClassName(),
                    newTypes);
        }

        return b.type(getSubstitutingClassName());
    }

    private boolean canInstantiationBeRefactored(final ASTNode node,
            final ITypeBinding nodeTypeBinding,
            final List<VariableDeclaration> variablesToRefactor, final List<MethodInvocation> methodCallsToRefactor) {
        ASTNode parentNode = node.getParent();

        switch (parentNode.getNodeType()) {
        case ASSIGNMENT:
        case RETURN_STATEMENT:
        case CAST_EXPRESSION:
        case INSTANCEOF_EXPRESSION:
        case CLASS_INSTANCE_CREATION:
        case CONSTRUCTOR_INVOCATION:
        case CONDITIONAL_EXPRESSION:
            return false;

        case PARENTHESIZED_EXPRESSION:
            return canInstantiationBeRefactored(parentNode, nodeTypeBinding, variablesToRefactor,
                    methodCallsToRefactor);

        case ENHANCED_FOR_STATEMENT:
            return canInvokeIterator();

        case SINGLE_VARIABLE_DECLARATION:
        case VARIABLE_DECLARATION_EXPRESSION:
        case VARIABLE_DECLARATION_FRAGMENT:
        case VARIABLE_DECLARATION_STATEMENT:
            final VariableDeclaration varDecl = (VariableDeclaration) parentNode;
            if (varDecl.getParent() instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement variableDeclaration =
                        (VariableDeclarationStatement) varDecl.getParent();
                if (isTypeCompatible(variableDeclaration.getType().resolveBinding(), nodeTypeBinding)) {
                    variablesToRefactor.add(varDecl);
                    return true;
                }
            }
            return false;

        case METHOD_INVOCATION:
            final MethodInvocation mi = (MethodInvocation) parentNode;
            if (isObjectPassedInParameter(node, mi)
                    || !canMethodBeRefactored(mi, methodCallsToRefactor)) {
                return false;
            } else if (!isMethodReturningExistingClass(mi)) {
                return true;
            }
            return canInstantiationBeRefactored(parentNode, nodeTypeBinding, variablesToRefactor,
                    methodCallsToRefactor);

        default:
            return true;
        }
    }

    /**
     * True if the type of the variable is compatible.
     *
     * @param variableType The type of the variable.
     * @param nodeTypeBinding The type of the node.
     *
     * @return true if the type of the variable is compatible.
     */
    public boolean isTypeCompatible(final ITypeBinding variableType, final ITypeBinding nodeTypeBinding) {
        return Objects.equals(variableType, nodeTypeBinding);
    }

    private boolean isObjectPassedInParameter(final ASTNode subNode, final MethodInvocation mi) {
        return !subNode.equals(mi.getExpression());
    }

    private final class ObjectInstantiationVisitor extends ASTVisitor {

        private final List<ClassInstanceCreation> objectInstantiations = new ArrayList<>();

        private final Block startNode;

        /**
         * Constructor.
         *
         * @param startNode The start node block
         */
        public ObjectInstantiationVisitor(final Block startNode) {
            this.startNode = startNode;
        }

        public List<ClassInstanceCreation> getObjectInstantiations() {
            return objectInstantiations;
        }

        @Override
        public boolean visit(Block node) {
            return (startNode == node) ? VISIT_SUBTREE : DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(AnonymousClassDeclaration node) {
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(ClassInstanceCreation instanceCreation) {
            final ITypeBinding typeBinding;
            if (instanceCreation.getType() != null) {
                typeBinding = instanceCreation.getType().resolveBinding();
            } else {
                typeBinding = instanceCreation.resolveTypeBinding();
            }

            if (hasType(typeBinding, getExistingClassCanonicalName())) {
                objectInstantiations.add(instanceCreation);
            }
            return VISIT_SUBTREE;
        }
    }

    private class VarOccurrenceVisitor extends ASTVisitor {

        private final VariableDeclaration varDecl;
        private final List<SimpleName> varOccurrences = new ArrayList<>();
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
            if (!canBeSharedInOtherThread()) {
                final VariableDefinitionsUsesVisitor variableUseVisitor =
                        new VariableDefinitionsUsesVisitor(varDecl.resolveBinding(), node).find();
                if (!variableUseVisitor.getUses().isEmpty()) {
                    isUsedInAnnonymousClass = true;
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
            return VISIT_SUBTREE;
        }
    }
}
