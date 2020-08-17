/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
 * Copyright (C) 2017-2018 Jean-Noël Rouvignac - fix NPE with Eclipse 4.5.2
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.jdt.internal.corext.dom.InterruptibleVisitor;
import org.autorefactor.jdt.internal.corext.dom.TypeNameDecider;
import org.autorefactor.jdt.internal.corext.dom.VarDefinitionsUsesVisitor;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParameterizedType;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/** See {@link #getDescription()} method. */
public abstract class AbstractClassSubstituteCleanUp extends NewClassImportCleanUp {
    private final class RefactoringWithObjectsClass extends CleanUpWithNewClassImport {
        @Override
        public boolean visit(final Block node) {
            return maybeRefactorBlock(node,
                    getClassesToUseWithImport(), getImportsToAdd());
        }
    }

    @Override
    public CleanUpWithNewClassImport getRefactoringClassInstance() {
        return new RefactoringWithObjectsClass();
    }

    @Override
    public Set<String> getClassesToImport() {
        return Collections.emptySet();
    }

    /**
     * Get the existing class canonical name.
     *
     * @return the existing class canonical name.
     */
    protected abstract String[] getExistingClassCanonicalName();

    /**
     * Get the substituting class name.
     *
     * @param origRawType The original raw type.
     *
     * @return the substituting class name or null if the class should be the same.
     */
    protected abstract String getSubstitutingClassName(String origRawType);

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
     * @return True if an iterator can be implicitly or explicitly invoked on the
     *         object.
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
     * @param methodInvocation The method invocation
     * @return True if the method returns the existing class.
     */
    protected boolean isMethodReturningExistingClass(final MethodInvocation methodInvocation) {
        return false;
    }

    /**
     * If the method can be refactored.
     *
     * @param methodInvocation                    The method invocation
     * @param methodCallsToRefactor The method calls to refactor
     * @return True if the method can be refactored.
     */
    protected boolean canMethodBeRefactored(final MethodInvocation methodInvocation,
            final List<MethodInvocation> methodCallsToRefactor) {
        return true;
    }

    /**
     * Refactor the instantiation.
     *
     * @param originalInstanceCreation The original instance creation
     * @param classesToUseWithImport   The classes that should be used with simple
     *                                 name.
     * @param importsToAdd             The imports that need to be added during this
     *                                 cleanup.
     */
    protected void refactorInstantiation(final ClassInstanceCreation originalInstanceCreation,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        Type substituteType= substituteType(originalInstanceCreation.getType(), originalInstanceCreation, classesToUseWithImport,
                importsToAdd);

        if (substituteType != null) {
            rewrite.replace(originalInstanceCreation.getType(), substituteType, null);
            originalInstanceCreation.setType(substituteType);
        }
    }

    /**
     * Refactor the method.
     *
     * @param methodCall   The original method invocation
     * @return The new method invocation
     */
    protected Expression getRefactoredMethod(MethodInvocation methodCall) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();
        MethodInvocation copyOfMethodCall= ast.copySubtree(methodCall);
        refactorMethod(methodCall, copyOfMethodCall);
        return copyOfMethodCall;
    }

    /**
     * Refactor the method.
     *
     * @param originalMi   The original method invocation
     * @param refactoredMi The new method invocation
     */
    protected void refactorMethod(final MethodInvocation originalMi, final MethodInvocation refactoredMi) {
    }

    /**
     * If the cleanup can be done.
     *
     * @return True if cleanup can be done.
     */
    protected boolean canCodeBeRefactored() {
        return true;
    }

    /**
     * Returns the substitute type or null if the class should be the same.
     * @param origType               The original type
     * @param originalExpression     The original expression
     * @param classesToUseWithImport The classes that should be used with simple
     *                               name.
     * @param importsToAdd           The imports that need to be added during this
     *                               cleanup.
     *
     * @return the substitute type or null if the class should be the same.
     */
    protected Type substituteType(final Type origType, final ASTNode originalExpression, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        ASTNodeFactory ast= cuRewrite.getASTBuilder();

        ITypeBinding origTypeBinding= origType.resolveBinding();

        if (origTypeBinding == null) {
            return null;
        }

        String origRawType= origTypeBinding.getErasure().getQualifiedName();
        String substitutingClassName= getSubstitutingClassName(origRawType);

        if (substitutingClassName != null) {
            if (classesToUseWithImport.contains(substitutingClassName)) {
                importsToAdd.add(substitutingClassName);
                substitutingClassName= getSimpleName(substitutingClassName);
            }

            TypeNameDecider typeNameDecider= new TypeNameDecider(originalExpression);

            if (origTypeBinding.isParameterizedType()) {
                ITypeBinding[] origTypeArgs= origTypeBinding.getTypeArguments();

                Type[] newTypes;
                if (((ParameterizedType) origType).typeArguments().isEmpty()) {
                    newTypes= new Type[0];
                } else {
                    newTypes= new Type[origTypeArgs.length];
                    for (int i= 0; i < origTypeArgs.length; i++) {
                        newTypes[i]= ast.toType(origTypeArgs[i], typeNameDecider);
                    }
                }

                return ast.genericType(substitutingClassName, newTypes);
            }

            return ast.type(substitutingClassName);
        }

        return null;
    }

    /**
     * True if the type of the variable is compatible.
     *
     * @param targetType The type of the destination.
     * @param sourceType The type of the node.
     *
     * @return true if the type of the variable is compatible.
     */
    protected boolean isTypeCompatible(final ITypeBinding targetType, final ITypeBinding sourceType) {
        return targetType != null && targetType.isAssignmentCompatible(sourceType);
    }

    @Override
    public boolean visit(final Block node) {
        return maybeRefactorBlock(node, getAlreadyImportedClasses(node), new HashSet<String>());
    }

    /**
     * Maybe refactor the block.
     *
     * @param node                   The node
     * @param classesToUseWithImport The classes to use with import
     * @param importsToAdd           The imports to add
     * @return True to visit subtree
     */
    protected boolean maybeRefactorBlock(final Block node, final Set<String> classesToUseWithImport,
            final Set<String> importsToAdd) {
        ObjectInstantiationVisitor classCreationVisitor= new ObjectInstantiationVisitor(node);
        node.accept(classCreationVisitor);

        for (ClassInstanceCreation instanceCreation : classCreationVisitor.getObjectInstantiations()) {
            List<VariableDeclaration> varDecls= new ArrayList<>();
            List<MethodInvocation> methodCallsToRefactor= new ArrayList<>();

            if (canInstantiationBeRefactored(instanceCreation) && canBeRefactored(node, instanceCreation,
                    instanceCreation.resolveTypeBinding(), varDecls, methodCallsToRefactor) && canCodeBeRefactored()) {
                replaceClass(instanceCreation, varDecls, methodCallsToRefactor, classesToUseWithImport, importsToAdd);
                return false;
            }
        }

        return true;
    }

    private boolean canBeRefactored(final Block node, final ASTNode itemToRefactor, final ITypeBinding itemTypeBinding,
            final List<VariableDeclaration> varDecls, final List<MethodInvocation> methodCallsToRefactor) {
        return canInstantiationBeRefactored(itemToRefactor, itemTypeBinding, varDecls, methodCallsToRefactor)
                && canVarOccurrenceBeRefactored(node, varDecls, methodCallsToRefactor);
    }

    private boolean canVarOccurrenceBeRefactored(final Block node, final List<VariableDeclaration> varDecls,
            final List<MethodInvocation> methodCallsToRefactor) {
        List<VariableDeclaration> otherVarDecls= new ArrayList<>();
        boolean canBeRefactored= canVarOccurrenceBeRefactored0(node, varDecls, methodCallsToRefactor,
                otherVarDecls);
        varDecls.addAll(otherVarDecls);
        return canBeRefactored;
    }

    private boolean canVarOccurrenceBeRefactored0(final Block node, final List<VariableDeclaration> varDecls,
            final List<MethodInvocation> methodCallsToRefactor, final List<VariableDeclaration> otherVarDecls) {
        for (VariableDeclaration varDecl : varDecls) {
            VarOccurrenceVisitor varOccurrenceVisitor= new VarOccurrenceVisitor(varDecl);

            Statement parent= ASTNodes.getTypedAncestor(varDecl, Statement.class);
            Statement nextSibling= ASTNodes.getNextSibling(parent);
            while (nextSibling != null) {
                varOccurrenceVisitor.visitNode(nextSibling);
                nextSibling= ASTNodes.getNextSibling(nextSibling);
            }

            if (varOccurrenceVisitor.isUsedInAnnonymousClass()) {
                return false;
            }

            for (SimpleName varOccurrence : varOccurrenceVisitor.getVarOccurrences()) {
                List<VariableDeclaration> subVarDecls= new ArrayList<>();
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
            final List<VariableDeclaration> variableDecls, final List<MethodInvocation> methodCallsToRefactor,
            final Set<String> classesToUseWithImport, final Set<String> importsToAdd) {
        ASTRewrite rewrite= cuRewrite.getASTRewrite();

        refactorInstantiation(originalInstanceCreation, classesToUseWithImport, importsToAdd);

        for (MethodInvocation methodCall : methodCallsToRefactor) {
            Expression copyOfMethodCall= getRefactoredMethod(methodCall);
            rewrite.replace(methodCall, copyOfMethodCall, null);
        }

        for (VariableDeclaration variableDecl : variableDecls) {
            VariableDeclarationStatement oldDeclareStatement= (VariableDeclarationStatement) variableDecl.getParent();
            Type substituteVarType= substituteType(oldDeclareStatement.getType(), (ASTNode) oldDeclareStatement.fragments().get(0),
                    classesToUseWithImport, importsToAdd);

            if (substituteVarType != null) {
                rewrite.replace(oldDeclareStatement.getType(), substituteVarType, null);
            }
        }
    }

    private boolean canInstantiationBeRefactored(final ASTNode node, final ITypeBinding nodeTypeBinding,
            final List<VariableDeclaration> variablesToRefactor, final List<MethodInvocation> methodCallsToRefactor) {
        ASTNode parentNode= node.getParent();

        switch (parentNode.getNodeType()) {
        case ASTNode.EXPRESSION_STATEMENT:
            return true;

        case ASTNode.ASSIGNMENT:
        case ASTNode.RETURN_STATEMENT:
        case ASTNode.CAST_EXPRESSION:
        case ASTNode.INSTANCEOF_EXPRESSION:
        case ASTNode.CLASS_INSTANCE_CREATION:
        case ASTNode.CONSTRUCTOR_INVOCATION:
        case ASTNode.CONDITIONAL_EXPRESSION:
        default:
            return false;

        case ASTNode.PARENTHESIZED_EXPRESSION:
            return canInstantiationBeRefactored(parentNode, nodeTypeBinding, variablesToRefactor,
                    methodCallsToRefactor);

        case ASTNode.ENHANCED_FOR_STATEMENT:
            return canInvokeIterator();

        case ASTNode.SINGLE_VARIABLE_DECLARATION:
        case ASTNode.VARIABLE_DECLARATION_EXPRESSION:
        case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
        case ASTNode.VARIABLE_DECLARATION_STATEMENT:
            VariableDeclaration varDecl= (VariableDeclaration) parentNode;
            if (varDecl.getParent() instanceof VariableDeclarationStatement) {
                VariableDeclarationStatement variableDeclaration= (VariableDeclarationStatement) varDecl
                        .getParent();
                if (isTypeCompatible(variableDeclaration.getType().resolveBinding(), nodeTypeBinding)) {
                    variablesToRefactor.add(varDecl);
                    return true;
                }
            }

            return false;

        case ASTNode.METHOD_INVOCATION:
            MethodInvocation methodInvocation= (MethodInvocation) parentNode;

            return node.getLocationInParent() != MethodInvocation.ARGUMENTS_PROPERTY && canMethodBeRefactored(methodInvocation, methodCallsToRefactor) && (!isMethodReturningExistingClass(methodInvocation) || canInstantiationBeRefactored(parentNode, nodeTypeBinding, variablesToRefactor,
                    methodCallsToRefactor));
        }
    }

    static String getArgumentType(final MethodInvocation methodInvocation) {
        Expression expression= methodInvocation.getExpression();
        if (expression != null) {
            ITypeBinding typeBinding= expression.resolveTypeBinding();
            if (typeBinding != null) {
                ITypeBinding[] typeArguments= typeBinding.getTypeArguments();
                if (typeArguments.length == 1) {
                    return typeArguments[0].getQualifiedName();
                }
            }
        }

        return Object.class.getCanonicalName();
    }

    private final class ObjectInstantiationVisitor extends ASTVisitor {
        private final List<ClassInstanceCreation> objectInstantiations= new ArrayList<>();

        private final Block startNode;

        /**
         * Constructor.
         *
         * @param startNode The start node block
         */
        public ObjectInstantiationVisitor(final Block startNode) {
            this.startNode= startNode;
        }

        public List<ClassInstanceCreation> getObjectInstantiations() {
            return objectInstantiations;
        }

        @Override
        public boolean visit(final Block node) {
            return startNode == node;
        }

        @Override
        public boolean visit(final AnonymousClassDeclaration node) {
            return false;
        }

        @Override
        public boolean visit(final ClassInstanceCreation instanceCreation) {
            ITypeBinding typeBinding;
            if (instanceCreation.getType() != null) {
                typeBinding= instanceCreation.getType().resolveBinding();
            } else {
                typeBinding= instanceCreation.resolveTypeBinding();
            }

            if (ASTNodes.hasType(typeBinding, getExistingClassCanonicalName())) {
                objectInstantiations.add(instanceCreation);
            }

            return true;
        }
    }

    private class VarOccurrenceVisitor extends InterruptibleVisitor {
        private final VariableDeclaration varDecl;
        private final List<SimpleName> varOccurrences= new ArrayList<>();
        private boolean isUsedInAnnonymousClass;

        public VarOccurrenceVisitor(final VariableDeclaration variable) {
            varDecl= variable;
        }

        public List<SimpleName> getVarOccurrences() {
            return varOccurrences;
        }

        public boolean isUsedInAnnonymousClass() {
            return isUsedInAnnonymousClass;
        }

        @Override
        public boolean visit(final SimpleName aVariable) {
            SimpleName varDeclName= varDecl.getName();
            if (ASTNodes.isSameLocalVariable(aVariable, varDeclName) && !aVariable.equals(varDeclName)) {
                varOccurrences.add(aVariable);
            }

            return true;
        }

        @Override
        public boolean visit(final AnonymousClassDeclaration node) {
            if (!canBeSharedInOtherThread()) {
                VarDefinitionsUsesVisitor variableUseVisitor= new VarDefinitionsUsesVisitor(
                        varDecl.resolveBinding(), node, true).find();
                if (!variableUseVisitor.getReads().isEmpty()) {
                    isUsedInAnnonymousClass= true;
                    return interruptVisit();
                }
            }

            return true;
        }
    }
}
