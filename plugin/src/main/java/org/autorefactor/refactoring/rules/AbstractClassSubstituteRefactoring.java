/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2017 Fabrice Tiercelin - initial API and implementation
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
import static org.autorefactor.refactoring.ASTHelper.hasType;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.AnonymousClassDeclaration;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ParenthesizedExpression;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
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

    @Override
    public boolean visit(Block node) {
        final ObjectInstantiationVisitor classCreationVisitor = new ObjectInstantiationVisitor();
        node.accept(classCreationVisitor);
        final Iterator<ClassInstanceCreation> iterator =
                classCreationVisitor.getObjectInstantiations().iterator();

        boolean result = VISIT_SUBTREE;
        while (result == VISIT_SUBTREE && iterator.hasNext()) {
            final ClassInstanceCreation instanceCreation = iterator.next();

            try {
                final List<VariableDeclaration> varDecls = new ArrayList<VariableDeclaration>();

                if (canBeRefactored(instanceCreation, varDecls) && canVarOccurrenceBeRefactored(node, varDecls)) {
                    replaceClass(instanceCreation, varDecls);
                    result = DO_NOT_VISIT_SUBTREE;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return result;
    }

    private boolean canVarOccurrenceBeRefactored(final Block node, final List<VariableDeclaration> varDecls) {
        boolean canBeRefactored = true;
        final List<VariableDeclaration> otherVarDecls = new ArrayList<VariableDeclaration>();
        final Iterator<VariableDeclaration> varIterator = varDecls.iterator();
        while (canBeRefactored && varIterator.hasNext()) {
            final VariableDeclaration varDecl = varIterator.next();
            final VarOccurrenceVisitor varOccurrenceVisitor = new VarOccurrenceVisitor(varDecl);
            node.accept(varOccurrenceVisitor);

            if (!varOccurrenceVisitor.isUsedInAnnonymousClass()) {
                Iterator<SimpleName> varOccurrenceIterator =
                        varOccurrenceVisitor.getVarOccurrences().iterator();
                while (canBeRefactored && varOccurrenceIterator.hasNext()) {
                    final List<VariableDeclaration> subVarDecls = new ArrayList<VariableDeclaration>();
                    canBeRefactored = canBeRefactored(varOccurrenceIterator.next(), subVarDecls);
                    canBeRefactored &= canVarOccurrenceBeRefactored(node, subVarDecls);

                    if (canBeRefactored) {
                        otherVarDecls.addAll(subVarDecls);
                    }
                }
            } else {
                canBeRefactored = false;
            }
        }
        varDecls.addAll(otherVarDecls);
        return canBeRefactored;
    }

    private void replaceClass(final ClassInstanceCreation instanceCreation,
            final List<VariableDeclaration> variableDecls) {
        final ASTBuilder b = ctx.getASTBuilder();

        if (variableDecls.isEmpty()) {
            final Expression[] copyOfArguments = new Expression[instanceCreation.arguments().size()];
            for (int i = 0; i < instanceCreation.arguments().size(); i++) {
                copyOfArguments[i] = b.copy((Expression) instanceCreation.arguments().get(i));
            }
            ctx.getRefactorings().replace(instanceCreation,
                    b.new0(getSubstitutingClassName(),
                            copyOfArguments));
        } else {
            final Type originalType = instanceCreation.getType();
            instanceCreation.setType(b.genericType(getSubstitutingClassName()));

            for (final VariableDeclaration variableDecl : variableDecls) {
                final VariableDeclarationStatement newDeclareStmt =
                        (VariableDeclarationStatement) b.copySubtree(variableDecl.getParent());
                newDeclareStmt.setType(b.genericType(getSubstitutingClassName()));
                ctx.getRefactorings().replace(variableDecl.getParent(), newDeclareStmt);
            }
            instanceCreation.setType(originalType);
        }
    }

    private boolean canBeRefactored(final ASTNode node, final List<VariableDeclaration> variables) {
        ASTNode subNode = node;
        ASTNode parentNode = subNode.getParent();
        boolean canBeRefactored = true;
        boolean stopVisit = false;
        do {
            if (parentNode instanceof ReturnStatement
                    || parentNode instanceof Assignment) {
                canBeRefactored = false;
                stopVisit = true;
            } else if (parentNode instanceof VariableDeclaration) {
                variables.add((VariableDeclaration) parentNode);
                stopVisit = true;
            } else if (parentNode instanceof MethodInvocation) {
                final MethodInvocation mi = (MethodInvocation) parentNode;
                if (!mi.getExpression().equals(subNode)) {
                    canBeRefactored = false;
                    stopVisit = true;
                } else if (!isMethodReturningExistingClass(mi)) {
                    stopVisit = true;
                }
            } else if (!(parentNode instanceof ParenthesizedExpression)) {
                stopVisit = true;
            }
            subNode = parentNode;
            parentNode = subNode.getParent();
        } while (!stopVisit);
        return canBeRefactored;
    }

    private final class ObjectInstantiationVisitor extends ASTVisitor {

        List<ClassInstanceCreation> objectInstantiations = new ArrayList<ClassInstanceCreation>();

        public List<ClassInstanceCreation> getObjectInstantiations() {
            return objectInstantiations;
        }

        @Override
        public boolean visit(ClassInstanceCreation instanceCreation) {
            final ITypeBinding typeBinding = instanceCreation.getType().resolveBinding();
            if (hasType(typeBinding, getExistingClassCanonicalName())) {
                objectInstantiations.add(instanceCreation);
            }
            return VISIT_SUBTREE;
        }
    }

    private static final class VarOccurrenceVisitor extends ASTVisitor {

        final VariableDeclaration varDecl;

        final List<SimpleName> varOccurrences = new ArrayList<SimpleName>();

        boolean isUsedInAnnonymousClass = false;

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
            if (varDecl.getName().getIdentifier()
                    .equals(aVariable.getIdentifier())
                    && !aVariable.equals(varDecl.getName())) {
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
