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
import static org.autorefactor.refactoring.ASTHelper.isMethod;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.autorefactor.preferences.Preferences;
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
public class StringBuilderRatherThanStringBufferRefactoring extends AbstractRefactoringRule {
    @Override
    public String getDescription() {
        return ""
            + "Replace StringBuffer by StringBuilder when possible.";
    }

    @Override
    public String getName() {
        return "StringBuilder rather than StringBuffer";
    }

    @Override
    public boolean isEnabled(Preferences preferences) {
        return super.isEnabled(preferences) && getJavaMinorVersion() >= 5;
    }

    private int getJavaMinorVersion() {
        return ctx.getJavaProjectOptions().getJavaSERelease().getMinorVersion();
    }

    @Override
    public boolean visit(Block node) {
        final StringBufferCreationVisitor stringBufferCreationVisitor = new StringBufferCreationVisitor();
        node.accept(stringBufferCreationVisitor);
        final Iterator<ClassInstanceCreation> iterator =
                stringBufferCreationVisitor.getStringBufferCreations().iterator();

        boolean result = VISIT_SUBTREE;
        while (result == VISIT_SUBTREE && iterator.hasNext()) {
            final ClassInstanceCreation instanceCreation = iterator.next();

            try {
                final List<VariableDeclaration> varDecls = new ArrayList<VariableDeclaration>();

                if (canBeRefactored(instanceCreation, varDecls) && canVarOccurrenceBeRefactored(node, varDecls)) {
                    replaceStringBuffer(instanceCreation, varDecls);
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

    private void replaceStringBuffer(final ClassInstanceCreation instanceCreation,
            final List<VariableDeclaration> variableDecls) {
        final ASTBuilder b = ctx.getASTBuilder();

        if (variableDecls.isEmpty()) {
            final Expression[] copyOfArguments = new Expression[instanceCreation.arguments().size()];
            for (int i = 0; i < instanceCreation.arguments().size(); i++) {
                copyOfArguments[i] = b.copy((Expression) instanceCreation.arguments().get(i));
            }
            ctx.getRefactorings().replace(instanceCreation,
                    b.new0("StringBuilder",
                            copyOfArguments));
        } else {
            final Type originalType = instanceCreation.getType();
            instanceCreation.setType(b.genericType("StringBuilder"));

            for (final VariableDeclaration variableDecl : variableDecls) {
                final VariableDeclarationStatement newDeclareStmt =
                        (VariableDeclarationStatement) b.copySubtree(variableDecl.getParent());
                newDeclareStmt.setType(b.genericType("StringBuilder"));
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
                } else if (!isMethodReturningStringBuffer(mi)) {
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

    private boolean isMethodReturningStringBuffer(final MethodInvocation mi) {
        return isMethod(mi, "java.lang.StringBuffer", "append", "boolean")
                || isMethod(mi, "java.lang.StringBuffer", "append", "char")
                || isMethod(mi, "java.lang.StringBuffer", "append", "char[]")
                || isMethod(mi, "java.lang.StringBuffer", "append", "char[]", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.CharSequence")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.CharSequence", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "append", "double")
                || isMethod(mi, "java.lang.StringBuffer", "append", "float")
                || isMethod(mi, "java.lang.StringBuffer", "append", "int")
                || isMethod(mi, "java.lang.StringBuffer", "append", "long")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.Object")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.String")
                || isMethod(mi, "java.lang.StringBuffer", "append", "java.lang.StringBuffer")
                || isMethod(mi, "java.lang.StringBuffer", "appendCodePoint", "int")
                || isMethod(mi, "java.lang.StringBuffer", "delete", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "deleteCharAt", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "boolean")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "char")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "char[]")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "char[]", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.CharSequence")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.CharSequence", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "double")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "float")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "int")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "long")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.Object")
                || isMethod(mi, "java.lang.StringBuffer", "insert", "int", "java.lang.String")
                || isMethod(mi, "java.lang.StringBuffer", "replace", "int", "int", "java.lang.String")
                || isMethod(mi, "java.lang.StringBuffer", "reverse")
                || isMethod(mi, "java.lang.StringBuffer", "ensureCapacity", "int")
                || isMethod(mi, "java.lang.StringBuffer", "getChars", "int", "int", "char[]", "int");
    }

    private static final class StringBufferCreationVisitor extends ASTVisitor {

        List<ClassInstanceCreation> stringBufferCreations = new ArrayList<ClassInstanceCreation>();

        public List<ClassInstanceCreation> getStringBufferCreations() {
            return stringBufferCreations;
        }

        @Override
        public boolean visit(ClassInstanceCreation instanceCreation) {
            final ITypeBinding typeBinding = instanceCreation.getType().resolveBinding();
            if (hasType(typeBinding, "java.lang.StringBuffer")) {
                stringBufferCreations.add(instanceCreation);
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
