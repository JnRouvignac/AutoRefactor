/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring Rules
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

import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SimpleType;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

import java.util.Arrays;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;

/** See {@link #getDescription()} method. */
public class AndroidDrawAllocationRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Optimization for Android applications to avoid the allocation of"
                + " objects inside drawing routines. ";
    }

    @Override
    public String getName() {
        return "Android DrawAllocation";
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        if (isMethod(node, "android.widget.TextView", "onDraw", "android.graphics.Canvas")) {
            node.accept(new OnDrawTransformer(this.ctx, node));
        }
        return VISIT_SUBTREE;
    }

    static class OnDrawTransformer extends ASTVisitor {
        private RefactoringContext ctx;
        private MethodDeclaration onDrawDeclaration;

        public OnDrawTransformer(RefactoringContext ctx, MethodDeclaration onDrawDeclaration) {
            this.ctx = ctx;
            this.onDrawDeclaration = onDrawDeclaration;
        }

        public boolean isTypeBindingSubclassOf(ITypeBinding typeBinding, List<String> superClassStrings) {
            ITypeBinding superClass = typeBinding;
            while (superClass != null) {
                if (superClassStrings.contains(superClass.getErasure().getName())) {
                    return true;
                }
                superClass = superClass.getSuperclass();
            }
            return false;
        }

        @Override
        public boolean visit(VariableDeclarationFragment node) {
            Expression initializer = node.getInitializer();
            if (initializer != null) {
                if (initializer.getNodeType() == ASTNode.CAST_EXPRESSION) {
                    initializer = ((CastExpression) initializer).getExpression();
                }
                if (initializer.getNodeType() == ASTNode.CLASS_INSTANCE_CREATION) {
                    ClassInstanceCreation classInstanceCreation = (ClassInstanceCreation) initializer;
                    InitializerVisitor initializerVisitor = new InitializerVisitor();
                    classInstanceCreation.accept(initializerVisitor);
                    if (initializerVisitor.initializerCanBeExtracted) {
                        Statement declarationStatement = getAncestor(node, VariableDeclarationStatement.class);
                        if (declarationStatement != null) {
                            final ASTBuilder b = this.ctx.getASTBuilder();
                            final Refactorings r = this.ctx.getRefactorings();
                            // Deal with collections
                            if (isTypeBindingSubclassOf(node.getName().resolveTypeBinding(),
                                    Arrays.asList("AbstractCollection", "Collection", "List", "AbstractMap", "Map"))) {
                                // It should only works with allocations of
                                // empty collections.
                                // Approximation: work only for 0 args
                                if (classInstanceCreation.arguments().size() == 0) {
                                    // allocate object outside onDraw
                                    r.insertBefore(b.move(declarationStatement), onDrawDeclaration);
                                    // call collection.clear() in the end of onDraw
                                    ASTNode clearNode = b.toStmt(b.invoke(node.getName().getIdentifier(), "clear"));
                                    List<Statement> bodyStatements = statements(onDrawDeclaration.getBody());
                                    Statement lastStatement = bodyStatements.get(bodyStatements.size() - 1);
                                    if (ASTNode.RETURN_STATEMENT == lastStatement.getNodeType()) {
                                        r.insertBefore(clearNode, lastStatement);
                                    } else {
                                        r.insertAfter(clearNode, lastStatement);
                                    }
                                    return DO_NOT_VISIT_SUBTREE;
                                }
                            } else {
                                r.insertBefore(b.move(declarationStatement), onDrawDeclaration);
                                return DO_NOT_VISIT_SUBTREE;
                            }
                        }
                    }
                }
            }
            return VISIT_SUBTREE;
        }
    }

    // This visitor intends to make sure that our initializer does not use variables.
    // We assume that, if they are not present, initializer is constant.
    static class InitializerVisitor extends ASTVisitor {
        private boolean initializerCanBeExtracted = true;

        @Override
        public boolean visit(MethodInvocation node) {
            initializerCanBeExtracted = false;
            return DO_NOT_VISIT_SUBTREE;
        }

        // Skip SimpleNames inside SimpleType like the following
        // HashMap<Integer, Object>();
        //(this should keep initializerCanBeExtracted=true)
        @Override
        public boolean visit(SimpleType node) {
            return DO_NOT_VISIT_SUBTREE;
        }

        @Override
        public boolean visit(SimpleName node) {
            initializerCanBeExtracted = false;
            return DO_NOT_VISIT_SUBTREE;
        }
    }
}
