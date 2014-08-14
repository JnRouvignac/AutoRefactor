/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014 Jean-Noël Rouvignac - initial API and implementation
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

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.autorefactor.refactoring.ForLoopHelper.*;

import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.ForLoopHelper.ForLoopContent;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.ClassInstanceCreation;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.EnhancedForStatement;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.IBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.Name;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

public class CollectionAddAllRefactoring extends ASTVisitor implements
        IJavaRefactoring {

    private RefactoringContext ctx;

    public CollectionAddAllRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ExpressionStatement node) {
        final MethodInvocation mi = asExpression(node, MethodInvocation.class);
        if (isMethod(mi, "java.util.Collection", "addAll", "java.util.Collection")) {
            final Expression arg0 = arguments(mi).get(0);
            final Statement previousStmt = getPreviousSibling(node);

            final Assignment as = asExpression(previousStmt, Assignment.class);
            if (as != null) {
                final Expression lhs = as.getLeftHandSide();
                if (lhs instanceof SimpleName) {
                    final SimpleName sn = (SimpleName) lhs;
                    if (isSameLocalVariable(mi.getExpression(), sn.resolveBinding())) {
                        return replaceInitializer(as.getRightHandSide(), arg0, node);
                    }
                }
            } else if (previousStmt instanceof VariableDeclarationStatement) {
                final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousStmt;
                if (vds != null && vds.fragments().size() == 1) {
                    final VariableDeclarationFragment vdf = fragments(vds).get(0);
                    if (isSameLocalVariable(mi.getExpression(), vdf.resolveBinding())) {
                        return replaceInitializer(vdf.getInitializer(), arg0, node);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceInitializer(Expression nodeToReplace,
            final Expression arg0, ExpressionStatement nodeToRemove) {
        final ClassInstanceCreation cic = as(nodeToReplace, ClassInstanceCreation.class);
        if (canReplaceInitializer(cic)) {
            final ASTBuilder b = this.ctx.getASTBuilder();
            this.ctx.getRefactorings().replace(nodeToReplace,
                    b.new0(cic.resolveTypeBinding(),
                            b.copyExpr(arg0)));
            this.ctx.getRefactorings().remove(nodeToRemove);
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private boolean canReplaceInitializer(final ClassInstanceCreation cic) {
        if (cic == null) {
            return false;
        }
        final List<Expression> args = arguments(cic);
        final boolean noArgsCtor = args.size() == 0;
        final boolean colCapacityCtor = args.size() == 1 && isPrimitive(args.get(0), "int");
        if (noArgsCtor && hasType(cic,
                "java.util.concurrent.ConcurrentLinkedDeque",
                "java.util.concurrent.ConcurrentLinkedQueue",
                "java.util.concurrent.ConcurrentSkipListSet",
                "java.util.concurrent.CopyOnWriteArrayList",
                "java.util.concurrent.CopyOnWriteArraySet",
                "java.util.concurrent.DelayQueue",
                "java.util.concurrent.LinkedBlockingDeque",
                "java.util.concurrent.LinkedBlockingQueue",
                "java.util.concurrent.LinkedTransferQueue",
                "java.util.concurrent.PriorityBlockingQueue",
                "java.util.concurrent.SynchronousQueue",
                "java.util.ArrayDeque",
                "java.util.ArrayList",
                "java.util.HashSet",
                "java.util.LinkedHashSet",
                "java.util.LinkedList",
                "java.util.PriorityQueue",
                "java.util.Stack",
                "java.util.TreeSet",
                "java.util.Vector")) {
            return true;
        }
        if (colCapacityCtor && hasType(cic,
                "java.util.concurrent.LinkedBlockingDeque",
                "java.util.concurrent.LinkedBlockingQueue",
                "java.util.concurrent.PriorityBlockingQueue",
                "java.util.ArrayDeque",
                "java.util.ArrayList",
                "java.util.HashSet",
                "java.util.LinkedHashSet",
                "java.util.PriorityQueue",
                "java.util.Vector")) {
            // TODO JNR verify capacity arguments is 0, 1 or col.length
            return true;
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(EnhancedForStatement node) {
        final Expression collectionVar = node.getExpression();
        if (instanceOf(collectionVar, "java.util.Collection")) {
            final List<Statement> stmts = asList(node.getBody());
            if (stmts.size() == 1) {
                final MethodInvocation addMI = asExpression(stmts.get(0), MethodInvocation.class);
                if (isMethod(addMI, "java.util.Collection", "add", "java.lang.Object")) {
                    final Expression arg0 = arguments(addMI).get(0);
                    final SingleVariableDeclaration foreachVariable = node.getParameter();
                    if (isSameLocalVariable(arg0, foreachVariable.resolveBinding())) {
                        return replaceWithCollectionAddAll(
                                node, addMI.getExpression(), collectionVar);
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ForStatement node) {
        final ForLoopContent loopContent = iterateOverContainer(node);
        final List<Statement> stmts = asList(node.getBody());
        if (loopContent != null && stmts.size() == 1) {
            final MethodInvocation addMI = asExpression(stmts.get(0), MethodInvocation.class);
            if (isMethod(addMI, "java.util.Collection", "add", "java.lang.Object")) {
                final Expression addArg0 = arguments(addMI).get(0);
                final MethodInvocation getMI = as(addArg0, MethodInvocation.class);
                if (isMethod(getMI, "java.util.List", "get", "int")
                        && getMI.getExpression() instanceof Name) {
                    final Expression getArg0 = arguments(getMI).get(0);
                    if (getArg0 instanceof Name
                            && isSameLocalVariable(getArg0, loopContent.loopVariable)) {
                        return replaceWithCollectionAddAll(
                                node, addMI.getExpression(), getMI.getExpression());
                    }
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private boolean replaceWithCollectionAddAll(ASTNode toReplace,
            Expression colWhereToAddAll, Expression colToAddAll) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        this.ctx.getRefactorings().replace(toReplace,
                b.toStmt(b.invoke(
                        b.copyExpr(colWhereToAddAll),
                        "addAll",
                        b.copyExpr(colToAddAll))));
        return DO_NOT_VISIT_SUBTREE;
    }

    private boolean isSameLocalVariable(Expression expr, IBinding varBinding) {
        if (expr instanceof SimpleName && varBinding instanceof IVariableBinding) {
            return ((SimpleName) expr).resolveBinding().equals(varBinding);
        }
        return false;
    }

    private boolean isSameLocalVariable(Expression expr1, Expression expr2) {
        if (expr1 instanceof SimpleName && expr2 instanceof SimpleName) {
            final IBinding binding1 = ((SimpleName) expr1).resolveBinding();
            final IBinding binding2 = ((SimpleName) expr2).resolveBinding();
            if (binding1 instanceof IVariableBinding) {
                return binding1.equals(binding2);
            }
        }
        return false;
    }

    /** {@inheritDoc} */
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }

}
