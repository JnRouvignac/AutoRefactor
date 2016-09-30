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
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import static org.autorefactor.refactoring.ASTHelper.*;
import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;

/*
 * TODO when the last use of resource is as arg of a method invocation,
 * it should be assumed that the given method will take care of the release.
 * TODO Track local variables. E.g., when a TypedArray a is assigned to variable b,
 * release() should be called only in one variable.
 * TODO (low priority) check whether resources are being used after release.
 * TODO add support for FragmentTransaction.beginTransaction(). It can use method
 * chaining (which means local variable might not be present) and it can be released
 * by two methods: commit() and commitAllowingStateLoss().
 */

/** See {@link #getDescription()} method. */
public class AndroidRecycleRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Android - Many resources, such as TypedArrays, VelocityTrackers, etc., should be "
                + "recycled (with a recycle()/close() call) after use.";
    }

    @Override
    public String getName() {
        return "AndroidRecycleRefactoring";
    }

    private static boolean isMethodIgnoringParameters(MethodInvocation node, String typeQualifiedName,
            String methodName) {
        if (node == null) {
            return false;
        }
        final IMethodBinding methodBinding = node.resolveMethodBinding();
        if (methodBinding == null || !methodName.equals(methodBinding.getName())) {
            return false;
        }
        final ITypeBinding declaringClazz = methodBinding.getDeclaringClass();
        return instanceOf(declaringClazz, typeQualifiedName);
    }

    private static boolean isMethodIgnoringParameters(MethodInvocation node, String typeQualifiedName,
            String... methodNames) {
        for (String methodName : methodNames) {
            if (isMethodIgnoringParameters(node, typeQualifiedName, methodName)) {
                return true;
            }
        }
        return false;
    }

    private String methodNameToCleanupResource(MethodInvocation node) {
        if (isMethodIgnoringParameters(
                node,
                "android.database.sqlite.SQLiteDatabase",
                "query", "rawQuery", "queryWithFactory", "rawQueryWithFactory")
        ) {
            return "close";
        } else if (isMethodIgnoringParameters(
                node,
                "android.content.ContentProvider",
                "query" , "rawQuery", "queryWithFactory", "rawQueryWithFactory")
        ) {
            return "close";
        } else if (isMethodIgnoringParameters(
                node,
                "android.content.ContentResolver",
                "query", "rawQuery", "queryWithFactory", "rawQueryWithFactory")
        ) {
            return "close";
        } else if (isMethodIgnoringParameters(
                node,
                "android.content.ContentProviderClient",
                "query", "rawQuery", "queryWithFactory", "rawQueryWithFactory")
        ) {
            return "close";
        } else if (isMethodIgnoringParameters(
                node,
                "android.content.Context",
                "obtainStyledAttributes")
        ) {
            return "recycle";
        } else if (isMethodIgnoringParameters(
                node,
                "android.content.res.Resources",
                "obtainTypedArray", "obtainAttributes", "obtainStyledAttributes")
        ) {
            return "recycle";
        } else if (isMethod(
                node,
                "android.view.VelocityTracker",
                "obtain")
        ) {
            return "recycle";
        } else if (isMethodIgnoringParameters(
                node,
                "android.os.Handler",
                "obtainMessage")
        ) {
            return "recycle";
        } else if (isMethodIgnoringParameters(
                node,
                "android.os.Message",
                "obtain")
        ) {
            return "recycle";
        } else if (isMethod(
                node,
                "android.view.MotionEvent",
                "obtainNoHistory", "android.view.MotionEvent")
        ) {
            return "recycle";
        } else if (isMethodIgnoringParameters(
                node,
                "android.view.MotionEvent",
                "obtain")
        ) {
            return "recycle";
        } else if (isMethod(
                node,
                "android.os.Parcel",
                "obtain")
        ) {
            return "recycle";
        } else if (isMethodIgnoringParameters(
                node,
                "android.content.ContentResolver",
                "acquireContentProviderClient")
        ) {
            return "release";
        }
        return null;
    }

    @Override
    public boolean visit(MethodInvocation node) {
        String recycleMethodName = methodNameToCleanupResource(node);
        if (recycleMethodName != null) {
            SimpleName cursorExpression;
            ASTNode variableAssignmentNode;
            VariableDeclarationFragment variableDeclarationFragment = getAncestorOrNull(node,
                    VariableDeclarationFragment.class);
            if (variableDeclarationFragment != null) {
                cursorExpression = variableDeclarationFragment.getName();
                variableAssignmentNode = variableDeclarationFragment;
            } else {
                Assignment variableAssignment = getAncestor(node, Assignment.class);
                cursorExpression = (SimpleName) variableAssignment.getLeftHandSide();
                variableAssignmentNode = variableAssignment;
            }
            // Check whether it has been closed
            ClosePresenceChecker closePresenceChecker = new ClosePresenceChecker(cursorExpression, recycleMethodName);
            VisitorDecorator visitor = new VisitorDecorator(variableAssignmentNode, cursorExpression,
                    closePresenceChecker);
            Block block = getAncestor(node, Block.class);
            block.accept(visitor);
            if (!closePresenceChecker.isClosePresent()) {
                Statement lastCursorAccess = closePresenceChecker.getLastCursorStatementInBlock(block);
                if (lastCursorAccess.getNodeType() != ASTNode.RETURN_STATEMENT) {
                    final ASTBuilder b = this.ctx.getASTBuilder();
                    final Refactorings r = this.ctx.getRefactorings();
                    MethodInvocation closeInvocation = b.invoke(b.copy(cursorExpression), recycleMethodName);
                    ExpressionStatement expressionStatement = b.toStmt(closeInvocation);
                    r.insertAfter(expressionStatement, lastCursorAccess);
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        }
        return VISIT_SUBTREE;
    }

    private class ClosePresenceChecker extends ASTVisitor {
        private boolean closePresent;
        private SimpleName lastCursorUse;
        private SimpleName cursorSimpleName;
        private String recycleMethodName;

        /**
         * @param cursorSimpleName Variable name of cursor
         * @param recycleMethodName Recycle method name
         */
        public ClosePresenceChecker(SimpleName cursorSimpleName, String recycleMethodName) {
            this.cursorSimpleName = cursorSimpleName;
            this.recycleMethodName = recycleMethodName;
        }

        public boolean isClosePresent() {
            return closePresent;
        }

        /**
         * Returns the last statement in the block where the cursor was accessed
         * before being assigned again or destroyed.
         * @param block Block with the Statement that we want to find
         * @return
         */
        public Statement getLastCursorStatementInBlock(Block block) {
            ASTNode lastCursorStatement = this.lastCursorUse.getParent();
            while (lastCursorStatement != null && !block.statements().contains(lastCursorStatement)) {
                lastCursorStatement = getAncestor(lastCursorStatement, Statement.class);
            }
            return (Statement) lastCursorStatement;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (isSameLocalVariable(cursorSimpleName, node.getExpression())) {
                if (this.recycleMethodName.equals(node.getName().getIdentifier())) {
                    this.closePresent = true;
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(SimpleName node) {
            if (isSameLocalVariable(node, cursorSimpleName)) {
                this.lastCursorUse = node;
            }
            return VISIT_SUBTREE;
        }

        @Override
        public boolean visit(Assignment node) {
            if (isSameLocalVariable(node.getLeftHandSide(), cursorSimpleName)) {
                return DO_NOT_VISIT_SUBTREE;
            }
            return VISIT_SUBTREE;
        }
    }

    /*
     * This visitor selects a partial part of the block to make the visit I.e.,
     * it will only analyze the visitor from the variable assignment until the
     * next assignment or end of the block startNode is a Assignment or
     * VariableDeclarationFragment
     */
    private class VisitorDecorator extends ASTVisitor {
        private ASTVisitor visitor;
        private SimpleName cursorSimpleName;
        private ASTVisitor specialVisitor;
        private ASTNode startNode;

        private class NopVisitor extends ASTVisitor {
        }

        VisitorDecorator(ASTNode startNode, SimpleName cursorSimpleName, ASTVisitor specialVisitor) {
            this.cursorSimpleName = cursorSimpleName;
            this.specialVisitor = specialVisitor;
            this.startNode = startNode;
            this.visitor = new NopVisitor();
        }

        @Override
        public boolean visit(Assignment node) {
            if (node.equals(startNode)) {
                visitor = specialVisitor;
            } else if (visitor != null && isSameLocalVariable(node.getLeftHandSide(), cursorSimpleName)) {
                visitor = new NopVisitor();
            }
            return visitor.visit(node);
        }

        @Override
        public boolean visit(VariableDeclarationFragment node) {
            if (node.equals(startNode)) {
                visitor = specialVisitor;
            }
            return visitor.visit(node);
        }

        @Override
        public boolean visit(SimpleName node) {
            return visitor.visit(node);
        }

        @Override
        public boolean visit(MethodInvocation node) {
            return visitor.visit(node);
        }
    }
}
