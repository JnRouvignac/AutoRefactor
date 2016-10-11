/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
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
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;
import org.eclipse.jdt.core.dom.MethodDeclaration;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.*;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.TypeNameDecider;

/*
 * TODO when findViewById is reusing a local variable,
 * the viewholderitem will create a new field with duplicate name.
 * Possible solution: use the id names instead of var names
 * TODO sometimes convertView is a final param in getView.
 * Possible solution: remove final modifier
 */

/** See {@link #getDescription()} method. */
public class AndroidViewHolderRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return "Optimization for Android applications to optimize getView routines. "
                + "It allows reducing the calls to inflate and getViewById Android " + "API methods.";
    }

    @Override
    public String getName() {
        return "Android ViewHolder";
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        IMethodBinding methodBinding = node.resolveBinding();
        Block body = node.getBody();
        if (body != null && isMethod(methodBinding, "android.widget.Adapter", "getView", "int", "android.view.View",
                "android.view.ViewGroup")) {
            GetViewVisitor visitor = new GetViewVisitor();
            body.accept(visitor);
            if (visitor.viewVariable != null && !visitor.isInflateInsideIf()) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                final Refactorings r = this.ctx.getRefactorings();

                // Transform tree

                // Create If statement
                // test-clause
                InfixExpression condition = b.infixExpr(b.simpleName("convertView"), EQUALS, b.null0());
                // then
                Assignment assignment = b.assign(b.simpleName("convertView"), ASSIGN, b.copy(visitor.getInflateExpr()));
                Block thenBlock = b.block(b.toStmt(assignment));

                IfStatement ifStmt = b.if0(condition, thenBlock);
                r.insertBefore(ifStmt, visitor.viewAssignmentStmt);

                // assign to local view variable when necessary
                if (!"convertView".equals(visitor.viewVariable.getIdentifier())) {
                    Statement assignConvertViewToView = null;
                    if (visitor.viewVariableDeclarationFragment != null) {
                        final TypeNameDecider typeNameDecider = new TypeNameDecider(visitor.viewVariable);
                        assignConvertViewToView = b.declareStmt(b.copyType(visitor.viewVariable, typeNameDecider),
                                b.copy(visitor.viewVariable), b.simpleName("convertView"));
                    } else if (visitor.viewVariableAssignment != null) {
                        assignConvertViewToView = b
                                .toStmt(b.assign(b.copy(visitor.viewVariable), ASSIGN, b.simpleName("convertView")));
                    }
                    if (assignConvertViewToView != null) {
                        r.insertBefore(assignConvertViewToView, visitor.viewAssignmentStmt);
                    }
                }

                // make sure method returns the view to be reused
                if (visitor.returnStmt != null) {
                    r.insertAfter(b.return0(b.copy(visitor.viewVariable)), visitor.returnStmt);
                    r.remove(visitor.returnStmt);
                }

                // Optimize findViewById calls
                FindViewByIdVisitor findViewByIdVisitor = new FindViewByIdVisitor();
                body.accept(findViewByIdVisitor);
                if (findViewByIdVisitor.items.size() > 0) {
                    String viewHolderItemVarName = "viewHolderItem";
                    String viewHolderItemClassName = "ViewHolderItem";
                    // create ViewHolderItem class
                    TypeDeclaration viewHolderItemDeclaration = b.getAST().newTypeDeclaration();
                    viewHolderItemDeclaration.setName(b.simpleName(viewHolderItemClassName));
                    List<BodyDeclaration> viewItemsDeclarations = bodyDeclarations(viewHolderItemDeclaration);
                    for (FindViewByIdVisitor.FindViewByIdItem item : findViewByIdVisitor.items) {
                        VariableDeclarationFragment declarationFragment = b.getAST().newVariableDeclarationFragment();
                        SimpleName simpleName = b.simpleName(item.variable.getIdentifier());
                        declarationFragment.setName(simpleName);
                        FieldDeclaration fieldDeclaration = b.getAST().newFieldDeclaration(declarationFragment);
                        fieldDeclaration.setType(
                                b.getAST().newSimpleType(b.simpleName(item.variable.resolveTypeBinding().getName())));
                        viewItemsDeclarations.add(fieldDeclaration);
                    }
                    modifiers(viewHolderItemDeclaration).add(b.static0());
                    r.insertBefore(viewHolderItemDeclaration, node);
                    // create viewhHolderItem object
                    VariableDeclarationStatement viewHolderItemVariableDeclaration = b
                            .declareStmt(b.type(viewHolderItemClassName), b.simpleName(viewHolderItemVarName), null);
                    r.insertFirst(body, Block.STATEMENTS_PROPERTY, viewHolderItemVariableDeclaration);
                    // initialize viewHolderItem
                    Assignment viewHolderItemInitialization = b.assign(b.simpleName(viewHolderItemVarName), ASSIGN,
                            b.new0(viewHolderItemClassName));
                    statements(thenBlock).add(b.toStmt(viewHolderItemInitialization));
                    // Assign findViewById to ViewHolderItem
                    for (FindViewByIdVisitor.FindViewByIdItem item : findViewByIdVisitor.items) {
                        // ensure we are accessing convertView object
                        QualifiedName qualifiedName = b.getAST().newQualifiedName(b.name(viewHolderItemVarName),
                                b.simpleName(item.variable.getIdentifier()));
                        item.findViewByIdInvocation.setExpression(b.simpleName("convertView"));
                        Assignment itemAssignment = b.assign(qualifiedName, ASSIGN,
                                (Expression) ASTNode.copySubtree(b.getAST(), item.findViewByIdExpr));
                        statements(thenBlock).add(b.toStmt(itemAssignment));

                        // replace previous fidnviewbyid with accesses to
                        // viewHolderItem
                        r.replace(item.findViewByIdExpr, b.copy(qualifiedName));
                    }
                    // store viewHolderItem in convertView
                    MethodInvocation setTagInvocation = b.invoke("convertView", "setTag",
                            b.simpleName(viewHolderItemVarName));
                    statements(thenBlock).add(b.toStmt(setTagInvocation));

                    // retrieve viewHolderItem from convertView
                    ifStmt.setElseStatement(b.block(b.toStmt(b.assign(b.simpleName(viewHolderItemVarName), ASSIGN,
                            b.cast(b.type(viewHolderItemClassName), b.invoke("convertView", "getTag"))))));

                }
                r.remove(visitor.viewAssignmentStmt);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private class GetViewVisitor extends ASTVisitor {
        private SimpleName viewVariable;
        private Statement viewAssignmentStmt;
        private VariableDeclarationFragment viewVariableDeclarationFragment;
        private Assignment viewVariableAssignment;
        private ReturnStatement returnStmt;

        private void resetData() {
            viewVariable = null;
            viewAssignmentStmt = null;
            viewVariableDeclarationFragment = null;
            viewVariableAssignment = null;
            returnStmt = null;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (isInflateMethod(node)) {
                ASTNode ancestor = getFirstAncestorOrNull(node, VariableDeclarationFragment.class, Assignment.class);
                if (ancestor instanceof VariableDeclarationFragment) {
                    viewVariableDeclarationFragment = (VariableDeclarationFragment) ancestor;
                    viewVariable = viewVariableDeclarationFragment.getName();
                    viewAssignmentStmt = getAncestorOrNull(viewVariableDeclarationFragment,
                            VariableDeclarationStatement.class);
                    if (viewAssignmentStmt == null) {
                        resetData();
                        return VISIT_SUBTREE;
                    }
                } else if (ancestor instanceof Assignment) {
                    viewVariableAssignment = (Assignment) ancestor;
                    ASTNode leftHandSideNode = viewVariableAssignment.getLeftHandSide();
                    if (leftHandSideNode.getNodeType() == SIMPLE_NAME) {
                        viewVariable = (SimpleName) leftHandSideNode;
                    } else {
                        resetData();
                        return VISIT_SUBTREE;
                    }
                    viewAssignmentStmt = getAncestorOrNull(viewVariableAssignment, ExpressionStatement.class);
                }
                return DO_NOT_VISIT_SUBTREE;
            }
            return VISIT_SUBTREE;
        }

        public boolean visit(ReturnStatement node) {
            this.returnStmt = node;
            return VISIT_SUBTREE;
        }

        public boolean isInflateInsideIf() {
            if (this.viewAssignmentStmt != null) {
                Expression inflateExpr = getInflateExpr();
                return getFirstAncestorOrNull(this.viewAssignmentStmt, IfStatement.class, SwitchStatement.class) != null
                        // check whether inflate is inside a conditional
                        // assignment
                        || (inflateExpr != null && inflateExpr.getNodeType() == ASTNode.CONDITIONAL_EXPRESSION);
            }
            return false;
        }

        public Expression getInflateExpr() {
            if (this.viewVariableDeclarationFragment != null) {
                return this.viewVariableDeclarationFragment.getInitializer();
            } else if (this.viewVariableAssignment != null) {
                return this.viewVariableAssignment.getRightHandSide();
            }
            return null;
        }

        private boolean isInflateMethod(MethodInvocation node) {
            return isMethod(node, "android.view.LayoutInflater", "inflate", "int", "android.view.ViewGroup")
                    || isMethod(node, "android.view.LayoutInflater", "inflate", "int", "android.view.ViewGroup",
                            "boolean")
                    || isMethod(node, "android.view.LayoutInflater", "inflate", "org.xmlpull.v1.XmlPullParser",
                            "android.view.ViewGroup")
                    || isMethod(node, "android.view.LayoutInflater", "inflate", "org.xmlpull.v1.XmlPullParser",
                            "android.view.ViewGroup", "boolean");
        }
    }

    static class FindViewByIdVisitor extends ASTVisitor {
        private List<FindViewByIdItem> items = new ArrayList<FindViewByIdItem>();

        static class FindViewByIdItem {
            private SimpleName variable;
            private Expression findViewByIdExpr;
            private VariableDeclarationFragment findViewByIdDeclarationFragment;
            private Assignment findViewByIdVariableAssignment;
            private MethodInvocation findViewByIdInvocation;

            private boolean setAssignment(MethodInvocation node) {
                this.findViewByIdInvocation = node;
                ASTNode ancestor = getFirstAncestorOrNull(node, VariableDeclarationFragment.class, Assignment.class);
                if (ancestor instanceof VariableDeclarationFragment) {
                    findViewByIdDeclarationFragment = (VariableDeclarationFragment) ancestor;
                    variable = findViewByIdDeclarationFragment.getName();
                    findViewByIdExpr = findViewByIdDeclarationFragment.getInitializer();
                } else if (ancestor instanceof Assignment) {
                    findViewByIdVariableAssignment = (Assignment) ancestor;
                    ASTNode leftSide = findViewByIdVariableAssignment.getLeftHandSide();
                    if (leftSide.getNodeType() == SIMPLE_NAME) {
                        variable = (SimpleName) leftSide;
                    } else {
                        return false;
                    }
                    findViewByIdExpr = findViewByIdVariableAssignment.getRightHandSide();
                }
                return true;
            }
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (isMethod(node, "android.view.View", "findViewById", "int")) {
                FindViewByIdItem item = new FindViewByIdItem();
                if (item.setAssignment(node)) {
                    items.add(item);
                }
            }
            return VISIT_SUBTREE;
        }
    }

}
