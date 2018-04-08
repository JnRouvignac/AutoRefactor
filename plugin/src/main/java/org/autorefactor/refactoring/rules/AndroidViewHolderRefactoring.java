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

import static java.util.Arrays.asList;
import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.bodyDeclarations;
import static org.autorefactor.refactoring.ASTHelper.getAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.getFirstAncestorOrNull;
import static org.autorefactor.refactoring.ASTHelper.isMethod;
import static org.autorefactor.refactoring.ASTHelper.isSameVariable;
import static org.autorefactor.refactoring.ASTHelper.modifiers;
import static org.autorefactor.refactoring.ASTHelper.parameters;
import static org.autorefactor.refactoring.ASTHelper.statements;
import static org.eclipse.jdt.core.dom.ASTNode.SIMPLE_NAME;
import static org.eclipse.jdt.core.dom.Assignment.Operator.ASSIGN;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.EQUALS;

import java.util.ArrayList;
import java.util.List;

import org.autorefactor.preferences.Preferences;
import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.Refactorings;
import org.autorefactor.refactoring.TypeNameDecider;
import org.autorefactor.refactoring.Variable;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ExpressionStatement;
import org.eclipse.jdt.core.dom.FieldAccess;
import org.eclipse.jdt.core.dom.FieldDeclaration;
import org.eclipse.jdt.core.dom.IfStatement;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.SingleVariableDeclaration;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.SwitchStatement;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

/**
 * TODO when findViewById is reusing a local variable, the viewHolderItem will create a new field
 * with duplicate name.
 * <P>
 * Possible solution: use the id names instead of var names
 * <P>
 * TODO sometimes convertView is a final param in getView.
 * <P>
 * Possible solution: remove final modifier
 *
 * @see {@link #getDescription()} method.
 */
public class AndroidViewHolderRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Android ViewHolder";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return "Android - Optimize getView() routines for Android applications. "
                + "It reduces the number calls to the costly inflate()) and getViewById() Android API methods.";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It improves the performance.";
    }

    @Override
    public boolean isEnabled(Preferences preferences) {
        // FIXME enable only when android libraries are detected
        return super.isEnabled(preferences);
    }

    @Override
    public boolean visit(MethodDeclaration node) {
        Block body = node.getBody();
        if (body != null && isMethod(node,
                "android.widget.Adapter", "getView", "int", "android.view.View", "android.view.ViewGroup")) {
            final GetViewVariableVisitor visitor = new GetViewVariableVisitor();
            body.accept(visitor);
            if (visitor.canApplyRefactoring()) {
                final ASTBuilder b = this.ctx.getASTBuilder();
                final Refactorings r = this.ctx.getRefactorings();
                final TypeNameDecider typeNameDecider = new TypeNameDecider(visitor.viewVariableName);

                // Transform tree

                // Create If statement
                final SingleVariableDeclaration viewArg = parameters(node).get(1);
                final Variable convertViewVar = new Variable(viewArg.getName().getIdentifier(), b);
                final InfixExpression condition = b.infixExpr(convertViewVar.varName(), EQUALS, b.null0());
                final Block thenBlock = b.block();
                final IfStatement ifStmt = b.if0(condition, thenBlock);
                r.insertBefore(ifStmt, visitor.viewAssignmentStmt);
                final List<Statement> thenStmts = statements(thenBlock);

                thenStmts.add(b.toStmt(b.assign(convertViewVar.varName(), ASSIGN, b.copy(visitor.getInflateExpr()))));

                // assign to local view variable when necessary
                if (!"convertView".equals(visitor.viewVariableName.getIdentifier())) {
                    Statement assignConvertViewToView = null;
                    if (visitor.viewVariableDeclFragment != null) {
                        assignConvertViewToView = b.declareStmt(
                                b.copyType(visitor.viewVariableName, typeNameDecider),
                                b.copy(visitor.viewVariableName), convertViewVar.varName());
                    } else if (visitor.viewVariableAssignment != null) {
                        assignConvertViewToView = b.toStmt(
                            b.assign(b.copy(visitor.viewVariableName), ASSIGN, convertViewVar.varName()));
                    }
                    if (assignConvertViewToView != null) {
                        r.insertBefore(assignConvertViewToView, visitor.viewAssignmentStmt);
                    }
                }

                // make sure method returns the view to be reused
                if (visitor.returnStmt != null) {
                    r.insertAfter(b.return0(b.copy(visitor.viewVariableName)), visitor.returnStmt);
                    r.remove(visitor.returnStmt);
                }

                // Optimize findViewById calls
                final FindViewByIdVisitor findViewByIdVisitor = new FindViewByIdVisitor(visitor.viewVariableName);
                body.accept(findViewByIdVisitor);
                if (!findViewByIdVisitor.items.isEmpty()) {
                    // TODO JNR name conflict? Use VariableNameDecider
                    Variable viewHolderItemVar = new Variable("ViewHolderItem", "viewHolderItem", b);

                    // create ViewHolderItem class
                    r.insertBefore(
                        createViewHolderItemClass(findViewByIdVisitor, viewHolderItemVar.typeName(), typeNameDecider),
                        node);

                    // declare viewhHolderItem object
                    r.insertFirst(body, Block.STATEMENTS_PROPERTY, viewHolderItemVar.declareStmt());
                    // initialize viewHolderItem
                    thenStmts.add(b.toStmt(
                        b.assign(viewHolderItemVar.varName(), ASSIGN, b.new0(viewHolderItemVar.type()))));
                    // Assign findViewById to ViewHolderItem
                    for (FindViewByIdVisitor.FindViewByIdItem item : findViewByIdVisitor.items) {
                        // ensure we are accessing convertView object
                        FieldAccess fieldAccess = b.fieldAccess(
                            viewHolderItemVar.varName(),
                            b.simpleName(item.variable.getIdentifier()));
                        // FIXME This does not work: not sure why??
                        // r.set(item.findViewByIdInvocation,
                        // MethodInvocation.EXPRESSION_PROPERTY, convertViewVar.varName());
                        item.findViewByIdInvocation.setExpression(convertViewVar.varName());
                        // FIXME For some reason b.copy() does not do what we would like
                        thenStmts.add(b.toStmt(b.assign(fieldAccess, ASSIGN, b.copySubtree(item.findViewByIdExpr))));

                        // replace previous findViewById with accesses to viewHolderItem
                        r.replace(item.findViewByIdExpr, b.copy(fieldAccess));
                    }
                    // store viewHolderItem in convertView
                    thenStmts.add(b.toStmt(b.invoke("convertView", "setTag", viewHolderItemVar.varName())));

                    // retrieve viewHolderItem from convertView
                    ifStmt.setElseStatement(b.block(b.toStmt(
                        b.assign(viewHolderItemVar.varName(),
                                 ASSIGN,
                                 b.cast(viewHolderItemVar.type(), b.invoke("convertView", "getTag"))))));
                }
                r.remove(visitor.viewAssignmentStmt);
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private TypeDeclaration createViewHolderItemClass(
            FindViewByIdVisitor findViewByIdVisitor, SimpleName typeName, TypeNameDecider typeNameDecider) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        TypeDeclaration result = b.getAST().newTypeDeclaration();
        modifiers(result).addAll(asList(b.private0(), b.static0()));
        result.setName(typeName);
        List<BodyDeclaration> viewItemsFieldDecls = bodyDeclarations(result);
        for (FindViewByIdVisitor.FindViewByIdItem item : findViewByIdVisitor.items) {
            viewItemsFieldDecls.add(item.toFieldDecl(b, typeNameDecider));
        }
        return result;
    }

    /** Visitor that returns all the interesting accesses to the view variable. */
    private static final class GetViewVariableVisitor extends ASTVisitor {
        private SimpleName viewVariableName;
        /** {@code null} when {@link #viewVariableAssignment} is not null. */
        private VariableDeclarationFragment viewVariableDeclFragment;
        /** {@code null} when {@link #viewVariableDeclFragment} is not null. */
        private Assignment viewVariableAssignment;
        /** Statement which is  the ancestor node of the assignment node. */
        private Statement viewAssignmentStmt;
        private ReturnStatement returnStmt;

        private void resetData() {
            viewVariableName = null;
            viewVariableDeclFragment = null;
            viewVariableAssignment = null;
            viewAssignmentStmt = null;
            returnStmt = null;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (isInflateMethod(node)) {
                ASTNode ancestor = getFirstAncestorOrNull(node, VariableDeclarationFragment.class, Assignment.class);
                if (ancestor instanceof VariableDeclarationFragment) {
                    viewVariableDeclFragment = (VariableDeclarationFragment) ancestor;
                    viewVariableName = viewVariableDeclFragment.getName();
                    viewAssignmentStmt =
                        getAncestorOrNull(viewVariableDeclFragment, VariableDeclarationStatement.class);
                    if (viewAssignmentStmt == null) {
                        resetData();
                        return VISIT_SUBTREE;
                    }
                } else if (ancestor instanceof Assignment) {
                    viewVariableAssignment = (Assignment) ancestor;
                    final Expression lhs = viewVariableAssignment.getLeftHandSide();
                    if (lhs.getNodeType() == SIMPLE_NAME) {
                        viewVariableName = (SimpleName) lhs;
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

        @Override
        public boolean visit(ReturnStatement node) {
            this.returnStmt = node;
            return VISIT_SUBTREE;
        }

        private boolean canApplyRefactoring() {
            // we found a suitable variable to replace
            return viewVariableName != null && !isInflateInsideIf();
        }

        private boolean isInflateInsideIf() {
            if (this.viewAssignmentStmt != null) {
                Expression inflateExpr = getInflateExpr();
                return getFirstAncestorOrNull(this.viewAssignmentStmt, IfStatement.class, SwitchStatement.class) != null
                        // check whether inflate is inside a conditional assignment
                        || (inflateExpr != null && inflateExpr.getNodeType() == ASTNode.CONDITIONAL_EXPRESSION);
            }
            return false;
        }

        private Expression getInflateExpr() {
            if (this.viewVariableDeclFragment != null) {
                return this.viewVariableDeclFragment.getInitializer();
            } else if (this.viewVariableAssignment != null) {
                return this.viewVariableAssignment.getRightHandSide();
            }
            return null;
        }

        private boolean isInflateMethod(MethodInvocation node) {
            final String inflaterType = "android.view.LayoutInflater";
            final String viewGroupType = "android.view.ViewGroup";
            return isMethod(node, inflaterType, "inflate", "int", viewGroupType)
                || isMethod(node, inflaterType, "inflate", "int", viewGroupType, "boolean")
                || isMethod(node, inflaterType, "inflate", "org.xmlpull.v1.XmlPullParser", viewGroupType)
                || isMethod(node, inflaterType, "inflate", "org.xmlpull.v1.XmlPullParser", viewGroupType, "boolean");
        }
    }

    /** Finds calls to {@code android.view.View#findViewById(int)}. */
    private static final class FindViewByIdVisitor extends ASTVisitor {
        static class FindViewByIdItem {
            private SimpleName variable;
            private Expression findViewByIdExpr;
            private MethodInvocation findViewByIdInvocation;

            private boolean setAssignment(MethodInvocation node) {
                this.findViewByIdInvocation = node;
                ASTNode ancestor = getFirstAncestorOrNull(node, VariableDeclarationFragment.class, Assignment.class);
                if (ancestor instanceof VariableDeclarationFragment) {
                    final VariableDeclarationFragment fragment = (VariableDeclarationFragment) ancestor;
                    variable = fragment.getName();
                    findViewByIdExpr = fragment.getInitializer();
                } else if (ancestor instanceof Assignment) {
                    final Assignment as = (Assignment) ancestor;
                    final Expression lhs = as.getLeftHandSide();
                    if (lhs.getNodeType() == SIMPLE_NAME) {
                        variable = (SimpleName) lhs;
                    } else {
                        // Only simple names are handled.
                        // Using anything else than simple name is unexpected,
                        // and even so, it is unexpected that simple names
                        // would not mixed with qualified names, etc.
                        return false;
                    }
                    findViewByIdExpr = as.getRightHandSide();
                } else {
                    return false;
                }
                return true;
            }

            private FieldDeclaration toFieldDecl(final ASTBuilder b, final TypeNameDecider typeNameDecider) {
                final FieldDeclaration field = b.declareField(
                    b.copyType(variable, typeNameDecider),
                    b.declareFragment(b.copy(variable)));
                modifiers(field).add(b.private0());
                return field;
            }
        }

        private List<FindViewByIdItem> items = new ArrayList<FindViewByIdItem>();
        private SimpleName viewVariableName;

        private FindViewByIdVisitor(SimpleName viewVariableName) {
            this.viewVariableName = viewVariableName;
        }

        @Override
        public boolean visit(MethodInvocation node) {
            if (isMethod(node, "android.view.View", "findViewById", "int")
                    && isSameVariable(viewVariableName, node.getExpression())) {
                FindViewByIdItem item = new FindViewByIdItem();
                if (item.setAssignment(node)) {
                    items.add(item);
                }
            }
            return VISIT_SUBTREE;
        }
    }
}
