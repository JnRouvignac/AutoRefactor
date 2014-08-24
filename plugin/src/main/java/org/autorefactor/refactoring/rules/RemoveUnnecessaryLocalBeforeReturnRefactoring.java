/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2014 Jean-Noël Rouvignac - initial API and implementation
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

import org.autorefactor.refactoring.ASTBuilder;
import org.autorefactor.refactoring.IJavaRefactoring;
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;

/**
 * Removes unnecessary local variable declaration or unnecessary variable
 * assignment before a return statement.
 */
public class RemoveUnnecessaryLocalBeforeReturnRefactoring extends ASTVisitor
        implements IJavaRefactoring {

    private RefactoringContext ctx;

    /** Default constructor. */
    public RemoveUnnecessaryLocalBeforeReturnRefactoring() {
        super();
    }

    /** {@inheritDoc} */
    @Override
    public void setRefactoringContext(RefactoringContext ctx) {
        this.ctx = ctx;
    }

    /** {@inheritDoc} */
    @Override
    public boolean visit(ReturnStatement node) {
        final Statement previousSibling = getPreviousSibling(node);
        if (previousSibling instanceof VariableDeclarationStatement) {
            final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousSibling;
            if (fragments(vds).size() == 1) {
                final VariableDeclarationFragment vdf = fragments(vds).get(0);
                final Expression origExpr = node.getExpression();
                if (origExpr instanceof SimpleName) {
                    replaceReturnStatement(node, vds, origExpr, vdf.getName(),
                            vdf.getInitializer());
                }
            }
        } else {
            final Assignment as = asExpression(previousSibling, Assignment.class);
            final Expression origExpr = node.getExpression();
            if (as != null && Assignment.Operator.ASSIGN.equals(as.getOperator())) {
                final Expression newExpr = as.getLeftHandSide();
                replaceReturnStatement(node, previousSibling, origExpr, newExpr,
                        as.getRightHandSide());
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceReturnStatement(ReturnStatement node,
            final ASTNode previousSibling, Expression expr1, Expression expr2,
            Expression returnExpr) {
        if (expr1 instanceof SimpleName && expr2 instanceof SimpleName) {
            final SimpleName sn1 = (SimpleName) expr1;
            final SimpleName sn2 = (SimpleName) expr2;
            final IVariableBinding bnd1 = (IVariableBinding) sn1.resolveBinding();
            final IVariableBinding bnd2 = (IVariableBinding) sn2.resolveBinding();
            if (bnd1 == null || bnd2 == null) {
                return;
            }
            // to avoid changing the class's behaviour,
            // we must not remove field's assignment
            if (!bnd1.isField() && !bnd2.isField() && bnd1.isEqualTo(bnd2)) {
                this.ctx.getRefactorings().remove(previousSibling);
                if (returnExpr instanceof ArrayInitializer && bnd1.getType().isArray()) {
                    this.ctx.getRefactorings().replace(node,
                        getReturnStatementForArray((ArrayInitializer) returnExpr, bnd1.getType()));
                } else {
                    this.ctx.getRefactorings().replace(node,
                        getReturnStatement(returnExpr));
                }
            }
        }
    }

    private ReturnStatement getReturnStatementForArray(ArrayInitializer returnAI, ITypeBinding typeBinding) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.return0(b.newArray(typeBinding, b.copyAll(expressions(returnAI))));
    }

    private ASTNode getReturnStatement(Expression initializer) {
        final ASTBuilder b = this.ctx.getASTBuilder();
        return b.return0(b.copy(initializer));
    }

    /** {@inheritDoc} */
    @Override
    public Refactorings getRefactorings(CompilationUnit astRoot) {
        astRoot.accept(this);
        return this.ctx.getRefactorings();
    }
}
