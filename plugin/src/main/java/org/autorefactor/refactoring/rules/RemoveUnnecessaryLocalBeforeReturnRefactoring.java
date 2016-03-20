/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2013-2016 Jean-Noël Rouvignac - initial API and implementation
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
import org.autorefactor.refactoring.Refactorings;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ArrayCreation;
import org.eclipse.jdt.core.dom.ArrayInitializer;
import org.eclipse.jdt.core.dom.ArrayType;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.IVariableBinding;
import org.eclipse.jdt.core.dom.ReturnStatement;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.Statement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.Assignment.Operator.*;

/** See {@link #getDescription()} method. */
@SuppressWarnings("javadoc")
public class RemoveUnnecessaryLocalBeforeReturnRefactoring extends AbstractRefactoringRule {

    @Override
    public String getDescription() {
        return ""
            + "Removes unnecessary local variable declaration"
            + " or unnecessary variable assignment before a return statement.";
    }

    @Override
    public String getName() {
        return "Remove unnecessary local before return";
    }

    @Override
    public boolean visit(ReturnStatement node) {
        final Statement previousSibling = getPreviousSibling(node);
        if (previousSibling instanceof VariableDeclarationStatement) {
            final VariableDeclarationStatement vds = (VariableDeclarationStatement) previousSibling;
            if (fragments(vds).size() == 1) {
                final VariableDeclarationFragment vdf = fragments(vds).get(0);
                if (isSameLocalVariableBinding(node.getExpression(), vdf.getName())) {
                    final Expression returnExpr = vdf.getInitializer();
                    if (returnExpr instanceof ArrayInitializer) {
                        final ASTBuilder b = ctx.getASTBuilder();
                        final ReturnStatement newReturnStmt =
                                b.return0(b.newArray(
                                        b.copy((ArrayType) vds.getType()),
                                        b.move((ArrayInitializer) returnExpr)));
                        replaceReturnStatementForArray(node, vds, newReturnStmt);
                    } else {
                        replaceReturnStatement(node, vds, returnExpr);
                    }
                    return DO_NOT_VISIT_SUBTREE;
                }
            }
        } else {
            final Assignment as = asExpression(previousSibling, Assignment.class);
            if (hasOperator(as, ASSIGN)
                    && isSameLocalVariableBinding(node.getExpression(), as.getLeftHandSide())) {
                final Expression returnExpr = as.getRightHandSide();
                if (isArray(returnExpr)) {
                    final ASTBuilder b = ctx.getASTBuilder();
                    final ReturnStatement newReturnStmt =
                            b.return0(b.copy((ArrayCreation) returnExpr));
                    replaceReturnStatementForArray(node, previousSibling, newReturnStmt);
                } else {
                    replaceReturnStatement(node, previousSibling, returnExpr);
                }
                return DO_NOT_VISIT_SUBTREE;
            }
        }
        return VISIT_SUBTREE;
    }

    private void replaceReturnStatementForArray(ReturnStatement node, final ASTNode previousSibling,
            ReturnStatement newReturnStmt) {
        final Refactorings r = ctx.getRefactorings();
        r.remove(previousSibling);
        r.replace(node, newReturnStmt);
    }

    private void replaceReturnStatement(ReturnStatement node, final ASTNode previousSibling,
            Expression returnExpr) {
        final ASTBuilder b = ctx.getASTBuilder();
        final Refactorings r = ctx.getRefactorings();
        r.remove(previousSibling);
        r.replace(node, b.return0(b.move(returnExpr)));
    }

    private boolean isSameLocalVariableBinding(Expression expr1, Expression expr2) {
        if (expr1 instanceof SimpleName && expr2 instanceof SimpleName) {
            final SimpleName sn1 = (SimpleName) expr1;
            final SimpleName sn2 = (SimpleName) expr2;
            final IVariableBinding bnd1 = (IVariableBinding) sn1.resolveBinding();
            final IVariableBinding bnd2 = (IVariableBinding) sn2.resolveBinding();
            // to avoid changing the class's behaviour,
            // we must not remove field's assignment
            return bnd1 != null && bnd2 != null && !bnd1.isField() && !bnd2.isField() && bnd1.isEqualTo(bnd2);
        }
        return false;
    }
}
