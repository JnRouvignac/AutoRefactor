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

import org.autorefactor.refactoring.ASTBuilder;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationStatement;

import static org.autorefactor.refactoring.ASTHelper.*;
import static org.eclipse.jdt.core.dom.ASTNode.*;

/**
 * Removes unnecessary widening casts from return statements, assignments and
 * infix expressions.
 */
public class RemoveUnnecessaryCastRefactoring extends AbstractRefactoring {

    // TODO JNR remove casts from method parameters

    /** {@inheritDoc} */
    @Override
    public boolean visit(CastExpression node) {
        if (canRemoveCast(node)) {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.move(node.getExpression()));
        }
        return VISIT_SUBTREE;
    }

    private boolean canRemoveCast(CastExpression node) {
        final ASTNode parent = node.getParent();
        switch (parent.getNodeType()) {
        case RETURN_STATEMENT:
            final MethodDeclaration md = getAncestor(parent, MethodDeclaration.class);
            return isAssignmentCompatible(node.getExpression(), md.getReturnType2());

        case ASSIGNMENT:
            final Assignment as = (Assignment) parent;
            return isAssignmentCompatible(node.getExpression(), as);

        case VARIABLE_DECLARATION_FRAGMENT:
            final VariableDeclarationFragment vdf = (VariableDeclarationFragment) parent;
            return isAssignmentCompatible(node.getExpression(), getType(vdf));

        case INFIX_EXPRESSION:
            final InfixExpression ie = (InfixExpression) parent;
            if (extendedOperands(ie).isEmpty()) {
                final Expression lo = ie.getLeftOperand();
                final Expression ro = ie.getRightOperand();
                if (node.equals(lo)) {
                    return isAssignmentCompatible(node.getExpression(), ro);
                } else if (node.equals(ro)) {
                    return isAssignmentCompatible(node.getExpression(), lo);
                }
            } // TODO JNR support extended operands
            break;

        }
        return false;
    }

    private Type getType(final VariableDeclarationFragment vdf) {
        final ASTNode parent = vdf.getParent();
        if (parent instanceof VariableDeclarationStatement) {
            final VariableDeclarationStatement vds = (VariableDeclarationStatement) parent;
            return vds.getType();
        } else if (parent instanceof VariableDeclarationExpression) {
            final VariableDeclarationExpression vde = (VariableDeclarationExpression) parent;
            return vde.getType();
        }
        return null;
    }

    private boolean isAssignmentCompatible(Expression expr, Type type) {
        if (expr != null && type != null) {
            final ITypeBinding binding1 = expr.resolveTypeBinding();
            final ITypeBinding binding2 = type.resolveBinding();
            if (binding1 != null && binding2 != null) {
                return binding1.isAssignmentCompatible(binding2);
            }
        }
        return false;
    }

    private boolean isAssignmentCompatible(Expression expr1, Expression expr2) {
        if (expr1 != null && expr2 != null) {
            final ITypeBinding binding1 = expr1.resolveTypeBinding();
            final ITypeBinding binding2 = expr2.resolveTypeBinding();
            if (binding1 != null && binding2 != null) {
                return binding1.isAssignmentCompatible(binding2);
            }
        }
        return false;
    }

}
