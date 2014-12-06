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
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

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
            return isAssignmentCompatible(node.getExpression(), as)
                    || isConstantExpressionAssignmentConversion(node);

        case VARIABLE_DECLARATION_FRAGMENT:
            final VariableDeclarationFragment vdf = (VariableDeclarationFragment) parent;
            return isAssignmentCompatible(node.getExpression(), getType(vdf))
                    || isConstantExpressionAssignmentConversion(node);

        case INFIX_EXPRESSION:
            final InfixExpression ie = (InfixExpression) parent;
            final boolean isDivision = Operator.DIVIDE.equals(ie.getOperator());
            final Expression lo = ie.getLeftOperand();
            final Expression ro = ie.getRightOperand();
            if (node.equals(lo)) {
                return (isAssignmentCompatible(node.getExpression(), ro) || isStringConcat(ie))
                        && !isPrimitiveTypeNarrowing(node, ie)
                        && !isDivision;
            } else {
                return (isAssignmentCompatible(node.getExpression(), lo) || isStringConcat(ie))
                        && !isPrimitiveTypeNarrowing(node, ie)
                        && !(isDivision && isIntegralType(lo) && isFloatingPointType(ro));
            }
        }
        return false;
    }

    private boolean isIntegralType(final Expression expr) {
        return hasType(expr, "byte", "char", "short", "int", "long");
    }

    private boolean isFloatingPointType(final Expression expr) {
        return hasType(expr, "float", "double");
    }

    /** @see JLS, section 5.2 Assignment Conversion */
    private boolean isConstantExpressionAssignmentConversion(CastExpression node) {
        final Object value = node.getExpression().resolveConstantExpressionValue();
        if (value instanceof Integer) {
            final int val = (Integer) value;
            return     (hasType(node, "byte")  &&   -128 <= val && val <= 127)
                    || (hasType(node, "short") && -32768 <= val && val <= 32767)
                    || (hasType(node, "char")  &&      0 <= val && val <= 65535);
        }
        return false;
    }

    private boolean isStringConcat(InfixExpression ie) {
        return hasType(ie, "java.lang.String");
    }

    private boolean isPrimitiveTypeNarrowing(CastExpression node, InfixExpression parent) {
        final ITypeBinding typeBinding1 = node.getType().resolveBinding();
        final ITypeBinding typeBinding2 = node.getExpression().resolveTypeBinding();
        return isPrimitive(typeBinding1)
                && isPrimitive(typeBinding2)
                && isAssignmentCompatible(typeBinding1, typeBinding2);
    }

    private boolean isAssignmentCompatible(Expression expr, Type type) {
        if (expr != null && type != null) {
            return isAssignmentCompatible(expr.resolveTypeBinding(), type.resolveBinding());
        }
        return false;
    }

    private boolean isAssignmentCompatible(Expression expr1, Expression expr2) {
        if (expr1 != null && expr2 != null) {
            return isAssignmentCompatible(expr1.resolveTypeBinding(), expr2.resolveTypeBinding());
        }
        return false;
    }

    private boolean isAssignmentCompatible(final ITypeBinding binding1, final ITypeBinding binding2) {
        if (binding1 != null && binding2 != null) {
            return binding1.isAssignmentCompatible(binding2);
        }
        return false;
    }

}
