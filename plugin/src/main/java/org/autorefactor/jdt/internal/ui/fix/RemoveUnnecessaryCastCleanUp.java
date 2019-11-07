/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2014-2016 Jean-Noël Rouvignac - initial API and implementation
 * Copyright (C) 2016 Fabrice Tiercelin - Make sure we do not visit again modified nodes
 * Copyright (C) 2018 Fabrice Tiercelin - Adds 'L', 'f' or 'd' to type literals.
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
package org.autorefactor.jdt.internal.ui.fix;

import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;

import java.util.Iterator;
import java.util.List;

import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.Assignment;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

/**
 * See {@link #getDescription()} method.
 * <p>
 * TODO JNR remove casts from method parameters
 */
public class RemoveUnnecessaryCastCleanUp extends AbstractCleanUpRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryCastCleanUp_name;
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryCastCleanUp_description;
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return MultiFixMessages.CleanUpRefactoringWizard_RemoveUnnecessaryCastCleanUp_reason;
    }

    @Override
    public boolean visit(CastExpression node) {
        final NumberLiteral literal= ASTNodes.as(node.getExpression(), NumberLiteral.class);
        if (literal != null && (literal.getToken().matches(".*[^lLdDfF]") || literal.getToken().matches("0x.*[^lL]"))) { //$NON-NLS-1$ //$NON-NLS-2$
            if (ASTNodes.hasType(node.getType().resolveBinding(), long.class.getSimpleName())) {
                createPrimitive(node, literal, 'L');
                return false;
            }

            if (ASTNodes.hasType(node.getType().resolveBinding(), float.class.getSimpleName())) {
                createPrimitive(node, literal, 'f');
                return false;
            }

            if (ASTNodes.hasType(node.getType().resolveBinding(), double.class.getSimpleName())) {
                createPrimitive(node, literal, 'd');
                return false;
            }
        }

        if (canRemoveCast(node)) {
            final ASTNodeFactory b= ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.move(node.getExpression()));
            return false;
        }

        return true;
    }

    private void createPrimitive(final CastExpression node, final NumberLiteral literal, final char postfix) {
        final ASTNodeFactory b= this.ctx.getASTBuilder();

        final NumberLiteral numberLiteral= b.number(literal.getToken() + postfix);

        ctx.getRefactorings().replace(node, numberLiteral);
    }

    private boolean canRemoveCast(CastExpression node) {
        final ASTNode parent= node.getParent();
        switch (parent.getNodeType()) {
        case RETURN_STATEMENT:
            final MethodDeclaration md= ASTNodes.getAncestor(parent, MethodDeclaration.class);
            return isAssignmentCompatible(node.getExpression(), md.getReturnType2())
                    || isConstantExpressionAssignmentConversion(node);

        case ASSIGNMENT:
            final Assignment as= (Assignment) parent;
            return isAssignmentCompatible(node.getExpression(), as) || isConstantExpressionAssignmentConversion(node);

        case VARIABLE_DECLARATION_FRAGMENT:
            final VariableDeclarationFragment vdf= (VariableDeclarationFragment) parent;
            return isAssignmentCompatible(node.getExpression(), ASTNodes.resolveTypeBinding(vdf))
                    || isConstantExpressionAssignmentConversion(node);

        case INFIX_EXPRESSION:
            if (!isPrimitiveTypeNarrowing(node)) {
                final InfixExpression ie= (InfixExpression) parent;
                final Expression lo= ie.getLeftOperand();
                final Expression ro= ie.getRightOperand();

                if (node.equals(lo)) {
                    return (isStringConcat(ie) || isAssignmentCompatible(node.getExpression(), ro))
                            && !ASTNodes.hasOperator(ie, InfixExpression.Operator.DIVIDE, InfixExpression.Operator.PLUS, InfixExpression.Operator.MINUS);
                }
                final boolean integralDivision= isIntegralDivision(ie);
                return ((isNotRefactored(lo) && isStringConcat(ie))
                        || (integralDivision ? canRemoveCastInIntegralDivision(node, ie)
                                : isAssignmentCompatibleInInfixExpression(node, ie)))
                        && !isIntegralDividedByFloatingPoint(node, ie);
            }
        }

        return false;
    }

    private boolean canRemoveCastInIntegralDivision(CastExpression node, InfixExpression ie) {
        final ITypeBinding leftOperandType= getLeftOperandType(ie, node);
        return isIntegralDivision(ie) // safety check
                && isAssignmentCompatible(node.getExpression().resolveTypeBinding(), leftOperandType)
                && compareTo(node.resolveTypeBinding(), leftOperandType) >= 0;
    }

    private boolean isIntegralDivision(final InfixExpression ie) {
        return isIntegralType(ie) && ASTNodes.hasOperator(ie, InfixExpression.Operator.DIVIDE);
    }

    private boolean isAssignmentCompatibleInInfixExpression(final CastExpression node, final InfixExpression ie) {
        final ITypeBinding leftOpType= getLeftOperandType(ie, node);
        return isAssignmentCompatible(node.getExpression().resolveTypeBinding(), leftOpType)
                && isAssignmentCompatible(node.resolveTypeBinding(), leftOpType);
    }

    private ITypeBinding getLeftOperandType(InfixExpression ie, CastExpression node) {
        final List<Expression> operands= ASTNodes.allOperands(ie);
        final List<Expression> previousOperands= operands.subList(0, operands.indexOf(node));
        if (isAnyRefactored(previousOperands)) {
            return null;
        }

        return getTypeBinding(previousOperands);
    }

    private ITypeBinding getTypeBinding(final List<Expression> previousOperands) {
        final Iterator<Expression> it= previousOperands.iterator();
        ITypeBinding maxTypeBinding= it.next().resolveTypeBinding();
        while (it.hasNext()) {
            final ITypeBinding typeBinding= it.next().resolveTypeBinding();
            if (compareTo(maxTypeBinding, typeBinding) < 0) {
                maxTypeBinding= typeBinding;
            }
        }

        return maxTypeBinding;
    }

    private int compareTo(ITypeBinding binding1, ITypeBinding binding2) {
        final int rank1= toPseudoEnum(binding1.getQualifiedName());
        final int rank2= toPseudoEnum(binding2.getQualifiedName());
        return rank1 - rank2;
    }

    private int toPseudoEnum(String name) {
        if (byte.class.getSimpleName().equals(name) || Byte.class.getCanonicalName().equals(name)) {
            return 1;
        }
        if (short.class.getSimpleName().equals(name) || Short.class.getCanonicalName().equals(name)) {
            return 2;
        }
        if (char.class.getSimpleName().equals(name) || Character.class.getCanonicalName().equals(name)) {
            return 3;
        }
        if (int.class.getSimpleName().equals(name) || Integer.class.getCanonicalName().equals(name)) {
            return 4;
        }
        if (long.class.getSimpleName().equals(name) || Long.class.getCanonicalName().equals(name)) {
            return 5;
        }
        if (float.class.getSimpleName().equals(name) || Float.class.getCanonicalName().equals(name)) {
            return 6;
        }
        if (double.class.getSimpleName().equals(name) || Double.class.getCanonicalName().equals(name)) {
            return 7;
        }
        throw new NotImplementedException(null, "for type '" + name + "'"); //$NON-NLS-1$ //$NON-NLS-2$
    }

    private boolean isAnyRefactored(final List<Expression> operands) {
        for (Expression operand : operands) {
            if (!isNotRefactored(operand)) {
                return true;
            }
        }

        return false;
    }

    /**
     * If left operand is refactored, we cannot easily make inferences about right
     * operand. Wait for next iteration.
     */
    private boolean isNotRefactored(Expression leftOperand) {
        return preVisit2(leftOperand);
    }

    private boolean isIntegralDividedByFloatingPoint(CastExpression node, InfixExpression ie) {
        final Expression rightOp= ie.getRightOperand();
        return isIntegralType(ie.getLeftOperand()) && ASTNodes.hasOperator(ie, InfixExpression.Operator.DIVIDE) && isFloatingPointType(rightOp)
                && node.equals(rightOp);
    }

    private boolean isIntegralType(final Expression expression) {
        return ASTNodes.hasType(expression, byte.class.getSimpleName(), char.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName());
    }

    private boolean isFloatingPointType(final Expression expression) {
        return ASTNodes.hasType(expression, float.class.getSimpleName(), double.class.getSimpleName());
    }

    /** @see JLS, section 5.2 Assignment Conversion */
    private boolean isConstantExpressionAssignmentConversion(CastExpression node) {
        final Object value= node.getExpression().resolveConstantExpressionValue();
        if (value instanceof Integer) {
            final int val= (Integer) value;
            return (ASTNodes.hasType(node, byte.class.getSimpleName()) && Byte.MIN_VALUE <= val && val <= Byte.MAX_VALUE)
                    || (ASTNodes.hasType(node, short.class.getSimpleName()) && Short.MIN_VALUE <= val && val <= Short.MAX_VALUE)
                    || (ASTNodes.hasType(node, char.class.getSimpleName()) && 0 <= val && val <= 65535);
        }

        return false;
    }

    private boolean isStringConcat(InfixExpression ie) {
        return ASTNodes.hasType(ie, String.class.getCanonicalName());
    }

    private boolean isPrimitiveTypeNarrowing(CastExpression node) {
        final ITypeBinding castTypeBinding= node.getType().resolveBinding();
        final ITypeBinding exprTypeBinding= node.getExpression().resolveTypeBinding();
        return ASTNodes.isPrimitive(castTypeBinding) && ASTNodes.isPrimitive(exprTypeBinding)
                && isAssignmentCompatible(castTypeBinding, exprTypeBinding);
    }

    private boolean isAssignmentCompatible(Expression expression, Type type) {
        return expression != null && type != null && isAssignmentCompatible(expression.resolveTypeBinding(), type.resolveBinding());
    }

    private boolean isAssignmentCompatible(Expression expression, ITypeBinding typeBinding) {
        return expression != null && typeBinding != null && isAssignmentCompatible(expression.resolveTypeBinding(), typeBinding);
    }

    private boolean isAssignmentCompatible(Expression expr1, Expression expr2) {
        return expr1 != null && expr2 != null
                && isAssignmentCompatible(expr1.resolveTypeBinding(), expr2.resolveTypeBinding());
    }

    private boolean isAssignmentCompatible(final ITypeBinding targetBinding, final ITypeBinding sourceBinding) {
        return targetBinding != null && sourceBinding != null && targetBinding.isAssignmentCompatible(sourceBinding);
    }
}
