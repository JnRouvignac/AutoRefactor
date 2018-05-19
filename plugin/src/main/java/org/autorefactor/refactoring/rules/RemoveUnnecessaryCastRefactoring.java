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
package org.autorefactor.refactoring.rules;

import static org.autorefactor.refactoring.ASTHelper.DO_NOT_VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.VISIT_SUBTREE;
import static org.autorefactor.refactoring.ASTHelper.allOperands;
import static org.autorefactor.refactoring.ASTHelper.as;
import static org.autorefactor.refactoring.ASTHelper.getAncestor;
import static org.autorefactor.refactoring.ASTHelper.hasOperator;
import static org.autorefactor.refactoring.ASTHelper.hasType;
import static org.autorefactor.refactoring.ASTHelper.isPrimitive;
import static org.autorefactor.refactoring.ASTHelper.resolveTypeBinding;
import static org.eclipse.jdt.core.dom.ASTNode.ASSIGNMENT;
import static org.eclipse.jdt.core.dom.ASTNode.INFIX_EXPRESSION;
import static org.eclipse.jdt.core.dom.ASTNode.RETURN_STATEMENT;
import static org.eclipse.jdt.core.dom.ASTNode.VARIABLE_DECLARATION_FRAGMENT;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.DIVIDE;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.MINUS;
import static org.eclipse.jdt.core.dom.InfixExpression.Operator.PLUS;

import java.util.Iterator;
import java.util.List;

import org.autorefactor.refactoring.ASTBuilder;
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
public class RemoveUnnecessaryCastRefactoring extends AbstractRefactoringRule {
    /**
     * Get the name.
     *
     * @return the name.
     */
    public String getName() {
        return "Remove unnecessary casts";
    }

    /**
     * Get the description.
     *
     * @return the description.
     */
    public String getDescription() {
        return ""
                + "Removes unnecessary widening casts from return statements, assignments and infix expressions. "
                + "Correctly types literals. ";
    }

    /**
     * Get the reason.
     *
     * @return the reason.
     */
    public String getReason() {
        return "It reduces code to focus attention on code that matters."
                + " It also improves the time and the space performance.";
    }

    @Override
    public boolean visit(CastExpression node) {
        final NumberLiteral literal = as(node.getExpression(), NumberLiteral.class);
        if (literal != null
                && (literal.getToken().matches(".*[^lLdDfF]") || literal.getToken().matches("0x.*[^lL]"))) {
            if (hasType(node.getType().resolveBinding(), "long")) {
                createPrimitive(node, literal, 'L');
                return DO_NOT_VISIT_SUBTREE;
            }

            if (hasType(node.getType().resolveBinding(), "float")) {
                createPrimitive(node, literal, 'f');
                return DO_NOT_VISIT_SUBTREE;
            }

            if (hasType(node.getType().resolveBinding(), "double")) {
                createPrimitive(node, literal, 'd');
                return DO_NOT_VISIT_SUBTREE;
            }
        }

        if (canRemoveCast(node)) {
            final ASTBuilder b = ctx.getASTBuilder();
            ctx.getRefactorings().replace(node, b.move(node.getExpression()));
            return DO_NOT_VISIT_SUBTREE;
        }
        return VISIT_SUBTREE;
    }

    private void createPrimitive(final CastExpression node, final NumberLiteral literal, final char postfix) {
        final ASTBuilder b = this.ctx.getASTBuilder();

        final NumberLiteral numberLiteral = b.numberLiteral();
        numberLiteral.setToken(literal.getToken() + postfix);

        ctx.getRefactorings().replace(node, numberLiteral);
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
            return isAssignmentCompatible(node.getExpression(), resolveTypeBinding(vdf))
                    || isConstantExpressionAssignmentConversion(node);

        case INFIX_EXPRESSION:
            final InfixExpression ie = (InfixExpression) parent;
            final Expression lo = ie.getLeftOperand();
            final Expression ro = ie.getRightOperand();
            if (node.equals(lo)) {
                return (isStringConcat(ie) || isAssignmentCompatible(node.getExpression(), ro))
                        && !isPrimitiveTypeNarrowing(node)
                        && !hasOperator(ie, DIVIDE)
                        && !hasOperator(ie, PLUS)
                        && !hasOperator(ie, MINUS);
            } else {
                final boolean integralDivision = isIntegralDivision(ie);
                return ((isNotRefactored(lo) && isStringConcat(ie))
                            || (!integralDivision && isAssignmentCompatibleInInfixExpression(node, ie))
                            || (integralDivision && canRemoveCastInIntegralDivision(node, ie)))
                        && !isPrimitiveTypeNarrowing(node)
                        && !isIntegralDividedByFloatingPoint(node, ie);
            }
        }
        return false;
    }

    private boolean canRemoveCastInIntegralDivision(CastExpression node, InfixExpression ie) {
        final ITypeBinding leftOperandType = getLeftOperandType(ie, node);
        return isIntegralDivision(ie) // safety check
                && isAssignmentCompatible(node.getExpression().resolveTypeBinding(), leftOperandType)
                && compareTo(node.resolveTypeBinding(), leftOperandType) >= 0;
    }

    private boolean isIntegralDivision(final InfixExpression ie) {
        return isIntegralType(ie) && hasOperator(ie, DIVIDE);
    }

    private boolean isAssignmentCompatibleInInfixExpression(final CastExpression node, final InfixExpression ie) {
        final ITypeBinding leftOpType = getLeftOperandType(ie, node);
        return isAssignmentCompatible(node.getExpression().resolveTypeBinding(), leftOpType)
                && isAssignmentCompatible(node.resolveTypeBinding(), leftOpType);
    }

    private ITypeBinding getLeftOperandType(InfixExpression ie, CastExpression node) {
        final List<Expression> operands = allOperands(ie);
        final List<Expression> previousOperands = operands.subList(0, operands.indexOf(node));
        if (isAnyRefactored(previousOperands)) {
            return null;
        }
        return getTypeBinding(previousOperands);
    }

    private ITypeBinding getTypeBinding(final List<Expression> previousOperands) {
        final Iterator<Expression> it = previousOperands.iterator();
        ITypeBinding maxTypeBinding = it.next().resolveTypeBinding();
        while (it.hasNext()) {
            final ITypeBinding typeBinding = it.next().resolveTypeBinding();
            if (compareTo(maxTypeBinding, typeBinding) < 0) {
                maxTypeBinding = typeBinding;
            }
        }
        return maxTypeBinding;
    }

    private int compareTo(ITypeBinding binding1, ITypeBinding binding2) {
        final int rank1 = toPseudoEnum(binding1.getQualifiedName());
        final int rank2 = toPseudoEnum(binding2.getQualifiedName());
        return rank1 - rank2;
    }

    private int toPseudoEnum(String name) {
        if (name.equals("byte") || name.equals("java.lang.Byte")) {
            return 1;
        } else if (name.equals("short") || name.equals("java.lang.Short")) {
            return 2;
        } else if (name.equals("char") || name.equals("java.lang.Character")) {
            return 3;
        } else if (name.equals("int") || name.equals("java.lang.Integer")) {
            return 4;
        } else if (name.equals("long") || name.equals("java.lang.Long")) {
            return 5;
        } else if (name.equals("float") || name.equals("java.lang.Float")) {
            return 6;
        } else if (name.equals("double") || name.equals("java.lang.Double")) {
            return 7;
        }
        throw new NotImplementedException(null, "for type '" + name + "'");
    }

    private boolean isAnyRefactored(final List<Expression> operands) {
        for (Expression operand : operands) {
            if (!isNotRefactored(operand)) {
                return true;
            }
        }
        return false;
    }

    /** If left operand is refactored, we cannot easily make inferences about right operand. Wait for next iteration. */
    private boolean isNotRefactored(Expression leftOperand) {
        return preVisit2(leftOperand);
    }

    private boolean isIntegralDividedByFloatingPoint(CastExpression node, InfixExpression ie) {
        final Expression rightOp = ie.getRightOperand();
        return isIntegralType(ie.getLeftOperand())
                && hasOperator(ie, DIVIDE)
                && isFloatingPointType(rightOp)
                && node.equals(rightOp);
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
            return     (hasType(node, "byte")  && Byte.MIN_VALUE  <= val && val <= Byte.MAX_VALUE)
                    || (hasType(node, "short") && Short.MIN_VALUE <= val && val <= Short.MAX_VALUE)
                    || (hasType(node, "char")  && 0               <= val && val <= 65535);
        }
        return false;
    }

    private boolean isStringConcat(InfixExpression ie) {
        return hasType(ie, "java.lang.String");
    }

    private boolean isPrimitiveTypeNarrowing(CastExpression node) {
        final ITypeBinding castTypeBinding = node.getType().resolveBinding();
        final ITypeBinding exprTypeBinding = node.getExpression().resolveTypeBinding();
        return isPrimitive(castTypeBinding)
                && isPrimitive(exprTypeBinding)
                && isAssignmentCompatible(castTypeBinding, exprTypeBinding);
    }

    private boolean isAssignmentCompatible(Expression expr, Type type) {
        if (expr != null && type != null) {
            return isAssignmentCompatible(expr.resolveTypeBinding(), type.resolveBinding());
        }
        return false;
    }

    private boolean isAssignmentCompatible(Expression expr, ITypeBinding typeBinding) {
        if (expr != null && typeBinding != null) {
            return isAssignmentCompatible(expr.resolveTypeBinding(), typeBinding);
        }
        return false;
    }

    private boolean isAssignmentCompatible(Expression expr1, Expression expr2) {
        if (expr1 != null && expr2 != null) {
            return isAssignmentCompatible(expr1.resolveTypeBinding(), expr2.resolveTypeBinding());
        }
        return false;
    }

    private boolean isAssignmentCompatible(final ITypeBinding targetBinding, final ITypeBinding sourceBinding) {
        if (targetBinding != null && sourceBinding != null) {
            return targetBinding.isAssignmentCompatible(sourceBinding);
        }
        return false;
    }

}
