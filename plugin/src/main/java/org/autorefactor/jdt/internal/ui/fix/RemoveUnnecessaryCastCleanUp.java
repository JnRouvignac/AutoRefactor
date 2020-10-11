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

import java.util.Iterator;
import java.util.List;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.autorefactor.util.NotImplementedException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CastExpression;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.InfixExpression;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 */
public class RemoveUnnecessaryCastCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.RemoveUnnecessaryCastCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.RemoveUnnecessaryCastCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.RemoveUnnecessaryCastCleanUp_reason;
	}

	@Override
	public boolean visit(final CastExpression node) {
		NumberLiteral literal= ASTNodes.as(node.getExpression(), NumberLiteral.class);

		if (literal != null && (literal.getToken().matches(".*[^lLdDfF]") || literal.getToken().matches("0x.*[^lL]"))) { //$NON-NLS-1$ //$NON-NLS-2$
			if (ASTNodes.hasType(node.getType().resolveBinding(), long.class.getSimpleName())) {
				createPrimitive(node, literal, 'L');
				return false;
			}

			if (ASTNodes.hasType(node.getType().resolveBinding(), float.class.getSimpleName())) {
				createPrimitive(node, literal, 'F');
				return false;
			}

			if (ASTNodes.hasType(node.getType().resolveBinding(), double.class.getSimpleName())) {
				createPrimitive(node, literal, 'D');
				return false;
			}
		}

		if (canRemoveCast(node)) {
			ASTRewrite rewrite= cuRewrite.getASTRewrite();
			TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveUnnecessaryCastCleanUp_description);
			ASTNodes.replaceButKeepComment(rewrite, node, ASTNodes.createMoveTarget(rewrite, node.getExpression()), group);
			return false;
		}

		return true;
	}

	private void createPrimitive(final CastExpression node, final NumberLiteral literal, final char postfix) {
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		NumberLiteral numberLiteral= ast.newNumberLiteral(literal.getToken() + postfix);

		TextEditGroup group= new TextEditGroup(MultiFixMessages.RemoveUnnecessaryCastCleanUp_description);

		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		ASTNodes.replaceButKeepComment(rewrite, node, numberLiteral, group);
	}

	private boolean canRemoveCast(final CastExpression node) {
		if ((ASTNodes.hasType(node.getExpression(), char.class.getCanonicalName()) && ASTNodes.hasType(node, int.class.getCanonicalName()))
				|| (ASTNodes.hasType(node.getExpression(), byte.class.getCanonicalName()) && ASTNodes.hasType(node, char.class.getCanonicalName()))) {
			return false;
		}

		ITypeBinding targetType= ASTNodes.getTargetType(node);

		if (isAssignmentCompatible(node.getExpression(), targetType)) {
			return true;
		}

		ASTNode parent= node.getParent();
		switch (parent.getNodeType()) {
		case ASTNode.RETURN_STATEMENT:
		case ASTNode.ASSIGNMENT:
		case ASTNode.VARIABLE_DECLARATION_FRAGMENT:
			return isConstantExpressionAssignmentConversion(node);

		case ASTNode.INFIX_EXPRESSION:
			if (!isPrimitiveTypeNarrowing(node)) {
				InfixExpression infixExpression= (InfixExpression) parent;
				Expression lo= infixExpression.getLeftOperand();
				Expression ro= infixExpression.getRightOperand();

				if (node.equals(lo)) {
					return (isStringConcat(infixExpression) || isAssignmentCompatible(ro, node.getExpression()))
							&& !ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.DIVIDE, InfixExpression.Operator.PLUS, InfixExpression.Operator.MINUS);
				}

				return ((isNotRefactored(lo) && isStringConcat(infixExpression))
						|| (isIntegralDivision(infixExpression) ? canRemoveCastInIntegralDivision(node, infixExpression)
								: isAssignmentCompatibleInInfixExpression(node, infixExpression)))
						&& !isIntegralDividedByFloatingPoint(node, infixExpression);
			}
		}

		return false;
	}

	private boolean canRemoveCastInIntegralDivision(final CastExpression node, final InfixExpression infixExpression) {
		ITypeBinding leftOperandType= getLeftOperandType(infixExpression, node);
		return isIntegralDivision(infixExpression) // safety check
				&& isAssignmentCompatible(leftOperandType, node.getExpression().resolveTypeBinding())
				&& compareTo(node.resolveTypeBinding(), leftOperandType) >= 0;
	}

	private boolean isIntegralDivision(final InfixExpression infixExpression) {
		return isIntegralType(infixExpression) && ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.DIVIDE);
	}

	private boolean isAssignmentCompatibleInInfixExpression(final CastExpression node, final InfixExpression infixExpression) {
		ITypeBinding leftOpType= getLeftOperandType(infixExpression, node);
		return isAssignmentCompatible(leftOpType, node.getExpression().resolveTypeBinding())
				&& isAssignmentCompatible(leftOpType, node.resolveTypeBinding());
	}

	private ITypeBinding getLeftOperandType(final InfixExpression infixExpression, final CastExpression node) {
		List<Expression> operands= ASTNodes.allOperands(infixExpression);
		List<Expression> previousOperands= operands.subList(0, operands.indexOf(node));
		if (isAnyRefactored(previousOperands)) {
			return null;
		}

		return getTypeBinding(previousOperands);
	}

	private ITypeBinding getTypeBinding(final List<Expression> previousOperands) {
		Iterator<Expression> it= previousOperands.iterator();
		ITypeBinding maxTypeBinding= it.next().resolveTypeBinding();
		while (it.hasNext()) {
			ITypeBinding typeBinding= it.next().resolveTypeBinding();
			if (compareTo(maxTypeBinding, typeBinding) < 0) {
				maxTypeBinding= typeBinding;
			}
		}

		return maxTypeBinding;
	}

	private int compareTo(final ITypeBinding binding1, final ITypeBinding binding2) {
		int rank1= toPseudoEnum(binding1.getQualifiedName());
		int rank2= toPseudoEnum(binding2.getQualifiedName());
		return rank1 - rank2;
	}

	private int toPseudoEnum(final String name) {
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
	private boolean isNotRefactored(final Expression leftOperand) {
		return preVisit2(leftOperand);
	}

	private boolean isIntegralDividedByFloatingPoint(final CastExpression node, final InfixExpression infixExpression) {
		return node.equals(infixExpression.getRightOperand())
				&& isIntegralType(infixExpression.getLeftOperand())
				&& ASTNodes.hasOperator(infixExpression, InfixExpression.Operator.DIVIDE)
				&& isFloatingPointType(node);
	}

	private boolean isIntegralType(final Expression expression) {
		return ASTNodes.hasType(expression, byte.class.getSimpleName(), char.class.getSimpleName(), short.class.getSimpleName(), int.class.getSimpleName(), long.class.getSimpleName());
	}

	private boolean isFloatingPointType(final Expression expression) {
		return ASTNodes.hasType(expression, float.class.getSimpleName(), double.class.getSimpleName());
	}

	/** See JLS, section 5.2 Assignment Conversion */
	private boolean isConstantExpressionAssignmentConversion(final CastExpression node) {
		Long value= ASTNodes.getIntegerLiteral(node.getExpression());

		if (value != null) {
			long val= value;
			return (ASTNodes.hasType(node, byte.class.getSimpleName()) && Byte.MIN_VALUE <= val && val <= Byte.MAX_VALUE)
					|| (ASTNodes.hasType(node, short.class.getSimpleName()) && Short.MIN_VALUE <= val && val <= Short.MAX_VALUE)
					|| (ASTNodes.hasType(node, char.class.getSimpleName()) && 0 <= val && val <= 65535);
		}

		return false;
	}

	private boolean isStringConcat(final InfixExpression infixExpression) {
		return ASTNodes.hasType(infixExpression, String.class.getCanonicalName());
	}

	private boolean isPrimitiveTypeNarrowing(final CastExpression node) {
		ITypeBinding castTypeBinding= node.getType().resolveBinding();
		ITypeBinding exprTypeBinding= node.getExpression().resolveTypeBinding();
		return ASTNodes.isPrimitive(castTypeBinding) && ASTNodes.isPrimitive(exprTypeBinding)
				&& isAssignmentCompatible(exprTypeBinding, castTypeBinding);
	}

	private boolean isAssignmentCompatible(final Expression expression, final ITypeBinding typeBinding) {
		return expression != null && typeBinding != null && isAssignmentCompatible(typeBinding, expression.resolveTypeBinding());
	}

	private boolean isAssignmentCompatible(final Expression expression1, final Expression expression2) {
		return expression1 != null && expression2 != null
				&& isAssignmentCompatible(expression1.resolveTypeBinding(), expression2.resolveTypeBinding());
	}

	private boolean isAssignmentCompatible(final ITypeBinding sourceBinding, final ITypeBinding targetBinding) {
		return sourceBinding != null
				&& targetBinding != null
				&& targetBinding.isAssignmentCompatible(sourceBinding);
	}
}
